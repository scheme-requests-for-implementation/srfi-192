;;;
;;; Generic transcoded port
;;;
;;; This implementation assumes the internal encoding is ASCII-only.
;;; 
;;; MIT License.  See COPYING

;; supporting parts
(define-record-type i/o-decoding-error
  (make-i/o-decoding-error message)
  i/o-decoding-error?
  (message i/o-decoding-error-message))

(define-record-type i/o-encoding-error
  (make-i/o-encoding-error char message)
  i/o-encoding-error?
  (message i/o-decoding-error-message)
  (char i/o-encoding-error-char))

(define-record-type codec
  (%make-codec name)
  codec?
  (name codec-name)) ; canonical symbol

(define *supported-codecs*
  ;; ((canonical-symbol name ...) ...)
  '((ascii    "ascii" "us-ascii" "iso-ir-6"
              "ansi_x3.4-1968" "ansi_x3.4-1986" "iso-646.irv:1991"
              "iso-646-us" "us" "ibm367" "cp367" "csascii")
    (latin-1  "latin-1" "iso-8859-1:1987" "iso-8859-1" "iso-ir-100"
              "iso_8859-1" "latin1" "l1" "ibm819" "cp819" 
              "csisolatin1")
    (utf-8    "utf-8" "csutf8")
    (utf-16   "utf-16" "csutf16"))
  )

(define (make-codec name)
  ;; We support latin-1, utf-8 and utf-16
  (let ((codec-entry (find (lambda (entry)
                             (member name (cdr entry) string-ci=?))
                           *supported-codecs*)))
    (if codec-entry
      (%make-codec (car codec-entry))
      (error "Unknown or unsupported codec" name))))

(define *ascii-codec* (make-codec "ascii"))
(define *latin-1-codec* (make-codec "latin-1"))
(define *utf-8-codec* (make-codec "utf-8"))
(define *utf-16-codec* (make-codec "utf-16"))

(define (latin-1-codec) *latin-1-codec*)
(define (utf-8-codec) *utf-8-codec*)
(define (utf-16-codec) *utf-16-codec*)
(define (native-transcoder) *ascii-codec*)

(define-record-type transcoder
  (%make-transcoder codec eol-style handling-mode)
  transcoder?
  (codec transcoder-codec)
  (eol-style transcoder-eol-style)
  (handling-mode transcoder-handling-mode))

(define (make-transcoder codec eol-style handling-mode)
  (unless (codec? codec)
    (error "codec required, but got" codec))
  (unless (memq eol-style '(none lf crlf))
    (error "unsupported eol-style, must be one of (none lf crlf), but got"
           eol-style))
  (unless (memq handling-mode '(replace raise))
    (error "unsupported handling-mode, must be either replace or raise, but got"
           handling-mode))
  (%make-transcoder codec eol-style handling-mode))


(define (native-eol-style) 'lf)

;; actual transcoding

(define (make-filler read-1)
  (lambda (buf start count)
    (let loop ((i start)
               (k 0))
      (if (= k count)
        k
        (let ((c (read-1)))
          (if (eof-object? c)
            k
            (begin
              ((if (string? buf) string-set! vector-set!) buf i c)
              (loop (+ i 1) (+ k 1)))))))))  

;; read latin-1 or utf-8 source into ascii.
(define (make-u8-ascii-read! source eol-style handling)
  (define (read-1)
    (let ((b (read-u8 source)))
      (cond ((eof-object? b) b)
            ((>= b #x80) (if (eq? handling 'replace)
                           #\?
                           (raise (make-i/o-decoding-error
                                   "input byte out of range"))))
            ((= b #x0d) (if (eq? eol-style 'none)
                          #\return
                          (let ((b2 (peek-u8 source)))
                            (when (= b2 #x0a)
                              (read-u8 source))
                            #\newline)))
            (else (integer->char b)))))
  (make-filler read-1))

;; read utf-16 source into ascii
(define (make-u16-ascii-read! source eol-style handling)
  (define endianness #f)
  (define (read-1)
    (let ((b0 (read-u8 source)))
      (if (eof-object? b0)
        b0
        (let ((b1 (read-u8 source)))
          (if (eof-object? b1)
            (raise (make-i/o-decoding-error "lone utf-16 octet"))
            (case endianness
              ((big)    (decode-1 b0 b1))
              ((little) (decode-1 b1 b0))
              (else
               (cond ((and (= b0 #xfe) (= b1 #xff))
                      (set! endianness 'big)
                      (read-1))
                     ((and (= b0 #xff) (= b1 #xfe))
                      (set! endianness 'little)
                      (read-1))
                     (else (decode-1 b0 b1)) ;; BE by default
                     ))))))))
  (define (decode-1 hi lo)
    (cond ((and (zero? hi) (< lo #x80))
           (if (and (= lo #x0d) (not (eq? eol-style 'none)))
             (let ((b2 (peek-u8 source)))
               (when (= b2 #x0a)
                 (read-u8 source))
               #\newline)
             (integer->char lo)))
          ((eq? handling 'replace) #\?)
          (else (make-i/o-decoding-error "input bytes out of range"))))
  (make-filler read-1))

(define (make-flusher write-1)
  (lambda (buf start count)
    (let loop ((i start)
               (k 0))
      (if (= k count)
        k
        (let ((c ((if (string? buf) string-ref vector-ref) buf i)))
          (write-1 c)
          (loop (+ i 1) (+ k 1)))))))

;; Write a character C to SINK as a single byte.  Handlng EOL style.
;; Returns a new pending-return value.
(define (output-1b c sink eol-style pending-return)
  (define (nl)
    (case eol-style
      ((none lf) (write-u8 #x0a sink))
      ((cr)      (write-u8 #x0d sink))
      ((crlf)    (write-u8 #x0d sink) (write-u8 #x0a sink))))
  (cond ((eq? eol-style 'none)          ; we don't need to worry newlines
         (write-u8 (char->integer c) sink) #f)
        ((eqv? c #\return) (when pending-return (nl)) #t)
        ((eqv? c #\newline) (nl) #f)
        (pending-return (nl) (write-u8 (char->integer c) sink) #f)
        (else (write-u8 (char->integer c) sink) #f)))

(define (make-u8-ascii-write! sink eol-style handling)
  (define pending-return #f)
  (define (write-1 c)
    (set! pending-return (output-1b c sink eol-style pending-return)))
  (make-flusher write-1))

(define (make-u16-ascii-write! sink eol-style handling)
  ;; we assume BE
  (define bom-written? #f)
  (define pending-return #f)
  (define (write-1 c)
    (unless bom-written?
      (write-u8 #xfe sink)
      (write-u8 #xff sink)
      (set! bom-written? #t))
    (write-u8 0 sink)
    (set! pending-return (output-1b c sink eol-style pending-return)))
  (make-flusher write-1))

;; API
(define (transcoded-port inner transcoder)
  (define (closer) (close-port inner))
  (define (port-id)
    (string-append "transcoding "
                   (if (input-port? inner) "from " "to ")
                   (symbol->string (codec-name (transcoder-codec transcoder)))))
  (unless (transcoder? transcoder)
    (error "transcoder required, but got" transcoder))
  (let ((eol (transcoder-eol-style transcoder))
        (handling (transcoder-handling-mode transcoder)))
    (cond
     ((input-port? inner)
      (case (codec-name (transcoder-codec transcoder))
        ((latin-1 utf-8)
         (make-custom-textual-input-port (port-id)
                                         (make-u8-ascii-read! inner eol handling)
                                         #f #f closer))
        ((utf-16)
         (make-custom-textual-input-port (port-id)
                                         (make-u16-ascii-read! inner eol handling)
                                         #f #f closer))))
     ((output-port? inner)
      (case (codec-name (transcoder-codec transcoder))
        ((latin-1 utf-8)
         (make-custom-textual-output-port (port-id)
                                          (make-u8-ascii-write! inner eol handling)
                                          #f #f closer))
        ((utf-16)
         (make-custom-textual-output-port (port-id)
                                          (make-u16-ascii-write! inner eol handling)
                                          #f #f closer))))
     (else
      (error "port required, but got" inner)))))

(define (bytevector->string bytevector transcoder)
  (let ((src (transcoded-port (open-input-bytevector bytevector) transcoder))
        (sink (open-output-string)))
    (let loop ((c (read-char src)))
      (if (eof-object? c)
        (get-output-string sink)
        (begin
          (write-char c sink)
          (loop (read-char src)))))))

(define (string->bytevector string transcoder)
  (let* ((src (open-input-string string))
         (dest (open-output-bytevector))
         (sink (transcoded-port dest transcoder)))
    (let loop ((c (read-char src)))
      (if (eof-object? c)
        (get-output-bytevector dest)
        (begin
          (write-char c sink)
          (loop (read-char src)))))))
