;;
;; Included from 181.sld
;;
;; MIT License.  See COPYING
;;

(define-constant *buffer-size* 1024)

(define (make-seeker get-position set-position!)
  (^[offset whence]
    (if (and (= offset 0) (eq? whence SEEK_CUR))
      (and get-position (get-position))
      (and set-position!
           (cond
            [(eqv? whence SEEK_SET) (set-position! offset)]
            [(eqv? whence SEEK_CUR)
             (and get-position
                  (set-position! (+ offset (get-position))))]
            [(eqv? whence SEEK_END) #f]            ; unsupportable
            )))))

(define (make-custom-binary-input-port id read!
                                       get-position set-position!
                                       close)
  (define (filler buf)
    (read! buf 0 (u8vector-length buf)))
  (make <buffered-input-port>
    :name id
    :fill filler
    :seek (make-seeker get-position set-position!)
    :close close))

(define (make-custom-textual-input-port id read!
                                        get-position set-position!
                                        close)
  (if (or get-position set-position!)
    ;; If positioning is required, we can't buffer characters.
    (let ([buf (make-vector 1)])
      (make <virtual-input-port>
        :getc (^[] (let1 n (read! buf 0 1)
                     (if (zero? n)
                       (eof-object)
                       (vector-ref buf 0))))
        :seek (make-seeker get-position set-position!)
        :close close))
    ;; <buffered-input-port> uses u8vector for the buffer, so we have
    ;; to convert it.
    (let ([cbuf #f])                     ;vector, allocated on demand
      (define (filler buf)
        (unless cbuf
          (set! cbuf (make-vector (quotient (u8vector-length buf) 4))))
        (let1 n (read! cbuf 0 (vector-length cbuf))
          (if (zero? n)
            n
            (let* ([s (vector->string cbuf 0 n)]
                   [size (string-size s)])
              (assume (<= size (u8vector-length buf)))
              (string->u8vector! buf 0 s)
              size))))
      (make <buffered-input-port>
        :name id
        :fill filler
        :close close))))

(define (make-custom-binary-output-port id write!
                                        get-position set-position!
                                        close :optional (flush #f))
  (define (flusher buf complete?)
    (if (not complete?)
      (write! buf 0 (u8vector-length buf))
      ;; this is a buffer-flush operation
      (let1 len (u8vector-length buf)
        (let loop ([pos 0])
          (if (= pos len)
            (begin
              (when flush (flush))
              len)
            (let1 n (write! buf pos (- len pos))
              (loop (+ pos n))))))))
  (make <buffered-output-port>
    :name id
    :flush flusher
    :seek (make-seeker get-position set-position!)
    :close close))

;; For textual output, <buffered-output-port> is inconvenient,
;; for the passed u8vector may end in the middle of multibyte character.
(define (make-custom-textual-output-port id write!
                                         get-position set-position!
                                         close :optional (flush #f))
  (define cbuf (make-vector 1))
  (make <virtual-output-port>
    :name id
    :putc (^c (vector-set! cbuf 0 c) (write! cbuf 0 1))
    :puts (^s (let1 siz (string-length s)
                (when (< (vector-length cbuf) siz)
                  (set! cbuf (make-vector siz)))
                (do-ec (: c (index i) s)
                       (vector-set! cbuf i c))
                (write! cbuf 0 siz)))
    :seek (make-seeker get-position set-position!)
    :flush (^[] (and flush (flush)))
    :close close))

(define (make-file-error . objs)
  ;; Up to 0.9.9, Gauche uses ad-hoc way to determine file-error--
  ;; that is, a <system-error> with certain errnos is a file error. 
  ;; It is impossible to translate arbitrary objs into meaningful 
  ;; <system-error>.  This is just a crude emulation.
  (condition
   (<system-error> (errno ENXIO) (message (write-to-string objs)))))
