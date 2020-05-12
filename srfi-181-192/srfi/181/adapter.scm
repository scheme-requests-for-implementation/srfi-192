;;;
;;; Primitive operations
;;;
;;; MIT License.  See COPYING
;;;

(define (cp:input-port? obj)
  (or (custom-input-port? obj)
      (input-port? obj)))

(define (cp:output-port? obj)
  (or (custom-output-port? obj)
      (output-port? obj)))

(define (cp:textual-port? obj)
  (or (custom-port-textual? obj)
      (textual-port? obj)))

(define (cp:binary-port? obj)
  (or (custom-port-binary? obj)
      (binary-port? obj)))

(define (cp:port? obj)
  (or (custom-port? obj)
      (port? obj)))

(define (cp:close-port obj)
  (if (custom-port? obj)
    (custom-port-close obj)
    (close-port obj)))

(define (cp:close-input-port obj)
  (cond ((custom-input-port? obj) (custom-port-close obj))
        ((input-port? obj) (close-port obj))
        (else (error "input port expected, got" obj))))

(define (cp:close-output-port obj)
  (cond ((custom-output-port? obj) (custom-port-close obj))
        ((output-port? obj) (close-port obj))
        (else (error "output port expected, got" obj))))

(define (cp:flush-output-port obj)
  (cond ((custom-output-port? obj) (custom-port-flush obj))
        ((output-port? obj) (flush-output-port obj))
        (else (error "output port expected, got" obj))))

(define cp:read-char
  (case-lambda
    (() (cp:read-char (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-read-char port))
                  ((input-port? port) (read-char port))
                  (else (error "input port expected, got" port))))))

(define cp:peek-char
  (case-lambda
    (() (cp:peek-char (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-peek-char port))
                  ((input-port? port) (peek-char port))
                  (else (error "input port expected, got" port))))))

(define cp:read-u8
  (case-lambda
    (() (cp:read-u8 (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-read-u8 port))
                  ((input-port? port) (read-u8 port))
                  (else (error "input port expected, got" port))))))

(define cp:peek-u8
  (case-lambda
    (() (cp:peek-u8 (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-peek-u8 port))
                  ((input-port? port) (peek-u8 port))
                  (else (error "input port expected, got" port))))))

(define cp:write-char
  (case-lambda
    ((char) (cp:write-char char (current-output-port)))
    ((char port)
     (cond ((custom-output-port? port) (custom-port-write-char char port))
           ((output-port? port) (write-char char port))
           (else (error "output port expected, got" port))))))

(define cp:write-u8
  (case-lambda
    ((u8) (cp:write-u8 u8 (current-output-port)))
    ((u8 port)
     (cond ((custom-output-port? port) (custom-port-write-u8 u8 port))
           ((output-port? port) (write-u8 u8 port))
           (else (error "output port expected, got" port))))))

;; These two are debatable.
(define cp:char-ready?
  (case-lambda
    (() (cp:char-ready? (current-input-port)))
    ((port)
     (cond ((custom-input-port? port) #t)
           ((input-port? port) (char-ready? port))
           (else (error "input port expected, got" port))))))

(define cp:u8-ready?
  (case-lambda
    (() (cp:u8-ready? (current-input-port)))
    ((port)
     (cond ((custom-input-port? port) #t)
           ((input-port? port) (u8-ready? port))
           (else (error "input port expected, got" port))))))

;;;
;;; Derived operations
;;;

(define cp:read-line
  (case-lambda
    (() (cp:read-line (current-input-port)))
    ((port)
     (let ((out (open-output-string)))
       (let loop ()
         (let ((c (cp:read-char port)))
           (cond ((or (eof-object? c) (eqv? c #\newline))
                  (get-output-string out))
                 ((eqv? c #\return)
                  (let ((c2 (cp:peek-char port)))
                    (when (eqv? c2 #\newline)
                      (cp:read-char port))
                    (get-output-string out)))
                 (else (loop)))))))))

(define cp:read-string
  (case-lambda
    ((k) (cp:read-string k (current-input-port)))
    ((k port)
     (let ((out (open-output-string)))
       (let loop ((k k))
         (if (zero? k)
           (get-output-string out)
           (let ((c (cp:read-char port)))
             (if (eof-object? c)
               (get-output-string out)
               (begin
                 (write-char c out)
                 (loop (- k 1)))))))))))

(define cp:read-bytevector
  (case-lambda
    ((k) (cp:read-bytevector k (current-input-port)))
    ((k port)
     (let ((out (open-output-bytevector)))
       (let loop ((k k))
         (if (zero? k)
           (get-output-bytevector out)
           (let ((b (cp:read-u8 port)))
             (if (eof-object? b)
               (get-output-bytevector out)
               (begin
                 (write-u8 b out)
                 (loop (- k 1)))))))))))

(define cp:read-bytevector!
  (case-lambda
    ((bv) (cp:read-bytevector! bv (current-input-port)))
    ((bv p) (cp:read-bytevector! bv (current-input-port) 0))
    ((bv p s) (cp:read-bytevector! bv (current-input-port) s
                                   (bytevector-length bv)))
    ((bv p s e)
     (let loop ((k s) (cnt 0))
       (if (>= k e)
         cnt
         (let ((b (cp:read-u8 p)))
           (if (eof-object? b)
             cnt
             (begin
               (bytevector-u8-set! bv k b)
               (loop (+ k 1) (+ cnt 1))))))))))

(define cp:write-string
  (case-lambda
    ((str) (cp:write-string str (current-output-port)))
    ((str p) (cp:write-string str p 0))
    ((str p s) (cp:write-string str p s (string-length str)))
    ((str p s e)
     (let ((src (open-input-string (substring str s e))))
       (let loop ((c (read-char src)))
         (unless (eof-object? c)
           (cp:write-char c p)
           (loop (read-char src))))))))

(define cp:write-bytevector
  (case-lambda
    ((bv) (cp:write-bytevector bv (current-output-port)))
    ((bv p) (cp:write-bytevector bv p 0))
    ((bv p s) (cp:write-bytevector bv p s (bytevector-length bv)))
    ((bv p s e)
     (let loop ((k s))
       (when (< k e)
         (cp:write-u8 (bytevector-u8-ref bv k) p)
         (loop (+ k 1)))))))

;; wrap (scheme write) - we take an easy but inefficient path

(define (%write-wrapper writer obj p)
  (if (not (custom-port? p))
    (writer obj p)
    (let ((out (open-output-string)))
      (writer obj out)
      (cp:write-string (get-output-string out) p))))

(define cp:write
  (case-lambda
    ((obj) (%write-wrapper write obj (current-output-port)))
    ((obj p) (%write-wrapper write obj p))))

(define cp:write-shared
  (case-lambda
    ((obj) (%write-wrapper write-shared obj (current-output-port)))
    ((obj p) (%write-wrapper write-shared obj p))))

(define cp:write-simple
  (case-lambda
    ((obj) (%write-wrapper write-simple obj (current-output-port)))
    ((obj p) (%write-wrapper write-simple obj p))))

(define cp:display
  (case-lambda
    ((obj) (%write-wrapper display obj (current-output-port)))
    ((obj p) (%write-wrapper display obj p))))

;; wrap (scheme read)
;; we read characters until we get complete sexpt, then use the native
;; read to parse.  It's easier than reproduce the read functionality.
;; (but we miss implementation-specific extensions).
(define cp:read
  (case-lambda
    (() (cp:read (current-input-port)))
    ((p) (if (not (custom-port? p))
           (read p)
           (read (open-input-string (%fetch-sexpr p)))))))

;; Fetch a complete sexpr into a string.  If input has invalid or premature
;; sexpr, we just return it as is, and let the native read handle the error.
;; This code is lifted from Gauche's complete-sexp?.
(define (%fetch-sexpr p)
  (define buf (open-output-string))

  ;; main loop
  (define (rec closer)
    (let ((ch (deposit (cp:read-char p))))
      (cond ((eof-object? ch))
            ((eqv? closer ch))
            ((eqv? #\( ch) (rec #\) ) (when closer (rec closer)))
            ((eqv? #\[ ch) (rec #\] ) (when closer (rec closer)))
            ((eqv? #\{ ch) (rec #\} ) (when closer (rec closer)))
            ((eqv? #\" ch) (rec-escaped #\") (when closer (rec closer)))
            ((eqv? #\| ch) (rec-escaped #\|) (when closer (rec closer)))
            ((eqv? #\; ch) (skip-to-nl) (rec closer))
            ((eqv? #\# ch)
             (let ((c2 (deposit (cp:read-char p))))
               (cond ((eof-object? c2))
                     ((eqv? c2 #\\)
                      (let ((c3 (deposit (cp:read-char p))))
                        (unless (eof-object? c3)
                          (skip-token)
                          (when closer (rec closer)))))
                     ((eqv? c2 #\,) (rec closer)) ; srfi-10
                     ((eqv? c2 #\() (rec #\)) (when closer (rec closer))) ;vector
                     (else (rec closer)))))
            (else (rec closer)))))

  (define (rec-escaped closer)
    (let ((ch (deposit (cp:read-char p))))
      (cond ((eof-object? ch))
            ((eqv? closer ch))
            ((eqv? #\\ ch) (deposit (cp:read-char p)) (rec-escaped closer))
            (else (rec-escaped closer)))))

  (define (skip-token)
    (let loop ((ch (cp:peek-char p)))
      (unless (or (eof-object? ch)
                  (token-delimiter? ch))
        (deposit (cp:read-char p))
        (loop (cp:peek-char p)))))

  (define (token-delimiter? ch)
    (or (char-whitespace? ch)
        (memv ch '(#\" #\' #\( #\) #\, #\; #\[ #\\ #\] #\` #\{ #\| #\} #\x7f))))

  (define (deposit ch) (when (char? ch) (write-char ch buf)) ch)

  (define (skip-ws)
    (let loop ((ch (cp:peek-char p)))
      (unless (or (eof-object? ch)
                  (not (char-whitespace? ch)))
        (deposit (cp:read-char p))
        (loop (cp:peek-char p)))))

  (define (skip-to-nl)
    (let loop ((ch (deposit (cp:read-char p))))
      (unless (or (eof-object? ch)
                  (eqv? ch #\newline))
        (loop (deposit (cp:read-char p))))))

  (rec #f)
  (get-output-string buf))
