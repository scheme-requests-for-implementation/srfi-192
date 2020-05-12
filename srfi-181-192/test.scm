;; -*- coding:utf-8 -*-

(cond-expand
 (gauche (import (scheme base)
                 (scheme write)
                 (scheme read)
                 (gauche base)
                 (srfi 1)
                 (srfi 13)
                 (srfi 64)))
 (chibi  (import (except (scheme base)
                         input-port? output-port? textual-port? binary-port?
                         port? close-port close-input-port close-output-port
                         read-char peek-char read-line char-ready?
                         read-string read-u8 peek-u8 u8-ready?
                         read-bytevector read-bytevector!
                         write-char write-string write-u8
                         write-bytevector flush-output-port)
                 (srfi 1)
                 (srfi 130)
                 (except (chibi test) test-equal)
                 (rename (chibi test) (test-equal chibi:test-equal))
                 (srfi 181 adapter)))
 )

(import (srfi 181))
(import (srfi 192))

(cond-expand
 (chibi
  ;; adapt srfi-64 macros to (chibi test)
  (define-syntax test-equal
    (syntax-rules ()
      ((_ expect expr) (chibi:test-equal equal? expect expr))
      ((_ name expect expr) (chibi:test-equal equal? name expect expr))))
  (define-syntax test-eqv
    (syntax-rules ()
      ((_ expect expr) (chibi:test-equal eqv? expect expr))
      ((_ name expect expr) (chibi:test-equal eqv? name expect expr)))))
 (else))

(test-begin "srfi-181-192-test")

(test-group
 "Binary input, no port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define pos 0)
 (define closed #f)
 (define p (make-custom-binary-input-port
            "binary-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t)))) ; close
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "not has position?" (not (port-has-port-position? p)))
 (test-assert "not set position?" (not (port-has-set-port-position!? p)))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (peek-u8 p))
 (test-eqv 2 (read-u8 p))

 (test-equal (bytevector-copy data 3)
             (read-bytevector 997 p))
 (test-equal (eof-object) (read-u8 p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Binary input, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define p (make-custom-binary-input-port
            "binary-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t)) ;close
            ))
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (peek-u8 p))
 (set! saved-pos (port-position p))
 (test-eqv 2 (read-u8 p))

 (test-equal (bytevector-copy data 3)
             (read-bytevector 997 p))
 (test-equal (eof-object) (read-u8 p))

 (set-port-position! p saved-pos)
 (test-eqv 2 (read-u8 p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual input, no port positioning"
 (define data (string-tabulate (lambda (i)
                                 (integer->char
                                  (cond-expand
                                   (full-unicode (+ #x3000 i))
                                   (else (modulo i 256)))))
                               1000))
 (define pos 0)
 (define closed #f)
 (define p (make-custom-textual-input-port
            "textual-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (string-length data) pos))))
                (unless (zero? size)
                  (if (string? buf)
                    (string-copy! buf start data pos (+ pos size))
                    (do ((i 0 (+ i 1))
                         (j pos (+ j 1)))
                        ((= i size) (set! pos j))
                      (vector-set! buf (+ start i) (string-ref data j)))))
                size))
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t)); close
            ))
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "not has position?" (not (port-has-port-position? p)))
 (test-assert "not set position?" (not (port-has-set-port-position!? p)))

 (test-eqv (string-ref data 0) (read-char p))
 (test-eqv (string-ref data 1) (read-char p))
 (test-eqv (string-ref data 2) (peek-char p))
 (test-eqv (string-ref data 2) (read-char p))

 (test-equal (string-copy data 3)
             (read-string 997 p))
 (test-equal (eof-object) (read-char p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual input, port positioning"
 (define data (string-tabulate (lambda (i)
                                 (integer->char
                                  (cond-expand
                                   (full-unicode (+ #x3000 i))
                                   (else (modulo i 256)))))
                               1000))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define p (make-custom-textual-input-port
            "textual-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (string-length data) pos))))
                (unless (zero? size)
                  (if (string? buf)
                    (string-copy! buf start data pos (+ pos size))
                    (do ((i 0 (+ i 1))
                         (j pos (+ j 1)))
                        ((= i size) (set! pos j))
                      (vector-set! buf (+ start i) (string-ref data j)))))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t)); close
            ))
 (test-assert (port? p))
 (test-assert (input-port? p))
 (test-assert (not (output-port? p)))
 (test-assert (port-has-port-position? p))
 (test-assert (port-has-set-port-position!? p))

 (test-eqv (string-ref data 0) (read-char p))
 (test-eqv (string-ref data 1) (read-char p))
 (test-eqv (string-ref data 2) (peek-char p))
 (set! saved-pos (port-position p))
 (test-eqv (string-ref data 2) (read-char p))
 (test-eqv (string-ref data 3) (peek-char p))

 (test-equal (string-copy data 3)
             (read-string 997 p))
 (test-equal (eof-object) (read-char p))

 (set-port-position! p saved-pos)
 (test-eqv (string-ref data 2) (peek-char p))

 (test-assert (begin (close-port p)
                     closed))
 )

(test-group
 "Binary output, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define sink (make-vector 2000 #f))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define flushed #f)
 (define p (make-custom-binary-output-port
            "binary-output"
            (lambda (buf start count)   ;write!
              (do ((i start (+ i 1))
                   (j pos (+ j 1)))
                  ((>= i (+ start count)) (set! pos j))
                (vector-set! sink j (bytevector-u8-ref buf i)))
              count)
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "not input-port?" (not (input-port? p)))
 (test-assert "output port?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (write-u8 3 p)
 (write-u8 1 p)
 (write-u8 4 p)
 (flush-output-port p)
 (test-assert "flush" flushed)
 (set! saved-pos (port-position p))

 (test-equal '#(3 1 4)
             (vector-copy sink 0 pos))
 (write-bytevector '#u8(1 5 9 2 6) p)
 (flush-output-port p)
 (test-equal '#(3 1 4 1 5 9 2 6)
             (vector-copy sink 0 pos))

 (set-port-position! p saved-pos)
 (for-each (lambda (b) (write-u8 b p)) '(5 3 5))
 (flush-output-port p)
 (test-equal '#(3 1 4 5 3 5)
             (vector-copy sink 0 pos))
 (test-equal '#(3 1 4 5 3 5 2 6)
             (vector-copy sink 0 (+ pos 2)))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual output, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define sink (make-vector 2000 #f))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define flushed #f)
 (define p (make-custom-textual-output-port
            "textual-output"
            (lambda (buf start count)   ;write!
              (do ((i start (+ i 1))
                   (j pos (+ j 1)))
                  ((>= i (+ start count)) (set! pos j))
                (vector-set! sink j
                             (if (string? buf)
                               (string-ref buf i)
                               (vector-ref buf i))))
              count)
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "not input?" (not (input-port? p)))
 (test-assert "output?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (write-char #\a p)
 (write-char #\b p)
 (write-char #\c p)
 (flush-output-port p)
 (test-assert "flush" flushed)
 (set! saved-pos (port-position p))

 (test-equal '#(#\a #\b #\c)
             (vector-copy sink 0 pos))
 (write-string "Quack" p)
 (flush-output-port p)
 (test-equal '#(#\a #\b #\c #\Q #\u #\a #\c #\k)
             (vector-copy sink 0 pos))

 (set-port-position! p saved-pos)
 (write-string "Cli" p)
 (flush-output-port p)
 (test-equal '#(#\a #\b #\c #\C #\l #\i)
             (vector-copy sink 0 pos))
 (test-equal '#(#\a #\b #\c #\C #\l #\i #\c #\k)
             (vector-copy sink 0 (+ pos 2)))

 (test-assert (begin (close-port p)
                     closed))
 )

;; NB: Gauche doesn't support input/output port yet.
(cond-expand
 (gauche)
 (else
(test-group
 "binary input/output"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define original-size 500)             ;writing may extend the size
 (define pos 0)
 (define saved-pos #f)
 (define flushed #f)
 (define closed #f)
 (define p (make-custom-binary-input/output-port
            "binary i/o"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- original-size pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda (buf start count)   ;write!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! data pos buf start (+ start size))
                (set! pos (+ pos size))
                (set! original-size (max original-size pos))
                size))
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "input?" (input-port? p))
 (test-assert "output?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (read-u8 p))
 (set! saved-pos (port-position p))
 (test-equal "rest of input"
             (bytevector-copy data 3 original-size)
             (read-bytevector 1000 p))
 (write-bytevector '#u8(255 255 255) p)
 (test-equal "appended"
             '#u8(255 255 255)
             (bytevector-copy data 500 503))
 (write-u8 254 p)
 (test-equal "appended more"
             '#u8(255 255 255 254)
             (bytevector-copy data 500 504))
 (write-u8 254 p)
 (test-eqv "still eof" (eof-object) (read-u8 p))

 (set-port-position! p saved-pos)
 (test-eqv "rewind & peek" 3 (peek-u8 p))
 (write-u8 100 p)
 (set-port-position! p saved-pos)
 (test-eqv "overwritten" 100 (read-u8 p))
 (test-eqv "overwritten" 4 (read-u8 p))
 )

(test-group
 "textual input/output"
 (define data (apply vector
                     (list-tabulate 1000 (lambda (i)
                                           (integer->char
                                            (+ 256 (modulo i 256)))))))
 (define original-size 500)             ;writing may extend the size
 (define pos 0)
 (define saved-pos #f)
 (define flushed #f)
 (define closed #f)
 (define p (make-custom-textual-input/output-port
            "textual i/o"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- original-size pos))))
                (vector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda (buf start count)   ;write!
              (let ((size (min count (- (vector-length data) pos))))
                (vector-copy! data pos buf start (+ start size))
                (set! pos (+ pos size))
                (set! original-size (max original-size pos))
                size))
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "input?" (input-port? p))
 (test-assert "output?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (test-eqv (vector-ref data 0) (read-char p))
 (test-eqv (vector-ref data 1) (read-char p))
 (test-eqv (vector-ref data 2) (read-char p))
 (set! saved-pos (port-position p))
 (test-equal "rest of input"
             (vector-copy data 3 original-size)
             (string->vector (read-string 1000 p)))
 (write-string "abc" p)
 (test-equal "appended" '#(#\a #\b #\c)
             (vector-copy data 500 503))
 (write-char #\d p)
 (test-equal "appended more" '#(#\a #\b #\c #\d)
             (vector-copy data 500 504))
 (test-eqv "still eof" (eof-object) (read-char p))

              (display ">> " (current-error-port))
              (write saved-pos (current-error-port))
              (newline (current-error-port))
 (set-port-position! p saved-pos)
 (test-eqv "rewind & peek" #\ă (peek-char p))
 (write-char #\Z p)
 (set-port-position! p saved-pos)
 (test-eqv "overwritten" #\Z (read-char p))
 )

)) ; cond expand

(test-group
 "high-level i/o"
 (define source
   (let ((orig "((a b c . d)
                  (a . (b . (c . (d . ()))))
                  #(a b c d)
                  (#\\a #\\b #\\c #\\d #\\x07 #\\) #\\#)
                  \"abcd\"
                  #t #f #true #false
                  |a\\x07;bcd|
                  ;;comment
                  #u8(1 2 3 4 5)
                  #0=(a) #0#
                  1 1.111 3e5 #xff #b-0101 -5/3+3.9i +inf.0 +nan.0
                  )"))
     (string-append orig "\n" orig "\n;end of input")))
 (define data (read (open-input-string source)))
 (define source-pos 0)
 (define sink (open-output-string))
 (define inp (make-custom-textual-input-port
              "textual-input"
              (lambda (buf start count) ;read!
                (let ((size (min count (- (string-length source) source-pos))))
                  (unless (zero? size)
                    (if (string? buf)
                      (string-copy! buf start source
                                    source-pos (+ source-pos count))
                      (do ((i 0 (+ i 1))
                           (j source-pos (+ j 1)))
                          ((= i size) (set! source-pos j))
                        (vector-set! buf (+ start i) (string-ref source j)))))
                  size))
              #f #f #f))
 (define outp (make-custom-textual-output-port
               "textual-output"
               (lambda (buf start count)   ;write!
                 (do ((i start (+ i 1)))
                     ((>= i (+ start count)))
                   (write-char (if (string? buf)
                                 (string-ref buf i)
                                 (vector-ref buf i))
                               sink))
                 count)
               #f #f #f))

 (let ((expr1 (read inp)))
   (test-equal "read first expr" data expr1)
   (write expr1 outp))
 (let ((expr2 (read inp)))
   (test-equal "read second expr" data expr2)
   (write expr2 outp))
 (test-assert "finishing" (eof-object? (read inp)))
 (let ((p (open-input-string (get-output-string sink))))
   (test-equal "written expr 1" data (read p))
   (test-equal "written expr 2" data (read p)))
 )

(test-end  "srfi-181-192-test")
