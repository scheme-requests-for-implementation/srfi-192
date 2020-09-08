(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 64)
        (srfi 192)
        (vector-ports))

(test-begin "vector-ports-test")

(test-group 
 "input binary vector"
 (let ((p (open-input-binary-vector '#(1 2 3 4 65535)))
       (saved-pos #f))
   (test-assert (input-port? p))
   (test-assert (port-has-port-position? p))
   (test-assert (port-has-set-port-position!? p))

   (test-eqv "read 1" 1 (read-u8 p))
   (test-eqv "read 2" 2 (read-u8 p))
   (test-eqv "peek 3" 3 (peek-u8 p))

   (set! saved-pos (port-position p))

   (test-eqv "read 3" 3 (read-u8 p))
   (test-eqv "read 4" 4 (read-u8 p))
   (test-eqv "read 255" 255 (read-u8 p))
   (test-assert (eof-object? (read-u8 p)))

   (set-port-position! p saved-pos)
   (test-eqv 3 (read-u8 p))

   (close-port p)
   (test-error (read-u8 p))
   ))

(test-group
 "input textual vector"
 (let ((p (open-input-textual-vector '#(#\a #\space #\b #\c #\z)))
       (saved-pos #f))
   (test-assert (input-port? p))
   (test-assert (port-has-port-position? p))
   (test-assert (port-has-set-port-position!? p))

   (test-eqv "read #\\a" #\a (read-char p))
   (test-eqv "read space" #\space (read-char p))
   (test-eqv "peek #\\b" #\b (peek-char p))

   (set! saved-pos (port-position p))

   (test-eq "read bcz" 'bcz (read p))
   (test-assert (eof-object? (read-u8 p)))

   (set-port-position! p saved-pos)
   (test-eqv #\b (read-char p))

   (close-port p)
   (test-error (read-char p))
   ))

(test-group
 "output binary vector"
 ;; We make the initial capacity very small to test storage extension
 (let ((p (open-output-binary-vector 2))
       (saved-pos #f))
   (test-assert (output-port? p))
   (test-assert (port-has-port-position? p))
   (test-assert (port-has-set-port-position!? p))

   (write-u8 1 p)
   (write-u8 2 p)
   (write-u8 3 p)

   (set! saved-pos (port-position p))

   (write-u8 4 p)
   (write-u8 5 p)
   (write-u8 6 p)

   (test-equal "output binary vector"
               '#(1 2 3 4 5 6) (get-output-vector p))
   
   (set-port-position! p saved-pos)

   (write-u8 99 p)
   (test-equal "output binary vector (overwrite)"
               '#(1 2 3 99 5 6) (get-output-vector p))

   (write-bytevector #u8(10 11 12) p)

   (test-equal "output binary vector (more)"
               '#(1 2 3 99 10 11 12) (get-output-vector p))

   (close-port p)
   (test-error (write-u8 7 p))
   (test-error (get-output-vector p))))

(test-group
 "output textual vector"
 (let ((p (open-output-textual-vector 2))
       (saved-pos #f))
   (test-assert (output-port? p))
   (test-assert (port-has-port-position? p))
   (test-assert (port-has-set-port-position!? p))

   (write-char #\a p)
   (write-string "bcd" p)

   (set! saved-pos (port-position p))

   (write '(efg) p)
   (write-char #\h p)

   (test-equal "output binary vector"
               '#(#\a #\b #\c #\d #\( #\e #\f #\g #\) #\h)
               (get-output-vector p))
   
   (set-port-position! p saved-pos)

   (write-char #\Z p)
   (test-equal "output binary vector (overwrite)"
               '#(#\a #\b #\c #\d #\Z #\e #\f #\g #\) #\h)
               (get-output-vector p))

   (write-string "abracadabra" p)

   (test-equal "output binary vector (more)"
               '#(#\a #\b #\c #\d #\Z #\a #\b #\r #\a #\c #\a #\d #\a #\b #\r #\a)
               (get-output-vector p))

   (close-port p)
   (test-error (write-char #\Z p))
   (test-error (get-output-vector p))))

(test-end "vector-ports-test")




