;;;
;;; Generic implementation of srfi-181 transcoded ports
;;;

(define-library (srfi 181 transcoder)
  (import (except (scheme base)
                  input-port? output-port? textual-port? binary-port?
                  port? close-port close-input-port close-output-port
                  read-char peek-char read-line char-ready?
                  read-string read-u8 peek-u8 u8-ready?
                  read-bytevector read-bytevector!
                  write-char write-string write-u8
                  write-bytevector flush-output-port file-error?)
          (scheme char)
          (scheme list)
          (srfi 181 adapter)
          (srfi 181 generic)
          (srfi 145))                   ;assume
  (export make-codec latin-1-codec utf-8-codec utf-16-codec
          native-eol-style
          unknown-encoding-error? unknown-encoding-error-name
          i/o-decoding-error? i/o-encoding-error?
          i/o-encoding-error-char
          make-transcoder native-transcoder
          transcoded-port
          bytevector->string string->bytevector)
  (include "transcoder.scm"))
