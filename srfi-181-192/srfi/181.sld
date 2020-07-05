(define-library (srfi 181)
  (export make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port

          make-codec latin-1-codec utf-8-codec utf-16-codec
          native-eol-style
          i/o-decoding-error? i/o-encoding-error?
          i/o-encoding-error-char
          make-transcoder native-transcoder
          transcoded-port
          bytevector->string string->bytevector)
          
  (cond-expand
   (gauche
    (import (scheme base)
            (gauche base)
            (gauche uvector)
            (gauche vport)
            (srfi 42)
            (srfi 181 transcoder))
    (include "181.gauche.scm"))
   (else
    ;; Actual implementation is provided in srfi/181/generic.scm
    (import (srfi 181 generic))
    (import (srfi 181 transcoder)))
   ))


;; Local variables:
;; mode: scheme
;; end:

