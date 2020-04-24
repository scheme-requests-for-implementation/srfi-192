(define-library (srfi 181)
  (export make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port)
  (cond-expand
   (gauche
    (import (scheme base)
            (gauche base)
            (gauche uvector)
            (gauche vport)
            (srfi 42))
    (include "181.gauche.scm"))
   (else
    ;; Actual implementation is provided in srfi/181/generic.scm
    (import (srfi 181 generic)))
   ))


;; Local variables:
;; mode: scheme
;; end:

