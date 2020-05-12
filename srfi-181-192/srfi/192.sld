
(define-library (srfi 192)
  (export port-has-port-position?
          port-position
          port-has-set-port-position!?
          set-port-position!)
  (cond-expand
   (gauche
    (import (scheme base)
            (gauche base))
    (include "192.gauche.scm"))
   (else
    (import (scheme base)
            (srfi 181 generic))
    (include "192.impl.scm"))
   ))

