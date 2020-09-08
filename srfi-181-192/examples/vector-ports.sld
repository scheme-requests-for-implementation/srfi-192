(define-library (vector-ports)
  (import (scheme base)
          (scheme bitwise)
          (scheme case-lambda)
          (srfi 181)
          (srfi 192))
  (export open-input-binary-vector
          open-input-textual-vector
          open-output-binary-vector
          open-output-textual-vector
          get-output-vector)
  (cond-expand
   ((library (scheme ephemeron))
    (import (scheme ephemeron)))
   (else))
  (include "vector-ports.scm"))
