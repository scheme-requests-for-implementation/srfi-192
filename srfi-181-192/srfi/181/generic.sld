;;;
;;; Generic implementation of srfi-181 and srfi-192
;;;

(define-library (srfi 181 generic)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 145))                   ; assume
  (export custom-port?
          custom-input-port?
          custom-output-port?
          custom-port-binary?
          custom-port-textual?
          custom-port-has-port-position?
          custom-port-position
          custom-port-has-set-port-position!?
          custom-port-set-port-position!
          custom-port-read-u8
          custom-port-peek-u8
          custom-port-read-char
          custom-port-peek-char
          custom-port-write-u8
          custom-port-write-char
          custom-port-close
          custom-port-flush

          make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port)
  (include "generic.scm"))
