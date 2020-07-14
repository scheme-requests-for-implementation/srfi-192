;;
;; Included from 192.sld
;;

(define (port-has-port-position? port)
  (assume (port? port))
  (boolean (port-tell port)))

(define (port-position port)
  (assume (port? port))
  (port-tell port))

(define (port-has-set-port-position!? port)
  (assume (port? port))
  ;; NB: We should have the way to specifically query the port is seekable.
  (boolean (port-tell port)))

(define (set-port-position! port pos)
  (assume (port? port))
  (port-seek port pos))


;; It'd be natural for this condition to inherit <port-error>, but
;; the public constructor doens't take the port.  So we implement it
;; as a mixin condition.  The recommended use is to make a compound
;; condition with a <port-error>.
(define-condition-type <i/o-invalid-position-error-mixin> <condition>
  i/o-invalid-position-error?
  (position))

(define (make-i/o-invalid-position-error position)
  (make <i/o-invalid-position-error-mixin> :position position))
