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


