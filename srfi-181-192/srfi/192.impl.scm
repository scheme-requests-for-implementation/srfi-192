;;
;; Generic version of srfi-192
;; There's no portable way to implement srfi-192 on native ports, so we
;; assume the feature is only available for our custom ports.
;;

(define (port-has-port-position? port)
  (and (custom-port? port)
       (custom-port-has-port-position? port)))

(define (port-position port)
  (custom-port-position port))

(define (port-has-set-port-position!? port)
  (and (custom-port? port)
       (custom-port-has-set-port-position!? port)))

(define (set-port-position! port pos)
  (custom-port-set-port-position! port pos))
