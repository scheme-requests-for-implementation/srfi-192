;;;
;;; Examples to illustrate usage of custom ports
;;;

;; This library provides a positionable vector-backed ports.  The public
;; API is as follows.
;;
;;   open-input-binary-vector vector
;;  
;;     VECTOR must be a vector of exact integers.  Returns a binary input port
;;     that reads from VECTOR, one value at a time.  If an element of the
;;     vector exceeds u8 range, bitwise and with #xff is taken.
;;
;;   open-input-textual-vector vector
;;
;;     VECTOR must be a vector of characters  Returns a textual input port
;;     that reads from VECTOR, one value at a time.
;;
;;   open-output-binary-vector [initial-capacity]
;;   open-output-textual-vector [initial-capacity]
;;
;;     Returns a binary/textual output port that stores each output datum
;;     into a vector.  It allocates a buffer of length INITIAL-CAPACITY
;;     initially, and extend it as needed.  The output data can be
;;     retrieved as a vector by get-output-vector.
;;     It is positionable, but it isn't allowed to set-port-position! past
;;     the current output size.
;;
;;   get-output-vector port
;;
;;     If PORT is a port created by open-output-binary-vector or
;;     open-output-textual-vector, retrieves the output data so far as
;;     a vector.  The length of the result is the maximum position
;;     the data is written to the port.
;;

;;
;; input vector ports
;;

(define (make-input-vector-common vec copier)
  (define pos 0)
  (define closed? #f)
  (define (read! buf start count)
    (if closed?
      (error "port is already closed" port)
      (let* ((size (min count (- (vector-length vec) pos)))
             (end  (+ pos size)))
        (do ((i pos   (+ i 1))
             (j start (+ j 1)))
            ((= i end))
          (copier buf j vec i))
        (set! pos end)
        size)))
  (define (get-position) pos)
  (define (set-position! p) 
    (if (and (<= 0 p) (< p (vector-length vec)))
      (set! pos p)
      (raise (make-i/o-invalid-position-error p))))
  (define (close) (set! closed? #t))
  
  (list read! get-position set-position! close))

(define (open-input-binary-vector vec)
  (apply make-custom-binary-input-port 
         "input binary vector"
         (make-input-vector-common vec
                                   (lambda (buf j vec i)
                                     (bytevector-u8-set! buf j
                                                         (bitwise-and
                                                          (vector-ref vec i)
                                                          #xff))))))

(define (open-input-textual-vector vec)
  (apply make-custom-textual-input-port 
         "input textual vector"
         (make-input-vector-common vec
                                   (lambda (buf j vec i)
                                     ((if (string? buf)
                                        string-set!
                                        vector-set!)
                                      buf j (vector-ref vec i))))))


;; For output vector, we need a way to map a port to its internal
;; custom state.  We could keep the custom state in the id field, but
;; there's no way to extract that info portably.
;; So we keep a global table.  We use ephemeron when avialable, and 
;; normal assoc list otherwise.
;; The entry may be removed during traversal, so we keep a dummy entry
;; at the head of the table.
(define *output-vectors* (cons #f '()))

(cond-expand
 ((library (scheme ephemeronxx))
  (begin 
    (define weak-cons make-ephemeron)
    (define (weak-find key alis)
      (let loop ((lis (cdr alis)) (prev alis))
        (and (pair? lis)
             (let ((k (ephemeron-key (car lis)))
                   (d (ephemeron-datum (car lis))))
               (cond ((ephemeron-broken? (car lis))
                      ;; remove the entry from the list
                      (set-cdr! prev (cdr lis))
                      (loop (cdr lis) lis))
                     ((eq? key k) d)
                     (else (loop (cdr lis) lis)))))))
    (define (weak-delete! key alis)
      (let loop ((lis (cdr alis)) (prev alis))
        (and (pair? lis)
             (let ((k (ephemeron-key (car lis))))
               (cond ((or (ephemeron-broken? (car lis))
                          (eq? key k))
                      (set-cdr! prev (cdr lis))
                      (loop (cdr lis) prev))
                     (else (loop (cdr lis) lis)))))))))
      
 (else
  (begin
    (define weak-cons cons)
    (define (weak-find key alis)
      (let ((e (assq key (cdr alis))))
        (and e (cdr e))))
    (define (weak-delete! key alis)
      (let loop ((lis (cdr alis)) (prev alis))
        (and (pair? lis)
             (if (eq? key (car lis))
               (begin (set-cdr! prev (cdr lis))
                      (loop (cdr lis) prev))
               (loop (cdr lis) lis)))))))
 )

(define-record-type output-vector
  (%make-output-vector buffer capacity position maxpos)
  output-vector?
  (buffer   output-vector-buffer   output-vector-buffer-set!)
  (capacity output-vector-capacity output-vector-capacity-set!)
  (position output-vector-position output-vector-position-set!)
  (maxpos   output-vector-maxpos   output-vector-maxpos-set!))

(define (make-output-vector init-capacity)
  (%make-output-vector (make-vector init-capacity) init-capacity 0 0))

(define (extend-output-vector! ov)
  (let* ((new-cap (* (output-vector-capacity ov) 2))
         (new-buf (make-vector new-cap)))
    (vector-copy! new-buf 0 (output-vector-buffer ov))
    (output-vector-buffer-set! ov new-buf)
    (output-vector-capacity-set! ov new-cap)))

(define (register-output-vector! port ov)
  (set-cdr! *output-vectors* (cons (weak-cons port ov) 
                                   (cdr *output-vectors*))))

(define (unregister-output-vector! port)
  (weak-delete! port *output-vectors*))

(define (port->output-vector port)
  (weak-find port *output-vectors*))

;; The core of custom output extensible vector
;; Returns output-vector record and a list of args to pass to custom port ctor
(define (make-output-vector-common init-capacity copier)
  (define ov (make-output-vector init-capacity))
  (define closed? #f)

  (define (write! buf start count)
    (when closed? (error "port is already closed"))
    (do ((i start (+ i 1))
         (j (output-vector-position ov) (+ j 1)))
        ((>= i (+ start count)) (output-vector-position-set! ov j))
      (when (>= j (output-vector-capacity ov))
        (extend-output-vector! ov))
      (copier (output-vector-buffer ov) j buf i))
    (when (< (output-vector-maxpos ov) (output-vector-position ov))
      (output-vector-maxpos-set! ov (output-vector-position ov)))
    count)
  (define (get-position) (output-vector-position ov))
  (define (set-position! p)
    ;; We don't allow to seek past the current max position.  Another option
    ;; is to extend the vector.
    (if (and (<= 0 p) (< p (output-vector-maxpos ov)))
      (output-vector-position-set! ov p)
      (raise (make-i/o-invalid-position-error p))))
  (define (mark-closed!) (set! closed? #t))

  (values ov write! get-position set-position! mark-closed!))

(define open-output-binary-vector
  (case-lambda
    (() (make-output-binary-vector 256))
    ((init-capacity)
     (let-values (((ov write! get-position set-position! mark-closed!)
                   (make-output-vector-common 
                    init-capacity
                    (lambda (vec j buf i)
                      (vector-set! vec j (bytevector-u8-ref buf i))))))
       (define port 
         (make-custom-binary-output-port "output binary vector"
                                         write!
                                         get-position
                                         set-position!
                                         (lambda ()
                                           (unregister-output-vector! port)
                                           (mark-closed!))))
       (register-output-vector! port ov)
       port))))

(define open-output-textual-vector
  (case-lambda
    (() (make-output-textual-vector 256))
    ((init-capacity)
     (let-values (((ov write! get-position set-position! mark-closed!)
                   (make-output-vector-common 
                    init-capacity
                    (lambda (vec j buf i)
                      (vector-set! vec j 
                                   (if (string? buf)
                                     (string-ref buf i)
                                     (vector-ref buf i)))))))
       (define port
         (make-custom-textual-output-port "output textual vector"
                                          write!
                                          get-position
                                          set-position!
                                          (lambda ()
                                            (unregister-output-vector! port)
                                            (mark-closed!))))
       (register-output-vector! port ov)
       port))))

(define (get-output-vector vec-port)
  (let ((ov (port->output-vector vec-port)))
    (unless ov (error "port is not an output vector port" port))
    (flush-output-port vec-port) ; flush buffering in the implementation's port layer
    (let* ((size (output-vector-maxpos ov))
           (output (make-vector size)))
      (vector-copy! output 0 (output-vector-buffer ov) 0 size)
      output)))
