;;;
;;; Generic implementation of custom ports.
;;;
;;; MIT License.  See COPYING
;;;

(define-record-type custom-port
  ;; constructor (internal)
  (%make-custom-port id binary? prefetch prefetch-pos buf
                     read! write! get-position set-position!
                     closer flusher)
  ;; predicate
  custom-port?

  ;; port name
  (id custom-port-id)
  ;; boolean flag
  (binary? custom-port-binary?)
  ;; prefeched byte / char, for peek-u8/peek-char
  (prefetch custom-port-prefetch custom-port-prefetch-set!)
  ;; saved the current pos before peek-char
  ;; can be (pos) or #f
  (prefetch-pos custom-port-prefetch-pos custom-port-prefetch-pos-set!)
  ;; one-element bytevector or vector
  (buf custom-port-buf)

  ;; handlers
  (read! custom-port-read!)
  (write! custom-port-write!)
  (get-position custom-port-get-position)
  (set-position! custom-port-set-position!)
  (closer custom-port-closer)
  (flusher custom-port-flusher))

(define (boolean x) (not (not x)))

(define (custom-port-has-port-position? port)
  (assume (custom-port? port))
  (boolean (custom-port-get-position port)))

(define (custom-port-has-set-port-position!? port)
  (assume (custom-port? port))
  (boolean (custom-port-set-position! port)))

(define (%call-get-position port)
  (if (custom-port-get-position port)
    ((custom-port-get-position port))
    0))

(define (%call-set-position! port pos)
  (when (custom-port-set-position! port)
    ((custom-port-set-position! port) pos)))

(define (custom-port-position port)
  (assume (and (custom-port? port)
               (custom-port-get-position port)))
  (if (custom-port-binary? port)
    (if (custom-port-prefetch port)
      (- (%call-get-position port) 1)
      (%call-get-position port))
    (or (custom-port-prefetch-pos port)
        (%call-get-position port))))

(define (custom-port-set-port-position! port pos)
  (assume (and (custom-port? port)
               (custom-port-set-position! port)))
  (custom-port-prefetch-set! port #f)
  (custom-port-prefetch-pos-set! port #f)
  (%call-set-position! port pos))

(define (custom-input-port? port)
  (boolean (and (custom-port? port)
                (custom-port-read! port))))

(define (custom-output-port? port)
  (boolean (and (custom-port? port)
                (custom-port-write! port))))

(define (custom-port-textual? port)
  (not (custom-port-binary? port)))

(define (custom-port-read-u8 port)
  (assume (and (custom-input-port? port)
               (custom-port-binary? port)))
  (let ((pre (custom-port-prefetch port)))
    (if pre
      (begin (custom-port-prefetch-set! port #f)
             pre)
      (let* ((buf (custom-port-buf port))
             (nbytes ((custom-port-read! port) buf 0 1)))
        (if (zero? nbytes)
          (eof-object)
          (bytevector-u8-ref buf 0))))))

(define (custom-port-peek-u8 port)
  (or (custom-port-prefetch port)
      (let* ((buf (custom-port-buf port))
             (nbytes ((custom-port-read! port) buf 0 1)))
        (if (zero? nbytes)
          (eof-object)
          (begin
            (custom-port-prefetch-set! port (bytevector-u8-ref buf 0))
            (bytevector-u8-ref buf 0))))))

(define (custom-port-read-char port)
  (assume (and (custom-input-port? port)
               (custom-port-textual? port)))
  (let ((pre (custom-port-prefetch port)))
    (if pre
      (begin (custom-port-prefetch-set! port #f)
             pre)
      (let* ((buf (custom-port-buf port))
             (nchars ((custom-port-read! port) buf 0 1)))
        (if (zero? nchars)
          (eof-object)
          (vector-ref buf 0))))))

(define (custom-port-peek-char port)
  (or (custom-port-prefetch port)
      (let* ((buf (custom-port-buf port))
             (pos (%call-get-position port))
             (nchars ((custom-port-read! port) buf 0 1)))
        (if (zero? nchars)
          (eof-object)
          (begin
            (custom-port-prefetch-set! port (vector-ref buf 0))
            (custom-port-prefetch-pos-set! port pos)
            (vector-ref buf 0))))))

(define (custom-port-write-u8 byte port)
  (let ((buf (custom-port-buf port)))
    (when (custom-port-prefetch port)
      (custom-port-prefetch-set! port #f)
      (custom-port-prefetch-pos-set! port #f)
      (%call-set-position! port (- (%call-get-position port) 1)))
    (bytevector-u8-set! buf 0 byte)
    ((custom-port-write! port) buf 0 1)))

(define (custom-port-write-char ch port)
  (let ((buf (custom-port-buf port)))
    (when (custom-port-prefetch port)
      (custom-port-prefetch-set! port #f)
      (%call-set-position! port (custom-port-prefetch-pos port)))
    (vector-set! buf 0 ch)
    ((custom-port-write! port) buf 0 1)))

(define (custom-port-close port)
  (let ((closer (custom-port-closer port)))
    (and closer (closer))))

(define (custom-port-flush port)
  (let ((flusher (custom-port-flusher port)))
    (and flusher (flusher))))

(define (make-custom-binary-input-port id read!
                                       get-position set-position!
                                       close)
  (%make-custom-port id #t #f #f (make-bytevector 1)
                     read! #f
                     get-position
                     set-position!
                     close
                     #f))

(define (make-custom-textual-input-port id read!
                                        get-position set-position!
                                        close)
  (%make-custom-port id #f #f #f (make-vector 1)
                     read! #f
                     get-position
                     set-position!
                     close
                     #f))

(define (make-custom-binary-output-port id write!
                                        get-position set-position!
                                        close . opts)
  (let ((flush (if (pair? opts) (car opts) #f)))
    (%make-custom-port id #t #f #f (make-bytevector 1)
                       #f write!
                       get-position
                       set-position!
                       close
                       flush)))

(define (make-custom-textual-output-port id write!
                                         get-position set-position!
                                         close . opts)
  (let ((flush (if (pair? opts) (car opts) #f)))
    (%make-custom-port id #f #f #f (make-vector 1)
                       #f write!
                       get-position
                       set-position!
                       close
                       flush)))

(define (make-custom-binary-input/output-port id read! write!
                                              get-position set-position!
                                              close . opts)
  (let ((flush (if (pair? opts) (car opts) #f)))
    (%make-custom-port id #t #f #f (make-bytevector 1)
                       read! write!
                       get-position
                       set-position!
                       close
                       flush)))
