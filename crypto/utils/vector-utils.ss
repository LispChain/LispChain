;;; Vector Getters ;;;

(define (vector-ref-multi v idxs)
  (let ((ref (vector-ref v (car idxs))))
    (if (not (pair? (cdr idxs))) 
        ref
        (vector-ref-multi ref (cdr idxs))))) 

;;; Vector Setters ;;;

(define (vector-set-multi! v idxs value)
    (if (not (pair? (cdr idxs))) 
        (vector-set! v (car idxs) value)
        (vector-set-multi! (vector-ref v (car idxs)) (cdr idxs)))) 
