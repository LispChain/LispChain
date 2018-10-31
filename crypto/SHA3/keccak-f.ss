(load "utils/hex.ss")
(load "utils/uint64.ss")
(load "utils/vector-utils.ss")

;;; The Keccak Round Constants ;;;
(define keccak-RC (vector "0x0000000000000001" "0x0000000000008082" 
                          "0x800000000000808a" "0x8000000080008000"
                          "0x000000000000808b" "0x0000000080000001"
                          "0x8000000080008081" "0x8000000000008009"
                          "0x000000000000008a" "0x0000000000000088"
                          "0x0000000080008009" "0x000000008000000a"
                          "0x000000008000808b" "0x800000000000008b"
                          "0x8000000000008089" "0x8000000000008003"
                          "0x8000000000008002" "0x8000000000000080"
                          "0x000000000000800a" "0x800000008000000a"
                          "0x8000000080008081" "0x8000000000008080"
                          "0x0000000080000001" "0x8000000080008008"))

;;; The Keccak Rotation Offsets ;;;
(define keccak-r (vector (vector (uint64 0) (uint64 36) (uint64 3) (uint64 41) (uint64 18))
                         (vector (uint64 1) (uint64 44) (uint64 10) (uint64 45) (uint64 2))
                         (vector (uint64 62) (uint64 6) (uint64 43) (uint64 15) (uint64 61))
                         (vector (uint64 28) (uint64 55) (uint64 25) (uint64 21) (uint64 56))
                         (vector (uint64 27) (uint64 20) (uint64 39) (uint64 8) (uint64 14))))

;;; @note While the round function defined in the keccak specification takes in an additional
;;; argument, b, which specfies the lane size, at the moment I have decided not to include this 
;;; parameter. Eventually, I may try to generalize this. 
(define (keccak-round A RC)
  (let 
    ((B (vector (make-vector 5) (make-vector 5) (make-vector 5) (make-vector 5) (make-vector 5))) 
     (C (make-vector 5))
     (D (make-vector 5))
     ;;; Theta step functions ;;;
     (theta-C 
       (lambda (C A x)
         (cond 
           ((> x 4) '())
           (vector-set! C 
                        x 
                        (uint64-xor (vector-ref-multi A (list x 0)) 
                                    (uint64-xor (vector-ref-multi A (list x 1))
                                                (uint64-xor (vector-ref-multi A (list x 2)) 
                                                            (uint64-xor (vector-ref-multi A (list x 3))
                                                                        (vector-ref-multi A (list x 4)))))))
           (theta-C C A (+ x 1)))))
     (theta-D 
       (lambda (D C x)
         (cond 
           ((> x 4) '())
           (vector-set! D x (uint64-xor (vector-ref C (mod (- x 1) 5))
                                        (uint64-rot (vector-ref C (mod (+ x 1) 5)) 
                                                    1)))
           (theta-D D C (+ x 1)))))
     (theta-A
       (lambda (A D x y)
         (cond 
           ((> y 4) '()) 
           ((> x 4) (theta-A A D 0 (+ y 1)))
           (vector-set-multi! A 
                              (list x y) 
                              (uint64-xor (vector-ref-multi A (list x y))
                                          (vector-ref D x)))
           (theta-A A D (+ x 1) y))))
     ;;; Rho and Pi step function ;;;
     (rho-pi
       (lambda (B A x y)
         (cond 
           ((> y 4) '())
           ((> x 4) (rho-pi B A 0 (+ y 1)))
           (vector-set-multi! B
                              (list y (mod (+ (* 2 x) (* 3 y)) 5))
                              (uint64-rot (vector-ref-multi A (list x y))
                                          (vector-ref-multi keccak-r (list x y))))
           (rho-pi B A (+ x 1) y)))) 
     ;;; Chi step function ;;;
     (chi
       (lambda (A B x y)
         (cond 
           ((> y 4) '())
           ((> x 4) (chi A B 0 (+ y 1)))
           (vector-set-multi! A
                              (list x y)
                              (uint64-xor (vector-ref-multi B (list x y))
                                          (and (not (vector-ref-multi B (list (mod (+ x 1) 5) y)))
                                               (vector-ref-multi B (list (mod (+ x 2) 5) y)))))
           (chi A B (+ x 1) y)))))
    ;;; theta step
    (theta-C C A 0)
    (theta-D D C 0)
    (theta-A A D 0 0)
    ;;; rho and pi steps
    (rho-pi B A 0 0)
    ;;; chi step
    (chi A B 0 0)
    ;;; l step
    (vector-set-multi! A (list 0 0) (uint64-xor (vector-ref-multi A (list 0 0)) RC))
    A))

;;; Keccak-f ;;;
(define (keccak-f A)
  (let 
    ((sub-keccak-f 
       (lambda (A idx) 
         (if (= idx 24) 
             A 
             (sub-keccak-f (keccak-round A (vector-ref keccak-RC i)) (+ idx 1))))))
    (sub-keccak-f A 0)))
