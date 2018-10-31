(import (hex-utils))

  ;;; Helper functions ;;;

  (define (exp base power)
      (if (= 0 power)
          1
          (* base
            (exp base (- power 1)))))

  ;;; uint64 factory function ;;;

  (define (uint64 val) 
    (pad (strip-hex (integer->hex (mod val (exp 2 64)))) 16))
  
  ;;; uint64 queries

  (define (uint64? str)
    (and 
      (hex-prefix? str)
      (= (string-length str) 18)
      (hex? str)))

  ;;; uint64 constants ;;;

  (define max_uint64 18446744073709551615)

  ;;; integer--uint64 conversions ;;;

  (define (integer->uint64 val)
    (if (or (> val max_uint64) (< val 0))
        "Error: val is not a uint64"
        (pad (substring (integer->hex val) 2 (string-length (integer->hex val))) 16)))

  (define (uint64->integer val)
    (if (uint64? val)
        (hex->integer val)
        "Error: val is not a uint64"))

  ;;; Arithmetic ;;;

  (define (uint64-add a b)
    (integer->uint64 (mod (+ (uint64->integer a) (uint64->integer b)) (exp 2 64))))

  (define (uint64-mul a b)
    (integer->uint64 (mod (* (uint64->integer a) (uint64->integer b)) (exp 2 64))))

  (define (uint64-and a b)
    (integer->uint64 (logand (uint64->integer a) (uint64->integer b))))

  (define (uint64-or a b)
    (integer->uint64 (logior (uint64->integer a) (uint64->integer b))))

  (define (uint64-xor a b) 
    (integer->uint64 (logxor (uint64->integer a) (uint64->integer b))))

  (define (uint64-rot W r)
    (if (= (mod r 64) 0)
        W
        (integer->uint64 
          (logior 
            (bitwise-arithmetic-shift-left (uint64->integer W) (mod r 64)) 
            (bitwise-arithmetic-shift-right (uint64->integer W) (- 64 (mod r 64))))))) 
