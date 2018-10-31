(library (hex-utils (1))
  (export hex? hex-prefix? pad nibble->integer
         hex-prefix! hex-string? hex->integer
         integer->nibble integer->hex integer-div
         strip-hex)
  (import (rnrs base))

    (define (hex-prefix? str)
      (and 
        (>= (string-length str) 2)
        (string=? (substring str 0 2) "0x")))

    (define (hex-prefix! str)
      (string-append "0x" str))

    (define (hex-string? str)
      (or 
        (= (string-length str) 0)
        (and (not (string? (nibble->integer (string-ref str 0))))
             (hex-string? (substring str 1 (string-length str))))))

    (define (hex? str)
      (and 
        (hex-string? (substring str 2 (string-length str)))
        (hex-prefix? str)))

    (define (strip-hex hex-string)
      (substring hex-string 2 (string-length hex-string)))

    (define (pad str len)
      (if (= (string-length str) len)
          (string-append "0x" str)
          (if (> (string-length str) len)
              "Error: string is longer than padding length"
              (pad (string-append "0" str) len))))

    ;;; If `nibble` is a valid hexadecimal digit, this function will return the 
    ;;; integer that it represents. If `nibble` is not a valid hexadecimal digit, 
    ;;; an error message will be returned. 
    (define (nibble->integer nib) 
      (if 
        (and (> (char->integer nib) 47) (< (char->integer nib) 58))
        (- (char->integer nib) 48)
        (if (and (> (char->integer nib) 96) (< (char->integer nib) 103))
            (- (char->integer nib) 87)
            "Error: Character is not valid nibble")))

    (define (integer->nibble int)
      (if (or (< int 0) (>= int 16))
          "Error: Intger is not representable by a nibble"
          (if (< int 10)
              (integer->char (+ int 48))
              (integer->char (+ int 87)))))

    (define (integer-div num den)
      (/ (- num (mod num den)) den))

    (define (integer->hex-string int)
      (if (< int 16)
          (string (integer->nibble (mod int 16)))
          (string-append 
            (integer->hex-string (integer-div int 16)) 
            (string (integer->nibble (mod int 16))))))

    (define (integer->hex int)
      (hex-prefix! (integer->hex-string int)))

    (define (hex-string->integer hex-string)
      (if (= 1 (string-length hex-string))
          (nibble->integer (string-ref hex-string 0))
          (+ (nibble->integer (string-ref hex-string (- (string-length hex-string) 1))) 
             (* 16 (hex-string->integer (substring hex-string 0 (- (string-length hex-string) 1)))))))


    ;;; Equivalent to `hex-string->integer except that this function checks that it's argument
    ;;; is a valid hex string, whereas hex-string->integer does no checks and is designed to  
    ;;; take un-prefixed byte arrays as input. 
    (define (hex->integer hex-string)
      (if (hex? hex-string)
          (hex-string->integer (substring hex-string 2 (string-length hex-string)))
          "Error: The provided string is not a hex string")))
