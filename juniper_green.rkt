#lang racket

(require memoize)
(require rackunit)

(define (max/argmax-recursive f xs)
  (if (null? xs)
      (list -inf.0 '())
      (let* ((x (car xs))
             (fx (f x)))
        (if (eqv? +inf.0 fx)
            (list fx x)
            (let* ((fy_y (max/argmax-recursive f (cdr xs)))
                   (fy (car fy_y))
                   (y (cadr fy_y)))
              (if (>= fx fy)
                  (list fx x)
                  fy_y))))))

(define (min/argmin-recursive f xs)
  (if (null? xs)
      (list +inf.0 '())
      (let* ((x (car xs))
             (fx (f x)))
        (if (eqv? -inf.0 fx)
            (list fx x)
            (let* ((fy_y (min/argmin-recursive f (cdr xs)))
                   (fy (car fy_y))
                   (y (cadr fy_y)))
              (if (<= fx fy)
                  (list fx x)
                  fy_y))))))




(define (combine-evals x y f)
  (let ((fx (f x))
        (fy (f y)))
    (if (<= (- (car fx)) (- (car fy)))
        (list (- (car fx)) (cons x (cadr fx)))
        (list (- (car fy)) (cons y (cadr fy))))))

                
(define/memo (muldivs n max)
  (set-union (divs n) (multiples n max)))
(define/memo (divs n)
  (list->set (filter (λ(_) (eq? 0 (modulo n _)))
                     (range 1 (+ 1 (quotient n 2))))))

(define (multiples-recursive n max i)
  (let ((p (* i n)))
    (if (> p max)
        '()
        (cons p (multiples-recursive n max (+ i 1))))))

(define (multiples-tailrec n max i res)
  (let ((p (* i n)))
    (if (> p max)
        res
        (multiples-tailrec n max (+ i 1) (set-add res p)))))

(define/memo (multiples n max)
  (multiples-tailrec n max 2 (set)))

; n and all elements of withdrawns must be <= max
(define (choices n withdrawns max)
  (set-subtract (muldivs n max) withdrawns))


; computes evalutate from the list of choices for next move.
; f returns a list of 2 elements:
; the evaluation, and the list of moves
(define (min/cons-recursive f xs)
  (if (null? xs)
      (list +inf.0 '())
      (let* ((x (car xs))
             (fx (f x))
             (eval-value (- (car fx))))
        ; optimization: since we look for min, skip recursive call if we have -inf.0
        (if (eqv? -inf.0 eval-value)
            (list eval-value (cons x (cadr fx)))
            (let* ((mincdr (min/cons-recursive f (cdr xs))))
              (if (<= eval-value (car mincdr))
                  (list eval-value (cons x (cadr fx)))
                  mincdr))))))


; elements of hints must not be in withdrawns
; elements of hinst must be in (choices n withdrawns max)
; returns (list evaluation moves) where
; evaluation is an number +inf.0 if the move is winning
;                         -inf.0 if the move is losing
; moves is a list of next moves leading to the end of game.
(define (evaluate n withdrawns max start-time max-time depth hints)
  (let ((cs (choices n withdrawns max)))
    (cond ((set-empty? cs) (list +inf.0 '())) ;adversary has no possibility, so you won
          ((and (null? hints)
                (> (- (current-inexact-milliseconds) start-time)
                   max-time))
           (list (set-count cs) '()))
          (else (min/cons-recursive ;*
                 (λ(_) (evaluate _ (set-add withdrawns n) max start-time max-time (+ depth 1) '()))
                 (append hints (set->list cs)))))))


;* if we have 2 possible next moves, one evaluated to +inf.0 (winning for the player)
;  and one evaluated to -inf.0 (losing for the player), the present move is losing,
;  because the adversary has a possibility to win. Therefore we must take the min of
;  (- (car (evaluate ...))), wich is the same as minus the max of (car (evaluate ..)).

(define (print-number i maxlen)
  (let ((len (string-length (number->string i))))
    (display (make-string (+ 1 (- maxlen len)) #\space))
    (print i)))

(define (print-_ i maxlen)
  (let ((len (string-length (number->string i))))
    (display (make-string (+ 1 (- maxlen len)) #\space))
    (display (make-string  len #\_))))


(define (print-game withdrawns max)
  (let ((maxlen (string-length (number->string max))))
    (for ([i (range 1 (+ 1 max))])
      (if (set-member? withdrawns i)
          (print-_ i maxlen)
          (print-number i maxlen))
      (when (equal? 0 (modulo i 10))
          (newline)))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (check-equal?
           (divs 72)
           (set 1 9 18 3 24 2 6 4 8 12 36)
           "divs 72")

          (check-equal?
           (multiples-recursive 2 100 1)
           '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100)
           "(multiples-recursive 2 100 1)")

          (check-equal?
           (multiples-recursive 2 100 40)
           '(80 82 84 86 88 90 92 94 96 98 100)
           "(multiples-recursive 2 100 40)")

          (check-equal?
           (multiples-tailrec 12 100 1 '())
           '(96 84 72 60 48 36 24 12)
           "(multiples-tailrec 12 100 1 '())")
 


          (check-equal?
           (multiples 12 100 )
           (set 84 60 24 48 72 96 36)
           "")



          (check-equal?
           (muldivs 12 100)
           (set 1 84 3 60 24 48 72 96 2 6 4 36)
           "")


          (check-equal?
           (choices 12 (set) 100)
           (set 1 84 3 60 24 48 72 96 2 6 4 36)
           "")


          (check-equal?
           (choices 12 (set 84) 100)
           (set 1 3 60 24 48 72 96 2 6 4 36)
           "")


          (check-equal?
           (choices 12 (set 84 96) 100)
           (set 1 3 60 24 48 72 2 6 4 36)
           "")


          (check-equal?
           (choices 12 (set 23 84 96) 100)
           (set 1 3 60 24 48 72 2 6 4 36)
           "")




          (check-equal?
           (choices 97 (set) 100)
           (set 1)
           "")


          (check-equal?
           (muldivs 97 100)
           (set 1)
           "")


          (check-equal?
           (divs 97)
           (set 1)
           "")




          (check-equal?
           (min/cons-recursive (λ(x) (list (abs x) '())) '())
           '(+inf.0 ())
           "")


          (check-equal?
           (max/argmax-recursive abs '())
           '(-inf.0 ())
           "")

          (check-equal?
           (min/cons-recursive (λ(x) (list (abs x) '())) '(-1))
           '(-1 (-1))
           "")


          (check-equal?
           (max/argmax-recursive abs '(-1))
           '(1 -1)
           "")

          (check-equal?
           (min/cons-recursive (λ (x) (list (if (odd? x) +inf.0 x) '())) '(-1))
           '(-inf.0 (-1))
           "")


          (check-equal?
           (min/cons-recursive identity '((-inf.0 (3)) (-inf.0 (6)) (+inf.0 ())))
           '(-inf.0 ((+inf.0 ())))
           "")


          (check-equal?
           (max/argmax-recursive (λ (x) (if (odd? x) +inf.0 x)) '(-1))
           '(+inf.0 -1)
           "")



          (check-equal?
           (max/argmax-recursive (λ (x) (if (odd? x) +inf.0 x)) '(-1 2))
           '(+inf.0 -1)
           "")


          (check-equal?
           (max/argmax-recursive (λ (x) (if (odd? x) +inf.0 x)) '( 2 -1))
           '(+inf.0 -1)
           "")



          (check-equal?
           (min/argmin-recursive (λ (x) (if (odd? x) -inf.0 x)) '(-1))
           '(-inf.0 -1)
           "")

          (check-equal?
           (max/argmax-recursive (λ (x) (if (odd? x) -inf.0 x)) '( 2 -1))
           '(2 2)
           "")

          (check-equal?
           (max/argmax-recursive (λ (x) (if (odd? x) -inf.0 x)) '( -1 2))
           '(2 2)
           "")

          (check-equal?
           (max/argmax-recursive abs '(1 -2))
           '(2 -2)
           "")


          (check-equal?
           (max/argmax-recursive abs '(2 -1))
           '(2 2)
           "")


          (check-equal?
           (max/argmax-recursive abs '(2 -1 0))
           '(2 2)
           "")


          (check-equal?
           (max/argmax-recursive abs '(2 -1 -4 7 -0.5))
           '(7 7)
           "")


          (check-equal?
           (evaluate 97 (set 1) 100 (current-inexact-milliseconds) 1000 0 '())
           '(+inf.0 ())
           "")


          (check-equal?
           (choices 97 (set 1) 100)
           (set)
           "")


          (check-equal?
           (evaluate 1 (set 2) 100 (current-inexact-milliseconds) 100000 0 '(97))
           '(-inf.0 (97))
           "")


          (check-equal?
           (choices 3 (set 1 2) 3)
           (set)
           "")

          (check-equal?
           (evaluate 3 (set 1 2) 3 (current-inexact-milliseconds) 10000000 0 '())
           '(+inf.0 ())
           "")

          (check-equal?
           (evaluate 1 (set  2 3) 3 (current-inexact-milliseconds) 10000000 0 '())
           '(+inf.0 ())
           "")

          (check-equal?
           (evaluate 1 (set 2) 3 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (3))
           "")


          (check-equal?
           (evaluate 3 (set 2) 3 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (1))
           "")

          (check-equal?
           (evaluate 2 (set) 3 (current-inexact-milliseconds) 10000000 0 '())
           '(+inf.0 (1 3))
           "")


          (check-equal?
           (evaluate 4 (set) 4 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (2 1 3))
           "")


          (check-equal?
           (evaluate 4 (set) 5 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (2 1 3))
           "")


          (check-equal?
           (evaluate 2 (set) 4 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (4 1 3))
           "")


          (check-equal?
           (evaluate 2 (set) 5 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (4 1 3))
           "")


          (check-equal?
           (evaluate 2 (set) 6 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (4 1 5))
           "")
 
 

          (check-equal?
           (evaluate 2 (set) 7 (current-inexact-milliseconds) 10000000 0 '())
           '(-inf.0 (4 1 7))
           "")

          (check-equal?
           (evaluate 2 (set) 8 (current-inexact-milliseconds) 10000000 0 '())
           '(+inf.0 (8 4 1 7))
           "")

          (check-equal?
           (evaluate 12 (set 23 84 96) 100 (current-inexact-milliseconds) 0 0 '())
           '(10 ())
           "")









          ;
          ;(check-equal?
          ;
          ; (set)
          ;  "")
          ; 
          