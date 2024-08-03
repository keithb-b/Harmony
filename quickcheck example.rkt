#lang racket
(require quickcheck)

(struct point (x y) #:transparent)
(define (distance p1 p2)
    (let ([dx (- (point-x p1)
                 (point-x p2))]
          [dy (- (point-y p1)
                 (point-y p2))])
      (sqrt (+ (* dx dx)
               (* dy dy)))))

(define (choose-point [min-coord -9999] [max-coord 9999])
    (bind-generators
     ([x (choose-integer min-coord max-coord)]
      [y (choose-integer min-coord max-coord)])
     (point x y)))

(quickcheck
   (property ([p1 (choose-point)]
              [p2 (choose-point)])
     (= (distance p1 p2)
        (distance p2 p1))))
