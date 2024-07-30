(module scales racket
    (provide T S mode starting-degree-for diatonic-scale-pattern rotated-to scale-from)
  (provide Lydian)

  (require (only-in racklog
                    %which
                    %rel))
  
  (require "library.rkt")
  (require (only-in "intervals.rkt" m2 M2 m3 M3 d5 P5))
  (require (only-in "notes.rkt" at-interval-from))
                    
  (define S m2)
  (define T M2)
  (define diatonic-scale-pattern (list T T S T T T S))
 
  (define (scale-from note scale-pattern)
    (reverse (let next-note ([remaining-intervals scale-pattern] ; often easier to build the thing we want in reverse
                             [scale (list note)])
               (cond [(null? remaining-intervals) scale]
                     [else (next-note (cdr remaining-intervals)
                                      `(,(at-interval-from (car scale)
                                                           (car remaining-intervals))
                                        ,@scale))]))))
 
  (define Ionian 'Ionian)
  (define Dorian 'Dorian)
  (define Phrygian 'Phrygian)
  (define Lydian 'Lydian)
  (define Mixolydian 'Mixolydian)
  (define Aeolian 'Aeolian)
  (define Locrian 'Locrian)
 
  (define %mode-starting-degrees
    (%rel ()
          ((Ionian 1))
          ((Dorian 2))
          ((Phrygian 3))
          ((Lydian 4))
          ((Mixolydian 5))
          ((Aeolian 6))
          ((Locrian 7))))
 
  (define (starting-degree-for mode-name)
    (let ([answer (%which (Mode-starting-degree)
                          (%mode-starting-degrees mode-name Mode-starting-degree ))])
      (cond
        [(%failed? answer) (error "I don't know this mode: " mode-name)]
        [else (let* ([binding (car answer)] ; expect only one binding
                     [result-elements (cdr binding)])
                result-elements)])))
 
  (define (rotated-to one-based-index list)
    (let continue ([result list]
                   [current-index one-based-index])
      (cond
        [(eq? 1 current-index) result]
        [else (continue `(,@ (cdr result) ,(car result))
                        (- current-index 1))])))
 
  (define (mode root-note mode-name)
    (scale-from root-note (rotated-to (starting-degree-for mode-name)
                                      diatonic-scale-pattern)))
 
 
 
  ; conventional tertian triads
  (define (tertian-triad-shape third-quality fifth-quality)
    (list third-quality fifth-quality))
 
  (define M-shape (tertian-triad-shape M3 P5))
  (define m-shape (tertian-triad-shape m3 P5))
  (define dim-shape (tertian-triad-shape m3 d5))
 
  (define (chord-on root-note shape)
    `(, root-note ,@(map (lambda (interval)
                           (at-interval-from root-note interval))
                         shape))))