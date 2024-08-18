(module intervals racket

  (provide Diminished Minor Perfect Major Augmented)
  (provide unison second third fourth fifth sixth seventh octave)
  (provide interval interval-semitone-count interval-quality)
  (provide interval-of-a inversion-of span-of stack diminished)
  (provide P1 m2 M2 m3 M3 P4 d5 P5 m6 M6 m7 M7 P8)
  (provide %chromatic-interval-sequence)
  
  (require (only-in srfi/1
                    fold
                    concatenate
                    zip
                    iota))
  
  (require (only-in racklog
                    %rel
                    %which))
  
  (require "library.rkt")
  
  ; interval qualities
  (define Diminished 'Diminished)
  (define Minor 'Minor)
  (define Perfect 'Perfect)
  (define Major 'Major)
  (define Augmented 'Augmented)
 
  (define diatonic-half-scale-quality-sequence (list Perfect Minor Major Minor Major Perfect))
 
  (define diatonic-octave-quality-sequence (append diatonic-half-scale-quality-sequence
                                                   `(Diminished)
                                                   diatonic-half-scale-quality-sequence))

  ; interval names
  (define unison 'unison)
  (define second 'second)
  (define third 'third)
  (define fourth 'fourth)
  (define fifth 'fifth)
  (define sixth 'sixth)
  (define seventh 'seventh)
  (define octave 'octave)

  (define diatonic-octave-interval-name-sequence (let ([twice (λ (x)
                                                                (list x x))])
                                                   `(,unison
                                                     ,@(twice second)
                                                     ,@(twice third)
                                                     ,fourth
                                                     ,@(twice fifth)
                                                     ,@(twice sixth)
                                                     ,@(twice seventh)
                                                     ,octave)))
  
  (define diatonic-octave-interval-sizes (iota 13))

  (struct interval (quality name semitone-count)
    #:transparent)
  
  (define first-order-intervals (map interval
                                     diatonic-octave-quality-sequence
                                     diatonic-octave-interval-name-sequence
                                     diatonic-octave-interval-sizes))
 
  (define (interval-of-a quality name)
    (let ([candidate-intervals (apply (compose (λ intervals (filter (λ (an-interval) (equal? quality (interval-quality an-interval)))
                                                                    (car intervals)))
                                               (λ intervals (filter (λ (an-interval) (equal? name (interval-name an-interval)))
                                                                    intervals)))
                                      first-order-intervals)])
      (cond [(not (eq? (length candidate-intervals) 1)) (error "I don't know about a "
                                                               (symbol->string quality)
                                                               (symbol->string name))]
            [else (car candidate-intervals)])))
    
 
  ; first-order interval names
  (define P1 (interval-of-a Perfect unison))
  (define m2 (interval-of-a Minor second))
  (define M2 (interval-of-a Major second))
  (define m3 (interval-of-a Minor third))
  (define M3 (interval-of-a Major third))
  (define P4 (interval-of-a Perfect fourth))
  (define d5 (interval-of-a Diminished fifth))
  (define P5 (interval-of-a Perfect fifth))
  (define m6 (interval-of-a Minor sixth))
  (define M6 (interval-of-a Major sixth))
  (define m7 (interval-of-a Minor seventh))
  (define M7 (interval-of-a Major seventh))
  (define P8 (interval-of-a Perfect octave))

  (define all-intervals (list P1 m2 M2 m3 M3 P4 d5 P5 m6 M6 m7 M7 P8))



  (define (inversion-of an-interval)
    (let ([defecit-to-octave (- 12 (interval-semitone-count an-interval))])
      (car (filter (λ (candidate-interval) (equal? defecit-to-octave (interval-semitone-count candidate-interval)))
                   all-intervals))))           

  (define %chromatic-interval-sequence
    (%rel ()
          ((P1 m2))
          ((m2 M2))
          ((M2 m3))
          ((m3 M3))
          ((M3 P4))
          ((P4 d5))
          ((d5 P5))
          ((P5 m6))
          ((m6 M6))
          ((M6 m7))
          ((m7 M7))
          ((M7 P8))))

 
  (define (diminished interval)
    (let ([answer (%which (Preceeding-interval)
                          (%chromatic-interval-sequence Preceeding-interval interval))])
      (cond
        [(%failed? answer) (error "I don't know how to diminish this: " interval)]
        [else (let* ([binding (car answer)] ; expect only one binding
                     [result-elements (cdr binding)])
                result-elements)])))

  (define stack list)
  (define (span-of possibly-stacked-intervals)
    (cond [(list? possibly-stacked-intervals) (fold + 0
                                                    (map interval-semitone-count
                                                         possibly-stacked-intervals))]
          [else (interval-semitone-count possibly-stacked-intervals)])))