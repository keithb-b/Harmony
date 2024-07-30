(module intervals racket

  (provide Diminished Minor Perfect Major Augmented)
  (provide unison second third fourth fifth sixth seventh octave)
  (provide interval name-of semitone-count-of quality-of inversion-of diminished)
  (provide P1 m2 M2 m3 M3 P4 d5 P5 m6 M6 m7 M7 P8)
  (provide %chromatic-interval-sequence)
  
  (require (only-in srfi/1
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

  (define first-order-intervals (zip diatonic-octave-quality-sequence
                                     diatonic-octave-interval-name-sequence
                                     diatonic-octave-interval-sizes))
 
  (define (quality-of interval)
    (car interval))
  (define (name-of interval)
    (cadr interval))
  (define (semitone-count-of interval)
    (last interval))
 
  (define (interval quality name)
    (let ([candidate-intervals (apply (compose (λ intervals (filter (λ (an-interval) (equal? quality (quality-of an-interval)))
                                                                    (car intervals)))
                                               (λ intervals (filter (λ (an-interval) (equal? name (name-of an-interval)))
                                                                    intervals)))
                                      first-order-intervals)])
      (cond [(not (eq? (length candidate-intervals) 1)) (error "I don't know about a "
                                                               (symbol->string quality)
                                                               (symbol->string name))]
            [else (car candidate-intervals)])))
    
 
  ; first-order interval names
  (define P1 (interval Perfect unison))
  (define m2 (interval Minor second))
  (define M2 (interval Major second))
  (define m3 (interval Minor third))
  (define M3 (interval Major third))
  (define P4 (interval Perfect fourth))
  (define d5 (interval Diminished fifth))
  (define P5 (interval Perfect fifth))
  (define m6 (interval Minor sixth))
  (define M6 (interval Major sixth))
  (define m7 (interval Minor seventh))
  (define M7 (interval Major seventh))
  (define P8 (interval Perfect octave))

  (define all-intervals (list P1 m2 M2 m3 M3 P4 d5 P5 m6 M6 m7 M7 P8))



  (define (inversion-of an-interval)
    (let ([defecit-to-octave (- 12 (semitone-count-of an-interval))])
      (car (filter (λ (candidate-interval) (equal? defecit-to-octave (semitone-count-of candidate-interval)))
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
                result-elements)]))))