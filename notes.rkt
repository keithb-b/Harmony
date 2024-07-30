(module notes racket
  (provide C C♯ D D♯ E F F♯ G G♯ A A♯ B)
  (provide D♭ E♭ G♭ A♭ B♭ A♯)
  (provide at-interval-from same-pitch-class? notes-enharmonic? notes-equal?)

  (require (only-in srfi/1 concatenate))
  (require (only-in racklog
                    %which
                    %rel))

  (require "library.rkt")
  (require (only-in "intervals.rkt"
                    semitone-count-of
                    diminished))
  
  (define ♮ '♮)
  (define ♯ '♯)
  (define ♭ '♭)
 
  (define-syntax note
    (syntax-rules ()
      [(note name) `(,name ♮)]
      [(note name accidental) `(,name ,(cond [(equal? accidental '+) ♯] ;unicode is hard to type
                                             [(equal? accidental '-) ♭]
                                             [else accidental]))]))
                                      
  (define C (note 'C))
  (define C♯ (note 'C '♯))
  (define D♭ (note 'D '♭))
  (define D (note 'D))
  (define D♯ (note 'D '♯))
  (define E♭ (note 'E '♭))
  (define E (note 'E))
  (define F (note 'F))
  (define F♯ (note 'F '♯))
  (define G♭ (note 'G '♭))
  (define G (note 'G))
  (define G♯ (note 'G '♯))
  (define A♭ (note 'A '♭))
  (define A (note 'A))
  (define A♯ (note 'A '♯))
  (define B♭ (note 'B '♭))
  (define B (note 'B))
 
  (define %twelve-tet-enharmonic
    (%rel ()
          ((C♯ D♭))
          ((D♯ E♭))
          ((F♯ G♭))
          ((G♯ A♭))
          ((A♯ B♭))
          ((D♭ C♯)) ; a %rel has a direction but "enharmonic" is a symmetrical relation
          ((E♭ D♯ ))
          ((G♭ F♯))
          ((A♭ G♯))
          ((B♭ A♯))))
 
  (define chromatic-scale-C (list C C♯ D D♯ E F F♯ G G♯ A A♯ B))
 
  (define (notes-equal? l r)
    (let ([left-note-name (car l)]
          [left-accidental (cadr l)]
          [right-note-name (car r)]
          [right-accidental (cadr r)])
      (and (equal? left-note-name right-note-name)
           (equal? left-accidental right-accidental))))
 
  (define (notes-enharmonic? l r)
    (%succeeded?
     (%which ()
             (%twelve-tet-enharmonic l r))))
 
  (define (same-pitch-class? l r)
    (or (equal? l r)
        (notes-enharmonic? l r)))               
  
  (define (unison? interval)
    (eq? 0 (semitone-count-of interval)))

  (define (at-interval-from starting-note size)
    (let continue ([remaining-notes (concatenate (list chromatic-scale-C chromatic-scale-C))] ; avoid needing a modulus
                   [remaining-interval size]
                   [result 'null]
                   [within-the-interval #f]) ; this little state machine walks up the list of notes...
      (cond [(unison? remaining-interval) result]
            [else (let ([current-note (car remaining-notes)]
                        [notes-yet-to-be-seen (cdr remaining-notes)])
                    (cond
                      [(equal? starting-note current-note) (continue notes-yet-to-be-seen
                                                                     remaining-interval
                                                                     current-note
                                                                     #t)] ;... until we see that we're within the required interval
                      [else (continue notes-yet-to-be-seen
                                      (cond [within-the-interval (diminished remaining-interval)]
                                            [else remaining-interval])
                                      current-note
                                      within-the-interval)]))]))))
 
