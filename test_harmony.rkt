(module test_harmony racket
  (require rackunit)       
  (require rackunit/text-ui)

  (require "scales.rkt")
  (require "notes.rkt")

  (define scale-tests
    (test-suite "scales"
                (test-suite "modes"
                            (test-suite "diatonic modes"
                                        (test-case "C Lydian"
                                                   (check-equal? (mode C Lydian)
                                                                 (list C D E F♯ G A B C))))
                            (test-suite "rotating patterns"
                                        (test-case "no steps"
                                                   (check-equal? (rotated-to 1 diatonic-scale-pattern)
                                                                 (list T T S T T T S)))
                                        (test-case "some steps"
                                                   (check-equal? (rotated-to 4 diatonic-scale-pattern)
                                                                 (list T T T S T T S))))
                            (test-suite "starting degrees"
                                        (test-case "Lydian"
                                                   (check-equal? (starting-degree-for Lydian) 4))))
                (test-suite "diatonic scale"
                            (test-suite "major scale"
                                        (test-case "C"
                                                   (check-equal? (scale-from C diatonic-scale-pattern)
                                                                 (list C D E F G A B C)))))))

  
  (require "intervals.rkt")

  (define interval-tests
    (test-suite "intervals"
                (test-suite "inversion"
                            (test-case "perfect intervals remain perfect"
                                       (check-equal? P4 (inversion-of P5))
                                       (check-equal? P5 (inversion-of P4)))
                            (test-case "minor intervals become major"
                                       (check-equal? Major (quality-of (inversion-of m2)))
                                       (check-equal? Major (quality-of (inversion-of m3)))
                                       (check-equal? Major (quality-of (inversion-of m6)))
                                       (check-equal? Major (quality-of (inversion-of m7))))
                            (test-case "major intervals become major"
                                       (check-equal? Minor (quality-of (inversion-of M2)))
                                       (check-equal? Minor (quality-of (inversion-of M3)))
                                       (check-equal? Minor (quality-of (inversion-of M6)))
                                       (check-equal? Minor (quality-of (inversion-of M7))))
                            (test-case "duals" ))
                
                (test-suite "diminution"
                            (test-case "bigger intervals get smaller"
                                       (check-equal? m3 (diminished M3)))
                            (test-case "for now, attempts to diminish unison indicates an error"
                                       (check-exn exn:fail?
                                                  (lambda ()
                                                    (diminished P1)))))
                (test-suite "equivalence"
                            (test-case "tone"
                                       (check-true (same-pitch-class? (at-interval-from C T) D)))
                            (test-case "semitone"
                                       (check-true (same-pitch-class? (at-interval-from C S) C♯)))
                            (test-case "semitone - enharmonic"
                                       (check-true (same-pitch-class? (at-interval-from C S) D♭)))
                            (test-case "tone from accidental"
                                       (check-true (same-pitch-class? (at-interval-from C♯ T) D♯)))
                            (test-case "tone from accidental - enharmonic"
                                       (check-true (same-pitch-class? (at-interval-from C♯ T) E♭)))
                            (test-case "semitone from accidental"
                                       (check-true (same-pitch-class? (at-interval-from C♯ S) D))))))

  (define note-tests
    (test-suite "notes"
                (test-case "same pitch class, allowing for 12TET enharmonics"
                           (check-true  (same-pitch-class? C♯ C♯))
                           (check-true  (same-pitch-class? C♯ D♭))
                           (check-false (same-pitch-class? C D♭)))
                (test-case "enharmonics"
                           (check-true  (notes-enharmonic? C♯ D♭))
                           (check-false (notes-enharmonic? C  D♭)))
                (test-case "note equality (note not pitch-class)"
                           (check-true (notes-equal? C C))
                           (check-true (notes-equal? C♯ C♯))
                           (check-false (notes-equal? C♯ D♭))
                           (check-false (notes-equal? C♯ D)))))
  
  (run-tests
   (test-suite "harmony"
               scale-tests
               interval-tests
               note-tests)))                                           
                        
