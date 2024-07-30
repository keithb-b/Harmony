(module library racket
  (provide %succeeded? %failed?)

  ; why doesn't racklog have these utilities anyway?
  (define %success '()) ; answer? of a goal satisfied without binding variables
  (define %failure #f) ; answer? of a goal not satisfied
  (define (%succeeded? goal-answer)
    (eq? %success goal-answer))
  (define (%failed? goal-answer)
    (eq? %failure goal-answer)))