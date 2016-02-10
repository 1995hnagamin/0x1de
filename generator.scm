(use srfi-27)
(random-source-randomize! default-random-source)

(define (sample lst)
  (list-ref lst (random-integer (length lst))))

(define (snoc lst x)
  (append lst (list x)))

(define (generate rules seed)
  (let loop ((stack (list seed))
             (stream '()))
    (cond
      ((null? stack) stream)
      ((assoc (car stack) rules) =>
        (lambda (cands)
          (let ((cand (sample cands)))
            (loop (append (if (list? cand) cand (list cand))
                          (cdr stack))
                  stream))))
      (else (loop (cdr stack)
                  (snoc stream (car stack)))))))

(define (list->sentence lst)
  (string-append (string-join (map x->string lst) " ")
                 "."))

(define grammer
  '(
    (S        NP (VT NP))
    (NP       (a N-single) (a AJS N-single) N-plural (AJS N-plural))
    (N-single food babe cafe beef face feed cable office bed disease fish seed sofa coffee bee
              (side effect))
    (N-plural faces boobs fish bees sofas beds (side effects))
    (VT       feed decide)
    (VI       feel decafe face feed defecate)
    (AJS      ADJ)
    (ADJ      bad dead big sad)
    ))
