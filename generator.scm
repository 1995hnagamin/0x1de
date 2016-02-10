(use srfi-27)

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
  '((S   (NP VP) (NP~ VP~) (N VP) (D N~ VP~))
    (NN  NP NP~)
    (NP  N (AJS N) (the N) (NN and NN))
    (NP~ (D AJS N~) (D N~))
    (VP  (VT NP) (VI PP))
    (VP~ (VT~ NP) (VI~ PP))
    (PP  (P NP) (P NP~) ADV)
    (AJS ADJ (ADV ADJ) (very ADJ))
    (N   ideas coffee information salt men women dogs cats flowers houses hammers)
    (N~  idea man woman dog cat flower house beauty freedom hammer foundation morning day night)
    (VI  sleep walk (get up) collapse)
    (VI~ sleeps walks (gets up) collapses)
    (VT  walk like break drink read found)
    (VT~ walks likes breaks drinks reads founds)
    (ADJ colorless green harmless beautiful free collapsed foundable good)
    (ADV fast harmlessly beautifully)
    (P   like with in without on by)
    (D   a the)))
