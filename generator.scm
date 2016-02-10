(use srfi-27)

(define (sample lst)
  (list-ref lst (random-integer (length lst))))

(define (assoc-all alist obj)
  (map cdr
       (filter (lambda (x)
                 (eq? (car x) obj))
               alist)))

;(define (generate rules seed)
;  (let loop ((stack (list seed))
;             (stream '()))
;    (if (null? stack)
;      stream
;      (let ((top (car stack))
;            (product (assoc-all rules (car stack))))
;        (if (null? product)
;          (loop (cdr stack) (append stream (list top)))
;          (loop (append (sample product) (cdr stack))
;                stream))))))
;
;(define SAID
;  '((Stmt S V O)
;    (S I)
;    (S He)
;    (S She)
;    (V said)
;    (O nothing)
;    (O that Stmt)))
;
;(define NP&NP
;  '((NP NP and NP)
;    (NP n)))
;
;(define KU
;  '((S NP VP)
;    (NP pron) (NP det n) (NP NP PP)
;    (VP v) (VP v NP) (VP VP PP)
;    (PP prep NP)
;    (pron I)
;    (det a)
;    (v broke)
;    (prep with)
;    (n I)
;    (n desk)
;    (n drawer)))

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

(define (show)
  (do ((i 0 (+ i 1)))
      ((= i 20))
    (print (list->sentence (generate grammer 'S)))))