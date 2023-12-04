(use-modules (ice-9 rdelim)
             (ice-9 format)
             (ice-9 textual-ports))

(define input
  (filter (lambda (s) (not (string-null? s)))
          (string-split (call-with-input-file "input.txt" get-string-all)
                        #\newline)))

(define (parse-numbers nums)
  (map string->number
       (filter (lambda (s)
                 (not (string-null? s)))
               (string-split nums #\space))))

(define (parse-card card)
  (let* ((a (string-split card #\:))
         (id (string->number (cadr (filter (lambda (x)
                                             (not (string-null? x)))
                                           (string-split (car a) #\space)))))
         (numbers (string-split (cadr a) #\|))
         (winning (parse-numbers (car numbers)))
         (elfs (parse-numbers (cadr numbers))))
    (cons id (list winning elfs))))

(define (-calculate-points card acc)
  (if (null? (cadr card))
      acc
      (let* ((winning (car card))
             (numbers (cadr card))
             (num (car numbers)))
        (-calculate-points (list winning (cdr numbers))
                           (if (member num winning)
                               (if (zero? acc)
                                   1
                                   (* acc 2))
                               acc)))))

(define (calculate-points card)
  (-calculate-points (cdr card) 0))

(define cards (map parse-card input))

(format #t "part 1: ~a\n" (apply + (map calculate-points cards)))

(define (-matching-numbers-count card acc)
  (if (null? (cadr card))
      acc
      (let* ((winning (car card))
             (numbers (cadr card))
             (num (car numbers)))
        (-matching-numbers-count
         (list winning (cdr numbers))
         (if (member num winning)
             (1+ acc)
             acc)))))

(define (matching-numbers-count card)
  (-matching-numbers-count (cdr card) 0))

(define (-calculate-total-amount-of-cards cards card-counts)
  (if (null? cards)
      (apply + (map cdr card-counts))
      (let* ((card (car cards))
             (id (car card))
             (matching (matching-numbers-count card))
             (count (cdr (assoc id card-counts))))
        (map (lambda (x)
               (assoc-set! card-counts
                           x
                           (+ (cdr (assoc x card-counts))
                              count)))
             (iota matching
                   (1+ id)))
        (-calculate-total-amount-of-cards
         (cdr cards)
         card-counts))))

(define (calculate-total-amount-of-cards cards)
  (-calculate-total-amount-of-cards cards
                                    (map (lambda (c)
                                           (cons (car c) 1))
                                         cards)))

(format #t "part 2: ~a\n" (calculate-total-amount-of-cards cards))
