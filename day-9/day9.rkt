#lang racket

(define sample-input
  '("0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"))

(define (parse-input input)
  (map (lambda (x)
         (map string->number
              (string-split x)))
       input))

(define (difference lst [diff (list)])
  (if (= 1 (length lst))
      diff
      (let {[a (first lst)]
            [b (second lst)]}
        (difference (rest lst)
                    (append diff (list (- b a)))))))

(define (guess-next-number lst)
  (let {[diffs (list (difference lst))]}
    (letrec {[fill-diffs
              (lambda ()
                (unless (for/and ([n (in-list (last diffs))])
                          (zero? n))
                  (set! diffs
                        (append diffs
                                (list (difference (last diffs)))))
                  (fill-diffs)))]}
      (fill-diffs)
      (apply +
             (last lst)
             (map last (rest (reverse diffs)))))))

(define (part-1 input)
  (apply + (map guess-next-number (parse-input input))))

(define (file->input path)
  (filter (lambda (s)
            (not (= (string-length s)
                    0)))
          (file->lines path)))

(define input
  (file->input "input.txt"))

(fprintf (current-output-port)
         "part 1 (sample input): ~a\n"
         (part-1 sample-input))

(fprintf (current-output-port)
         "part 1: ~a\n"
         (part-1 input))

(define (extrapolate-backwards lst)
  (if (= 1 (length lst))
      (first lst)
      (let {[a (first lst)]
            [b (second lst)]}
        (extrapolate-backwards (append (list (- b a))
                                       (cddr lst))))))

(define (guess-previous-number lst)
  (let {[diffs (list (difference lst))]}
    (letrec {[fill-diffs
              (lambda ()
                (unless (for/and ([n (in-list (last diffs))])
                          (zero? n))
                  (set! diffs
                        (append diffs
                                (list (difference (last diffs)))))
                  (fill-diffs)))]}
      (fill-diffs)
      (extrapolate-backwards
       (append (reverse (map first diffs))
               (list (first lst)))))))

(define (part-2 input)
  (apply + (map guess-previous-number (parse-input input))))


(fprintf (current-output-port)
         "part 2 (sample input): ~a\n"
         (part-2 sample-input))

(fprintf (current-output-port)
         "part 2: ~a\n"
         (part-2 input))
