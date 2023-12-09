#lang racket

(define sample-input
  '("RL"
    "AAA = (BBB, CCC)"
    "BBB = (DDD, EEE)"
    "CCC = (ZZZ, GGG)"
    "DDD = (DDD, DDD)"
    "EEE = (EEE, EEE)"
    "GGG = (GGG, GGG)"
    "ZZZ = (ZZZ, ZZZ)"))

(define sample-input-2
  '("LR"
    "11A = (11B, XXX)"
    "11B = (XXX, 11Z)"
    "11Z = (11B, XXX)"
    "22A = (22B, XXX)"
    "22B = (22C, 22C)"
    "22C = (22Z, 22Z)"
    "22Z = (22B, 22B)"
    "XXX = (XXX, XXX)"))

(define (parse-node node)
  (let* {[a (string-split node " = ")]
         [name (car a)]
         [paths (cadr a)]}
    (list name
          (string-split
           (string-replace paths
                           #rx"\\(|\\)|,"
                           "")))))

(define (parse-input input)
  (let {[instructions (car input)]
        [nodes (cdr input)]}
    (values instructions
            (map parse-node
                 nodes))))

(define (walk instructions nodes current [ip 0] [steps 0])
  (if (for/and ([n current])
        (string-suffix? n "Z"))
      steps
      (let* {[inst (string-ref instructions ip)]
             [next (cond
                     [(char=? #\L inst) first]
                     [(char=? #\R inst) second])]
             [next-nodes (map (lambda (n)
                                (next (cadr (assoc n nodes))))
                              current)]}
        (walk instructions nodes next-nodes
              (remainder (add1 ip) (string-length instructions))
              (add1 steps)))))

(define (walk-smarter instructions nodes current
                      [ip 0] [steps 0] [steps-cache (make-hash)])
  (for {[n (in-list current)]}
    (when (string-suffix? n "Z")
      (if (hash-has-key? steps-cache n)
          (when (negative? (hash-ref steps-cache n))
            (hash-set! steps-cache n (+ steps (hash-ref steps-cache n))))
          (hash-set! steps-cache n (- steps)))))
  (if (and (not (hash-empty? steps-cache))
           (for/and {[[k v] steps-cache]} (positive? v)))
      (apply lcm (hash-values steps-cache))
      (let* {[inst (string-ref instructions ip)]
             [next (cond
                     [(char=? #\L inst) first]
                     [(char=? #\R inst) second])]
             [next-nodes (map (lambda (n)
                                (next (cadr (assoc n nodes))))
                              current)]}
        (walk-smarter
         instructions nodes next-nodes
         (remainder (add1 ip) (string-length instructions))
         (add1 steps)
         steps-cache))))

(define (part-1 input)
  (let-values {[[instructions nodes] (parse-input input)]}
    (walk instructions nodes '("AAA"))))

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

(define (part-2 input)
  (let-values {[[instructions nodes] (parse-input input)]}
    (walk-smarter
     instructions nodes
     (filter (lambda (x)
               (string-suffix? x "A"))
             (map car nodes)))))

(fprintf (current-output-port)
         "part 2 (sample input): ~a\n"
         (part-2 sample-input-2))

(fprintf (current-output-port)
         "part 2: ~a\n"
         (part-2 input))
