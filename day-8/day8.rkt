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

(define (walk instructions nodes node [ip 0] [steps 0])
  (if (string=? node "ZZZ")
      steps
      (let* {[inst (string-ref instructions ip)]
             [paths (cadr (assoc node nodes))]
             [next-node (cond
                          [(char=? #\L inst) (first paths)]
                          [(char=? #\R inst) (second paths)])]}
        (walk instructions nodes next-node
              (remainder (add1 ip) (string-length instructions))
              (add1 steps)))))

(define (part-1 input)
  (let-values {[(instructions nodes) (parse-input input)]}
    (walk instructions nodes "AAA")))

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
