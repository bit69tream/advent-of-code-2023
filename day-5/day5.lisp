(defparameter sample-input
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defun split-by (lst by)
  (let ((result (list)))
    (loop :with sublist
          :for el :in lst
          :if (equal el by)
            :do (push (nreverse sublist) result)
            :and :do (setf sublist nil)
          :else
            :do (push el sublist)
          :finally (when sublist
                     (push (nreverse sublist) result)))
    (nreverse result)))

(defun parse-seeds (seeds)
  (mapcar #'parse-integer
          (cdr (uiop:split-string seeds :separator '(#\Space)))))

(defun parse-map (map)
  (destructuring-bind (name &rest ranges) map
    (destructuring-bind (from dont-care to dont-care-2)
        (uiop:split-string name :separator '(#\Space #\-))
      (declare (ignore dont-care dont-care-2))
      (list :from from
            :to to
            :ranges (mapcar (lambda (nums)
                              (mapcar #'parse-integer
                                      (uiop:split-string nums)))
                            ranges)))))

(defun parse-input (input)
  (let* ((lines (uiop:split-string input
                                   :separator '(#\Newline)))
         (groups (split-by lines ""))
         (maps (mapcar #'parse-map (cdr groups)))
         (seeds (parse-seeds (caar groups))))
    (values seeds
            maps)))

(defun value-in-range-p (value range)
  (destructuring-bind (dest src len) range
    (declare (ignore dest))
    (and (>= value src)
         (< value (+ src len)))))

(defun map-value-range (value range)
  (assert (value-in-range-p value range))
  (destructuring-bind (dest src len) range
    (declare (ignore len))
    (+ dest (- value src))))

(defun map-value (value maps from-what)
  (let ((map (find-if (lambda (x)
                        (destructuring-bind (&key (from) &allow-other-keys) x
                          (equal from from-what)))
                      maps)))
    (destructuring-bind (&key (ranges) &allow-other-keys) map
      (loop :for range :in ranges
            :if (value-in-range-p value range)
              :return (map-value-range value range)
            :finally (return value)))))

(defun propagate-seed-through-maps (seed maps)
  (let ((categories (mapcar #'cadr maps))
        (result seed))
    (loop :for category :in categories
          :do (setf result (map-value result maps category)))
    result))

(defun part-1 (input)
  (multiple-value-bind (seeds maps) (parse-input input)
    (apply #'min
           (loop :for seed :in seeds
                 :collect (propagate-seed-through-maps seed maps)))))

(defparameter input (uiop:read-file-string "input.txt"))

(format t "part 1 (sample input): ~a~%" (part-1 sample-input))
(format t "part 1: ~a~%" (part-1 input))

(defun parse-input-2 (input)
  (multiple-value-bind (seeds maps) (parse-input input)
    (assert (evenp (length seeds)))
    (values (loop :for (start len) :on seeds :by #'cddr
                  :collect (cons start len))
            maps)))

(defun part-2 (input)
  (multiple-value-bind (seeds maps) (parse-input-2 input)
    (let ((result most-positive-fixnum))
      (loop :for seed :in seeds
            :do (loop :for current :from (car seed) :upto (+ (car seed)
                                                             (cdr seed))
                      :do (setf result (min result
                                            (propagate-seed-through-maps current maps)))))
      result)))


(format t "part 2 (sample input): ~a~%" (part-2 sample-input))
;; it takes ages to compute yes idc
(format t "part 2: ~a~%" (part-2 input))
