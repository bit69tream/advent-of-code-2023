(defparameter sample-input
  "Time:      7  15   30
Distance:  9  40  200
")

(defparameter input
  (uiop:read-file-string "input.txt"))

(defun string-empty-p (str)
  (zerop (length str)))

(defun filter-empty-string (lst)
  (remove-if #'string-empty-p lst))

(defun parse-input (input)
  (let* ((lines (uiop:split-string input :separator '(#\Newline)))
         (time (mapcar #'parse-integer
                       (cdr (filter-empty-string (uiop:split-string (car lines))))))
         (distance (mapcar #'parse-integer
                           (cdr (filter-empty-string (uiop:split-string (cadr lines)))))))
    (assert (eq (length time)
                (length distance)))
    (mapcar #'list
            time distance)))

(defun winning-moves (time distance)
  (loop :for ti :from 0 :to time
        :if (> (* (- time ti) ti)
               distance)
          :collect ti))

(defun part-1 (input)
  (apply #'*
         (mapcar (lambda (race)
                   (length (apply #'winning-moves race)))
                 (parse-input input))))

(format t "part 1 (sample input): ~a~%" (part-1 sample-input))
(format t "part 1: ~a~%" (part-1 input))

(defun parse-input-2 (input)
  (let* ((lines (uiop:split-string input :separator '(#\Newline)))
         (time (parse-integer
                (apply #'concatenate 'string
                       (cdr (filter-empty-string (uiop:split-string (car lines)))))))
         (distance (parse-integer
                    (apply #'concatenate 'string
                           (cdr (filter-empty-string (uiop:split-string (cadr lines))))))))
    (list time distance)))

(defun part-2 (input)
  (length (apply #'winning-moves (parse-input-2 input))))


(format t "part 2 (sample input): ~a~%" (part-2 sample-input))
(format t "part 2: ~a~%" (part-2 input))
