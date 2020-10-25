(defun string-cartesian-product (sA sB)
  "Cartesian product of strings sA and sB."
  (loop for cA across sA
     append (loop for cB across sB
               collect (concatenate 'string (string cA) (string cB)))))

(defun get-peers (units square)
  (loop for peer in (apply #'append units) when (not (string= peer square)) collect peer))


(defvar *digits* "123456789")
(defvar *rows* "ABCDEFGHI")
(defvar *cols* *digits*)
(defvar *squares* (string-cartesian-product *rows* *cols*))
(defvar *unitlist*
  (append (loop for c across *cols*
                collect (string-cartesian-product *rows* (string c)))
          (loop for r across *rows*
                collect (string-cartesian-product (string r) *cols*))
          (loop for r in '("ABC" "DEF" "GHI")
                append (loop for c in '("123" "456" "789")
                             collect (string-cartesian-product r c)))))

(defun unitlist-of-square (square)
  (loop for unit in *unitlist* when (member square unit :test #'string=) collect unit))

(defvar *units*
  (mapcar (lambda (x) (cons x (unitlist-of-square x))) *squares*))
(defvar *peers*
  (mapcar (lambda (x) (cons (car x) (get-peers (cdr x) (car x)))) *units*))


(defun parse-grid (grid)
  "Convert grid to an alist of (square possible-values) or return nil
   if contradiction."
  (let ((values (mapcar (lambda (x) (cons x *digits*)) *squares*)))
    (dolist (grid-value (grid-values grid))
      (let ((s (car grid-value))
            (d (cdr grid-value)))
        (if (and (find d *digits*) (not (assign values s d)))
            (return-from parse-grid nil))))
    values))

(defun grid-values (grid)
  "Convert grid into an alist of (square char)."
  (let ((chars (remove-if-not
                (lambda (x)
                  (find x (concatenate 'string "0." *digits*)))
                grid)))
    (pairlis *squares* (coerce chars 'list))))

(defun assign (values s d)
  "Eliminate all other values (except d) from values. Values is an alist of
   (square possible-values)."
  (let ((other-values (remove-if
                       (lambda (x) (char= d x))
                       (cdr (assoc s values :test #'string=)))))
    (if (every (lambda (x) (eliminate values s x)) other-values)
        values
        nil)))

(defun eliminate (values s d)
  "Eliminate d from values. Values is an alist of (square possible-values).
   Return values or nil if contradiction. Propagate in these cases:
   Case 1: When a square has only one value left, eliminate this value
           from the peers of the square.
   Case 2: Check each unit of the square. If, in a unit, there is only
           one place for d, then assign d to that place (square)."
  (let ((values-s (assoc s values :test #'string=)))
    (if (notany (lambda (x) (char= d x)) (cdr values-s))
        (return-from eliminate values))
    (setf (cdr values-s)
          (remove-if (lambda (x) (char= d x)) (cdr values-s)))
    ;; Case 1
    (case (length (cdr values-s))
      (0 (return-from eliminate nil))
      (1 (let ((s2 (cdr (assoc s *peers* :test #'string=))))
           (if (notevery (lambda (x) (eliminate values x (coerce (cdr values-s) 'character)))
                         s2)
               (return-from eliminate nil)))))
    ;; Case 2
    (dolist (u (cdr (assoc s *units* :test #'string=)))
      (let ((dplaces
             (remove-if-not
              (lambda (x)
                (find d (cdr (assoc x values :test #'string=))))
              u)))
        (case (length dplaces)
          (0 (return-from eliminate nil))
          (1 (if (not (assign values (first dplaces) d))
                 (return-from eliminate nil))))))
    values))

(defun display (values)
  "Display values in a 2D grid."
  (let* ((width (1+ (reduce #'max (mapcar #'cdr values) :key #'length)))
         (line (let ((subline (make-string (* 3 width) :initial-element #\-)))
                 (concatenate 'string subline "+" subline "+" subline))))
    (dotimes (i (length *rows*))
      (dotimes (j (length *cols*))
        (let* ((square (concatenate 'string
                                    (string (elt *rows* i))
                                    (string (elt *cols* j))))
               (square-value (cdr (assoc square values :test #'string=))))
          (write-string
           (concatenate 'string
                        (make-string (- width (length square-value))
                                     :initial-element #\Space)
                        square-value))
          (cond ((or (= j 2) (= j 5)) (write-string "|"))
                ((= j 8) (write-line "")))))
      (if (or (= i 2) (= i 5))
          (write-line line)))))

(defun solve (grid)
  "Solve a grid given as a string of 81 numbers between 1 and 9,
   or 0 or . for empty squares."
  (search-solution (parse-grid grid)))

(defun search-solution (values)
  "Find the square with the fewest possibilities and try them all."
  (cond ((not values) nil)
        ((every (lambda (x) (= 1 (length (cdr x)))) values) values)
        (t (let ((min-square (reduce (lambda (x y) (if (< (length (cdr x)) (length (cdr y))) x y))
                                     (remove-if-not (lambda (x) (> (length (cdr x)) 1)) values))))
             (some #'identity (map 'list (lambda (d) (search-solution (assign (copy-alist values)
                                                                              (car min-square)
                                                                              d)))
                                   (cdr min-square)))))))
