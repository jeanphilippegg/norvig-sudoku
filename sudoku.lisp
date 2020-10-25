(defun copy-hash-table (hash-table)
  (let ((new-hash-table (make-hash-table :test (hash-table-test hash-table)
                                         :size (hash-table-size hash-table))))
    (maphash (lambda (key value) (setf (gethash key new-hash-table) value)) hash-table)
    new-hash-table))

(defun hash-table-every (predicate hash-table)
  (loop for v being the hash-value in hash-table
        always (funcall predicate v)))

(defun string-cartesian-product (sA sB)
  "Cartesian product of strings sA and sB."
  (loop for cA across sA
     append (loop for cB across sB
               collect (concatenate 'string (string cA) (string cB)))))

(defvar *digits* "123456789")
(defvar *rows* "ABCDEFGHI")
(defvar *cols* *digits*)
(defvar *squares* (string-cartesian-product *rows* *cols*))
(defvar *unitlist* (append (loop for c across *cols*
                                 collect (string-cartesian-product *rows* (string c)))
                           (loop for r across *rows*
                                 collect (string-cartesian-product (string r) *cols*))
                           (loop for r in '("ABC" "DEF" "GHI")
                                 append (loop for c in '("123" "456" "789")
                                              collect (string-cartesian-product r c)))))

(defun unitlist-of-square (square)
  (loop for unit in *unitlist* when (member square unit :test #'string=) collect unit))

(defvar *units* (let ((hash-table (make-hash-table :test #'equal)))
                  (loop for s in *squares*
                        do (setf (gethash s hash-table) (unitlist-of-square s)))
                  hash-table))

(defun get-peers (units square)
  (remove-duplicates (loop for peer in (apply #'append units) when (not (string= peer square)) collect peer)
                     :test #'equal))

(defvar *peers* (let ((hash-table (make-hash-table :test #'equal)))
                  (loop for s in *squares*
                        do (setf (gethash s hash-table) (get-peers (gethash s *units*) s)))
                  hash-table))


(defun parse-grid (grid)
  "Convert grid to a hash-table of (square possible-values) or return nil
   if contradiction."
  (let ((values (make-hash-table :test #'equal)))
    (loop for s in *squares* do (setf (gethash s values) *digits*))
    (dolist (grid-value (grid-values grid))
      (let ((s (car grid-value))
            (d (cdr grid-value)))
        (if (and (find d *digits*) (not (assign values s d)))
            (return-from parse-grid nil))))
    values))

(defun grid-values (grid)
  "Convert grid into an alist of (square char)."
  (let ((chars (remove-if-not (lambda (x) (or (find x *digits*) (find x "0."))) grid)))
    (pairlis *squares* (coerce chars 'list))))

(defun assign (values s d)
  "Eliminate all other values (except d) from values. Values is a hash-table of
   (square possible-values)."
  (let ((other-values (remove-if (lambda (x) (char= d x)) (gethash s values))))
    (if (every (lambda (x) (eliminate values s x)) other-values)
        values
        nil)))

(defun eliminate (values s d)
  "Eliminate d from values. Values is a hash-table of (square possible-values).
   Return values or nil if contradiction. Propagate in these cases:
   Case 1: When a square has only one value left, eliminate this value
           from the peers of the square.
   Case 2: Check each unit of the square. If, in a unit, there is only
           one place for d, then assign d to that place (square)."
  (if (notany (lambda (x) (char= d x)) (gethash s values))
      (return-from eliminate values))
  (setf (gethash s values) (remove-if (lambda (x) (char= d x)) (gethash s values)))
  ;; Case 1
  (case (length (gethash s values))
    (0 (return-from eliminate nil))
    (1 (let ((s2 (gethash s *peers*)))
         (if (notevery (lambda (x) (eliminate values x (coerce (gethash s values) 'character))) s2)
             (return-from eliminate nil)))))
  ;; Case 2
  (dolist (u (gethash s *units*))
    (let ((dplaces (remove-if-not (lambda (x) (find d (gethash x values))) u)))
      (case (length dplaces)
        (0 (return-from eliminate nil))
        (1 (if (not (assign values (first dplaces) d))
               (return-from eliminate nil))))))
  values)

(defun display (values)
  "Display values in a 2D grid."
  (let* ((width (1+ (loop for v being the hash-values in values maximizing (length v))))
         (line (let ((subline (make-string (* 3 width) :initial-element #\-)))
                 (concatenate 'string subline "+" subline "+" subline))))
    (dotimes (i (length *rows*))
      (dotimes (j (length *cols*))
        (let* ((square (concatenate 'string (string (elt *rows* i)) (string (elt *cols* j))))
               (square-value (gethash square values)))
          (write-string
           (concatenate 'string
                        (make-string (- width (length square-value)) :initial-element #\Space)
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
        ((hash-table-every (lambda (x) (= 1 (length x))) values) values)
        (t (let ((min-square "A1")
                 (min-square-values "xxxxxxxxxx"))
             (loop for k being the hash-keys in values using (hash-value v)
                   do (when (and (> (length v) 1) (< (length v) (length min-square-values)))
                        (setq min-square k)
                        (setq min-square-values v)))
             (some #'identity (map 'list
                                   (lambda (d) (search-solution (assign (copy-hash-table values)
                                                                        min-square
                                                                        d)))
                                   min-square-values))))))
