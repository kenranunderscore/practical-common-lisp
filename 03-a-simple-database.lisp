(defvar *db* nil
  "The global record database.")

(defun make-cd (title artist rating ripped)
  "Create a record."
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  "Add a record to the database."
  (push cd *db*))

(defun dump-db ()
  "Print the database in a human-readable fashion."
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  "Read user input after showing some prompt string."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  "Prompt the user for the fields of a CD record."
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]:")))

(defun add-cds ()
  "Keep asking the user to enter CDs."
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:"))
            (return))))

(defun save-db (filename)
  "Save the current database value to a file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  "Load a database value from a file."
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  "Select all records matching a selector predicate."
  (remove-if-not selector-fn *db*))

;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   "Build a selector function for a specific key/value pair."
;;   #'(lambda (cd)
;;       (and
;;        (if title (equal (getf cd :title) title) t)
;;        (if artist (equal (getf cd :artist) artist) t)
;;        (if rating (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "Update all records matching a predicate."
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row)
         *db*)))

; (update (where :artist "Dixie Chicks") :rating 11)

(defun delete-rows (selector-fn)
  "Delete all rows matching a predicate."
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))

(macroexpand-1 '(where :artist "foo" :ripped nil))
