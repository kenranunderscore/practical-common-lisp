(defvar *test-name* nil
  "Used to hold the name of the test that is being executed.")

(defmacro with-gensyms ((&rest names) &body body)
  "Create gensyms for the names passed to with-gensyms."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  "combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ',f ',f))))

(defmacro report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  `(format t "~:[FAIL~;pass~] ... ~a~%" ,result ,form)
  result)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can all other test
   functions or use 'check' to run individual test cases."
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -2) -3)))

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 -5) -15)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))

(if (test-math)
    (format t "ok~%")
    (format t "fail~%"))

(macroexpand '(check (= (+ 1 2) 3)))
