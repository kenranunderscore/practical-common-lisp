(defun primep (n)
  "Check whether a number is a prime."
  (when (> n 1)
    (loop for fac from 2 to (isqrt n) never (zerop (mod n fac)))))

(defun next-prime (n)
  (loop for i from n when (primep i) return i))

(defmacro with-gensyms ((&rest names) &body body)
  "Create gensyms for the names passed to with-gensyms."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  "Loop body over all primes between start and end."
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,end))
       ,@body)))

(do-primes (p 0 19)
  (format t "~d " p)
  (format t "~%"))

(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

;; No idea what this does yet.
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
