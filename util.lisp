(in-package :mine)

(defun shuffle (list &aux (ary (coerce list 'vector)) (size (length ary)))
  (loop REPEAT 2 DO
    (loop FOR i FROM 0 BELOW size DO
      (rotatef (aref ary i) (aref ary (random size)))))
  (coerce ary 'list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; "/dir/file.ext" -> "file.ext"
  (defun basename (pathstring)
    (let ((path (parse-namestring pathstring)))
      (format nil "~A~@[.~A~]" (pathname-name path) (pathname-type path))))

  ;; '(a b c &optional c &key (d e)) -> '(a b c d)
  (defun collect-varsym (args)
    (mapcar (lambda (a)
	      (if (consp a) (car a) a))
	    (remove-if (lambda (a)
			 (and (symbolp a) (string= "&" a :end2 1)))
		       args))))

(defmacro defmain (fn-name args &body body)
  (let ((usage nil))
    ;; If first expression of body is string type, it treated as command documentation
    (when (stringp (car body))
      (setf usage (car body)
	    body  (cdr body)))
    
    `(defun ,fn-name ()
       ;; Need to override *invoke-debugger-hook*
       (let ((sb-ext:*invoke-debugger-hook*
	      (lambda (condition hook)
		(declare (ignore hook))
		(format *error-output* "Error: ~A~%" condition)
		(sb-ext:quit :unix-status 1))))
         
	 ;; When failed arguments destructuring, show documentation and exit
	 ,(when usage
	    `(handler-case 
	      (destructuring-bind ,args (cdr sb-ext:*posix-argv*) 
	        (declare (ignore ,@(collect-varsym args))))
	      (error ()
	        (format *error-output* "~&~?~%~%" 
			,usage
			(list (basename (car sb-ext:*posix-argv*))))
		(sb-ext:quit :unix-status 1))))

         (destructuring-bind ,args (cdr sb-ext:*posix-argv*)
           ,@body
	   (sb-ext:quit :unix-status 0))))))
