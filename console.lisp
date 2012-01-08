(defpackage console
  (:use :common-lisp :sb-alien)
  (:shadow :common-lisp format)
  (:export with-raw-mode
           clear
           move
           set-pos
           format
           newline
           style))
(in-package :console)

;;;;;;;;;
;;; types
(deftype direction () '(member :up :down :left :right)) 
(deftype color () '(member :black :red :green :yellow :blue :magenta :cyan :white :normal))


;;;;;;;;;;;;;
;;; constants
(defparameter +ESC+ (common-lisp:format nil "~c[" (code-char #8r33)))
(defparameter +STDIN+ sb-sys:*stdin*)
(defparameter +STDOUT+ sb-sys:*stdout*)
(defparameter +STDIN_FD+ (sb-sys:fd-stream-fd +STDIN+))


;;;;;;;;;;;;;;;;;;;;;;
;;; internal functions
(defun color-code (color)
  (declare (color color))
  (ecase color 
    (:black   30)
    (:red     31)
    (:green   32)
    (:yellow  33)
    (:blue    34)
    (:magenta 35)
    (:cyan    36)
    (:white   37)
    (:normal  39)))

(define-alien-routine ("cfmakeraw" %cfmakeraw) void (termios* (* t)))

(defun cfmakeraw ()
  (let ((termios (sb-posix::allocate-alien-termios)))
    (%cfmakeraw termios)
    (unwind-protect
        (sb-posix::alien-to-termios termios)
      (sb-posix::free-alien-termios termios))))


;;;;;;;;;;;;;;;;;;;;;;
;;; exported functions
(defmacro format (control-string &rest format-arguments)
  `(common-lisp:format +STDOUT+ ,control-string ,@format-arguments))

(defun newline ()
  (format "~c~c" #\Newline #\Return))

(defun style (x &key (color :normal) (bgcolor :normal) bold inverse underline)
  (declare (color color bgcolor))
  (common-lisp:format nil "~a~{~d;~}~d;~dm~a~a0m"
    +ESC+
    (remove nil (list (and bold 1) (and underline 4) (and inverse 7)))
    (color-code color)
    (+ (color-code bgcolor) 10)
    x
    +ESC+))

(defun move (direction &optional (delta 1))
  (declare (direction direction))
  (format "~a~d~a" +ESC+ delta
          (ecase direction
            (:up    "A")
            (:down  "B")
            (:left  "C")
            (:right "D"))))

(defun clear ()
  (format "~a2J" +ESC+))

(defun set-pos (x y)
  (format "~s~d;~dH" +ESC+ x y))

(defmacro with-raw-mode (&body body)
  (let ((old (gensym)))
    `(let ((,old (sb-posix:tcgetattr +STDIN_FD+)))
       (sb-posix:tcsetattr +STDIN_FD+ sb-posix:tcsanow (cfmakeraw))
       (unwind-protect
           (locally ,@body)
         (sb-posix:tcsetattr +STDIN_FD+ sb-posix:tcsanow ,old)))))
