(defpackage console
  (:use :common-lisp :sb-alien)
  (:shadow :common-lisp format)
  (:export with-raw-mode
           clear
           move
           set-pos
           format
           newline
           formatln
           style))
(in-package :console)

;;;;;;;;;
;;; types
(deftype direction () '(member :up :down :left :right)) 
(deftype color () '(member :black :red :green :yellow :blue :magenta :cyan :white :normal))


;;;;;;;;;;;;;
;;; constants
(defparameter +ESC+ (common-lisp:format nil "~c[" (code-char #8r33)))
(defparameter +STDIN_FD+ (sb-sys:fd-stream-fd sb-sys:*stdin*))


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
  `(progn (common-lisp:format t ,control-string ,@format-arguments)
          (force-output)))

(defmacro formatln (control-string &rest format-arguments)
  `(progn (format ,control-string ,@format-arguments)
          (newline)))

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
  (when (plusp delta)
    (format "~a~d~a" +ESC+ delta
            (ecase direction
              (:up    "A")
              (:down  "B")
              (:left  "D")
              (:right "C")))))

(defun clear (&key line)
  (if line
      (format "~a2K" +ESC+)
    (format "~a2J" +ESC+)))

(defun set-pos (x y)
  (format "~a~d;~dH" +ESC+ y x))

(defmacro with-raw-mode (&body body)
  (let ((old (gensym)))
    `(locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((,old (sb-posix:tcgetattr +STDIN_FD+)))
        (unwind-protect
            (locally 
             (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
             (sb-posix:tcsetattr +STDIN_FD+ sb-posix:tcsadrain (cfmakeraw))
             ,@body)
          (sb-posix:tcsetattr +STDIN_FD+ sb-posix:tcsanow ,old))))))
