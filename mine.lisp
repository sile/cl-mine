(in-package :mine)

(defun game (width height bomb-count)
  (declare (ignore width height bomb-count))
  #+C
  (console:with-raw-mode
   )
  (values))

