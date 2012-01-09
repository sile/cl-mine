(defpackage game
  (:use :common-lisp)
  (:import-from :mine shuffle)
  (:export init-game
           board-width
           board-height
           locate-bombs
           open-cell
           flip-flag
           each
           finish?
           bomb-count
           flag-count))
(in-package :game)

;;;;;;;;;
;;; types
(deftype cell-state () '(member :mask :flag :open))


;;;;;;;;;;;
;;; structs
(defstruct game
  (board       #2A() :type array)
  (cell-states #2A() :type (array cell-state)))


;;;;;;;;;;;;;;;;;;;;;;
;;; internal functions
(defun surrounding-cells (game x y)
  (destructuring-bind (height width) 
                      (array-dimensions (game-board game))
    (loop FOR y~ FROM (max 0 (1- y)) TO (min (1- height) (1+ y))
          APPEND
          (loop FOR x~ FROM (max 0 (1- x)) TO (min (1- width) (1+ x))
                UNLESS (and (= x~ x) (= y~ y))
                COLLECT (list x~ y~)))))

(defun board-size (game)
  (array-total-size (game-board game)))

(defmacro cell (board x y)
  `(aref ,board ,y ,x))

;;;;;;;;;;;;;;;;;;;;;;
;;; exported functions
(defun init-game (width height)
  (make-game :board (make-array `(,height ,width) 
                                :initial-element nil)
             :cell-states (make-array `(,height ,width) 
                                      :element-type 'cell-state
                                      :initial-element :mask)))

(defun board-width (game)
  (second (array-dimensions (game-board game))))

(defun board-height (game)
  (first (array-dimensions (game-board game))))

(defmacro each ((cell state &key (x (gensym)) (y (gensym)) result-form eol-form) game &body body)
  (let ((height (gensym))
        (width (gensym))
        (g (gensym)))
    `(let ((,g ,game))
       (destructuring-bind (,height ,width) (array-dimensions (game-board ,g))
         (loop FOR ,y FROM 0 BELOW ,height DO
           (locally
            (loop FOR ,x FROM 0 BELOW ,width DO
              (symbol-macrolet ((,cell (cell (game-board ,g) ,x ,y))
                                (,state (cell (game-cell-states ,g) ,x ,y)))
                               ,@body))
            ,eol-form)))
       ,result-form)))

(defun locate-bombs (game bomb-count init-x init-y)
  (assert (< 0 bomb-count (- (board-size game) 9)))

  (let* ((excludes `((,init-x ,init-y) . ,(surrounding-cells game init-x init-y)))
         (bombs (subseq (shuffle (loop FOR x FROM 0 BELOW (board-width game)
                                       APPEND
                                       (loop FOR y FROM 0 BELOW (board-height game)
                                             FOR pos = `(,x ,y)
                                             UNLESS (find pos excludes :test #'equal)
                                             COLLECT pos)))
                        0 bomb-count)))
    (loop FOR (x y) IN bombs DO
      (setf (cell (game-board game) x y) :bomb)))
    
  (each (cell state :x x :y y) game
    (unless (eq cell :bomb)
      (setf cell (count :bomb (surrounding-cells game x y) 
                        :key (lambda (pos) 
                               (cell (game-board game) (first pos) (second pos)))))))
  (open-cell game init-x init-y))

(defun open-surrounding-cells (game x y)
  (let ((bomb-num (cell (game-board game) x y))
        (flag-num (loop FOR (x~ y~) IN (surrounding-cells game x y)
                        WHEN (eq (cell (game-cell-states game) x~ y~) :flag)
                        SUM 1)))
    (when (= bomb-num flag-num)
      (loop FOR (x~ y~) IN (surrounding-cells game x y)
            DO (open-cell game x~ y~ nil)))))
  
(defun open-cell (game x y &optional (open-surrounding-cells t))
  (ecase (cell (game-cell-states game) x y)
    (:flag 'ignore)
    (:open (when (and (not (has-bomb? game x y))
                      open-surrounding-cells)
             (open-surrounding-cells game x y)))
    (:mask 
     (setf (cell (game-cell-states game) x y) :open)
     (when (and (not (has-bomb? game x y))
                (zerop (cell (game-board game) x y)))
       (loop FOR (x~ y~) IN (surrounding-cells game x y)
             DO (open-cell game x~ y~ nil))))))
        
(defun flip-flag (game x y)
  (ecase #1=(cell (game-cell-states game) x y)
    (:open 'ignore)
    (:flag (setf #1# :mask))
    (:mask (setf #1# :flag))))

(defun has-bomb? (game x y)
  (eq (cell (game-board game) x y) :bomb))

(defun bomb-count (game &aux (count 0))
  (each (cell state :result-form count) game
    (when (eq cell :bomb)
      (incf count))))

(defun flag-count (game &aux (count 0))
  (each (cell state :result-form count) game
    (when (eq state :flag)
      (incf count))))

(defun finish? (game)
  (each (cell state) game
    (when (and (eq cell :bomb)
               (eq state :open))
      (return-from finish? (values t nil))))
    
  (each (cell state) game
    (when (eq state :mask)
      (return-from finish? (values nil nil))))
  
  (when (/= (bomb-count game) (flag-count game))
    (return-from finish? (values nil nil)))

  (values t t))
