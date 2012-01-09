(in-package :mine)

(defconstant +TOP+ 2)
(defconstant +LEFT+ 2)

;;;;;;;;;;;
;;; structs
(defstruct state
  (pos-x 0 :type fixnum)
  (pos-y 0 :type fixnum)
  (beg-time (get-internal-real-time) :type fixnum)
  (bomb-count 0 :type fixnum))

;;;;;;;;;;;;;;;;;;;;;;
;;; internal functions
(defun show-board (game)
  (console:set-pos +LEFT+ +TOP+)
  
  (game:each (cell state :eol-form (progn (console:newline)
                                          (console:move :right (1- +LEFT+)))) game
    (console:format "~a "
                    (case state
                          (:mask "#")
                          (:flag (console:style "!" :color :red :bold t))
                          (:open (case cell
                                   (:bomb     (console:style "*" :color :yellow :bold t))
                                   (0         "_")
                                   (otherwise cell)))))))

(defun elapsed-time (state)
  (let ((secs (round (- (get-internal-real-time) (state-beg-time state)) 
                     internal-time-units-per-second)))
    (format nil "~2,'0d:~2,'0d:~2,'0d" (floor secs (* 60 60))
                                       (floor (mod secs (* 60 60)) 60)
                                       (mod secs 60))))

(defun show-status-bar (game state)
  (let ((bar-top (+ +TOP+ (game:board-height game)))
        (bar-left (- +LEFT+ 1)))
    (console:set-pos bar-left bar-top)
    (console:formatln "~{~a~}-" (loop REPEAT (* 2 (game:board-width game)) COLLECT "-"))

    (console:move :right (1- bar-left))
    (console:format " flag: ~d/~d" (game:flag-count game) (state-bomb-count state))
    (console:formatln "~18T~a" (elapsed-time state))

    (console:move :right (1- bar-left))
    (console:formatln "~{~a~}-" (loop REPEAT (* 2 (game:board-width game)) COLLECT "-"))

    (console:move :right (1- bar-left))
    (console:formatln " up:'e' down:'d' left:'s' right:'f'")
    
    (console:move :right (1- bar-left))
    (console:formatln " open:'j' flag:'k' exit:'c'")))

(defun show-game (game state)
  (show-board game)
  (show-status-bar game state)
  (console:set-pos (+ +LEFT+ (* (state-pos-x state) 2))
                   (+ +TOP+ (state-pos-y state))))

(defun init (width height bomb-count)
  (console:clear)
  (let ((game (game:init-game width height))
        (state (make-state :bomb-count bomb-count)))
    (show-game game state)
    (console:set-pos +LEFT+ +TOP+)
    (values game state)))

(defmacro command-loop ((cmd game state) &body body)
  (let ((recur (gensym)))
    `(block nil
       (labels ((,recur ()
                  (handler-case 
                   (let ((,cmd (case (sb-ext:with-timeout 1 (read-char))
                                 (#\c (return-from ,recur))
                                 (#\s :left)
                                 (#\d :down)
                                 (#\f :right)
                                 (#\e :up)
                                 (#\j :open)
                                 (#\k :flag))))
                     
                     ,@body
                     (,recur))
                   (sb-ext:timeout () ; show elapsed time
                     (show-game ,game ,state)
                     (,recur)))))
       (,recur)))))

(defun go-up (game state)
  (declare (ignore game))
  (when (plusp #1=(state-pos-y state))
    (console:move :up)
    (decf #1#)))

(defun go-down (game state)
  (when (< #1=(state-pos-y state) (1- (game:board-height game)))
    (console:move :down)
    (incf #1#)))

(defun go-left (game state)
  (declare (ignore game))
  (when (plusp #1=(state-pos-x state))
    (console:move :left 2)
    (decf #1#)))

(defun go-right (game state)
  (when (< #1=(state-pos-x state) (1- (game:board-width game)))
    (console:move :right 2)
    (incf #1#)))

(defun game-start (game state)
  (with-slots (bomb-count pos-x pos-y) state
    (game:locate-bombs game bomb-count pos-x pos-y)
    (show-game game state)
    
    (flet ((check-finish ()
             (multiple-value-bind (finish complete) (game:finish? game)
               (when finish
                 (return-from game-start complete)))))
      (command-loop (cmd game state)
        (case cmd
          (:up    (go-up game state))
          (:down  (go-down game state))
          (:left  (go-left game state))
          (:right (go-right game state))
          (:flag 
           (game:flip-flag game pos-x pos-y)
           (show-game game state)
           (check-finish))
          (:open 
           (game:open-cell game pos-x pos-y)
           (show-game game state)
           (check-finish)))))))
      
(defun show-result (complete? game state)
  (unless complete?
    (game:each (cell state) game
      (when (eq cell :bomb)
        (setf state :open))))
  (show-game game state)

  (console:set-pos 1 (+ +TOP+ (game:board-height game) 6))
  (if complete?
      (console:formatln "~a" (console:style "COMPLETE!" :color :green :bold t))
    (console:formatln "~a" (console:style "BOMB!" :color :red :bold t))))
    
(defmain main (width height bomb-count)
  "Usage: mine WIDTH HEIGHT BOMB_COUNT"
  (game (parse-integer width) (parse-integer height) (parse-integer bomb-count)))


;;;;;;;;;;;;;;;;;;;;;;
;;; exported functions
(defun game (width height bomb-count)
  (console:with-raw-mode
   (multiple-value-bind (game state) 
                        (init width height bomb-count)
     (show-result
      (command-loop (cmd game state)
        (case cmd
          (:up    (go-up game state))
          (:down  (go-down game state))
          (:left  (go-left game state))
          (:right (go-right game state))
          (:open 
           (return (game-start game state)))))
      game state)))
  (values))

(defun make-mine-command (&optional (name "mine"))
  (sb-ext:save-lisp-and-die 
   name
   :toplevel #'main
   :executable t))
