;;;; wizard-special-actions.lisp (http://landoflisp.com/source.html)
;;;; Adds special actions to "Wizard's Game" (wizards-game.lisp).

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
      (if (and (eq *location* ',place)
          (eq subject ',subj)
          (eq object ',obj)
          (have ',subj))
        ,@body
        '(i cant ,command like that.)))
    (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
    (progn (setf *chain-welded* 't)
      '(The chain is now securely welded to the bucket.))
    '(You do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
    (progn (setf *bucket-filled* 't)
      '(The bucket is now full of water.))
    '(The water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(The bucket has nothing in it.))
    ((have 'frog) '(The wizard awakens and sees that you stole his frog. He is
        so upset he banishes you to the netherworlds. Game over!))
    (t '(The wizard awakens from his slumber and greets you warmly. He hands
        you the magic low-carb donut. You win!))))
