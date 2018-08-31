(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :dufy)
  (asdf:load-system :mid))

(defparameter *current-path* *load-pathname*)

(defparameter *task-lst*
  `(("srgb-d65" . ,dufy:+srgb+)
    ("adobergb-d65" . ,dufy:+adobe+)))

(defun do-task (basename rgbspace)
  (let* ((filename (concatenate 'string basename ".dat"))
	 (path (merge-pathnames filename *current-path*)))
    (format t "Now generating ~A...~%" filename)
    (mid:save-munsell-inversion-data
     (mid:make-munsell-inversion-data rgbspace t)
     path)))

#-swank
(dolist (task *task-lst*)
  (do-task (car task) (cdr task)))


;; (defparameter mid (mid:load-munsell-inversion-data (merge-pathnames "srgb-d65.dat" current-path)))
