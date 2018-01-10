(in-package :cl-user)

(require :dufy)
(require :dufy-tools)

(defparameter current-path *load-pathname*)
(defparameter srgbd50 (dufy:new-rgbspace 0.64d0 0.33d0 0.3d0 0.6d0 0.15d0 0.06d0
					 :illuminant dufy:illum-d50))
(defparameter adobed50 (dufy:new-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
					  :illuminant dufy:illum-d50))

(defparameter task-lst
  (list (cons "srgb-d65" dufy:srgb)
	(cons "srgb-d50" srgbd50)
	(cons "adobergb-d65" dufy:adobe)
	(cons "adobergb-d50" adobed50)))

(defun do-task (basename rgbspace)
  (let* ((filename (concatenate 'string basename ".dat"))
	 (path (merge-pathnames filename current-path)))
    (format t "Now generating ~A...~%" filename)
    (dufy-tools:save-munsell-inversion-data
     (dufy-tools::make-munsell-inversion-data rgbspace t)
     path)))
  
;; (dolist (task task-lst)
;;   (do-task (car task) (cdr task))
;;   ))

