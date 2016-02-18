;;;; amct.lisp

(in-package #:amct)

(defun entry-point ()
  (start-user-interface)
  (cl-user::quit))

#+sbcl
(defun build-executable ()
  (sb-ext:save-lisp-and-die "hmeant" :toplevel #'entry-point :executable t))
