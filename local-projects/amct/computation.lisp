;;;; computation.lisp

(in-package #:amct)

(defun f1-score (precision recall)
  (* 2 (/ (* precision recall)
          (+ precision recall))))

(defmacro sum-of ((&rest sets) (&rest elts) &body body)
  `(reduce #'+
           (mapcar (lambda (,@elts)
                     ,@body)
                   ,@sets)))

(defun matching-role-labels (tf X C P w-pred w w-part)
  "Helper function to compute PRECISION and RECALL parameters

tf_i – #tokens filled in aligned frame i / total #tokens
X_ij – total #ARG j of aligned frame i (M for MT, R for REF)
C_ij – # correct ARG j of aligned frame i
P_ij – # partially correct ARG j of aligned frame i
w-pred – weight of similarity of predicates
w_j – weight of similarity of ARG j
w-part – weight of the partially correct translated ARG"
  (/ (sum-of (tf X C P) (tf_i xi C_i P_i)
       (* tf_i
          (/
           (+ w-pred
              (sum-of (w C_i P_i) (w_j C_ij P_ij)
                (* w_j (+ C_ij
                         (* w-part P_ij)))))
           (+ w-pred
              (sum-of (w xi) (w_j xij)
                (* w_j xij))))))
     (reduce #'+ tf)))
