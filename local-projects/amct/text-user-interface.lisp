;;;; text-user-interface.lisp

(in-package #:amct)


;;; Translations (annotate sentences with the semantic labels)
(defmethod %annotate-sentences ((ui-type (eql :tui))
                                (corpus  list))
  (labels ((make-counter (function)
             (let ((cnt 0))
               (values
                (lambda (&rest args)
                  (let* ((result (apply function args))
                         (fillers (length
                                   (split-sequence #\space result))))
                    (when result
                      (incf cnt fillers)
                      (cons fillers result))))
                (lambda () cnt))))
           (label-sentence (sentence)
             (format t "~&---~%Sentence: '~A'~%" sentence)
             (do* ((pred (prompt "Predicate [       action]")
                         (prompt "Predicate [       action]"))
                   (counter (multiple-value-list (make-counter #'prompt))
                            (multiple-value-list (make-counter #'prompt)))
                   (prompt (car counter)
                           (car counter))
                   acc)
                  ((null pred)
                   (make-instance 'translation-frame
                                  :sentence sentence
                                  :frames (nreverse acc)))
               (push
                (make-instance
                 'slr-frame
                 :pred pred
                 :arg-0 (funcall prompt "Who?      [        agent]")
                 :arg-1 (funcall prompt "What?     [  experiencer]")
                 :arg-pat (funcall prompt "Whom?     [      patient]")
                 :arg-tmp (funcall prompt "When?     [     temporal]")
                 :arg-loc (funcall prompt "Where?    [     location]")
                 :arg-pur (funcall prompt "Why?      [      purpose]")
                 :arg-man (funcall prompt "How?      [       manner]")
                 :arg-deg (funcall prompt "How?      [degree/extent]")
                 :arg-neg (funcall prompt "How?      [     negation]")
                 :arg-mod (funcall prompt "How?      [        modal]")
                 :arg-oth (funcall prompt "How?      [other adv arg]")
                 :fillers (funcall (cadr counter)))
                acc)
               (format t "--- Semantic frame labeled.~%")
               )))
    (mapcar #'label-sentence corpus)))


;;; Framesets (judge translation correctness)
(defmethod %annotate-framesets ((ui-type   (eql :tui))
                                (reference list)
                                (machine   list))
  (labels ((match-frames (ref mt)
             ;;(format t "Sentence (original) : '~A'~%" ?)
             (format t "~%---~%Sentence (reference): '~A'~%"
                     (sentence ref))
             (format t "Sentence (machine)  : '~A'~%" (sentence mt))
             (do ((predicates
                   (frames ref)
                   (cdr predicates))
                  (options
                   (frames mt)
                   (remove choice options
                           :key #'pred
                           :test #'equal
                           :count 1))
                  acc choice)
                 ((null predicates) (nreverse acc))
               (format t "---~%")
               (setf choice
                     (menu (format nil "--- Select best match for '~A'"
                                   (pred (car predicates)))
                           (append (mapcar #'pred options)
                                   '(:no-match))))
               (match-labels (car predicates)
                             (find choice options
                                   :key  #'pred
                                   :test #'equal)))))
    (mapcar #'match-frames reference machine)))

(defun match-labels (ref mt)
  "Textual match-label executor"
  (if (null mt)                        ; nil is :no-match
      nil
      (labels ((match-label (slr ref mt)
                 (do ((response
                       (when (or (null ref)
                                 (null mt))
                         :no-match)))
                     ((typep response 'label-match)
                      response)
                   (setf response
                         (progn
                           (format t "~16A | ~16A | ~16A | (y, n, p): "
                                   slr ref mt)
                           (finish-output)
                           (switch ((read-line) :test #'equal)
                             ("y" :correct)
                             ("n" :incorrect)
                             ("p" :partial)))))))

        (setf (matched?  mt)   t
              (correct   mt)   0
              (partial   mt)   0
              (incorrect mt)   0
              (no-match  mt)   0
              (fillers-ref mt) (fillers ref))

        (map nil (lambda (arg)
                   (let ((ref-arg (slot-value ref arg))
                         (mt-arg  (slot-value mt  arg)))
                     (case (match-label arg (cdr ref-arg) (cdr mt-arg))
                       (:correct   (incf (correct mt)
                                         (car mt-arg)))
                       (:partial   (incf (partial mt)
                                         (car mt-arg)))
                       (:incorrect (incf (incorrect mt)))
                       (:no-match  (when (slot-value ref arg)
                                     (incf (no-match mt)))))))
             '(#|pred|#
               arg-0 arg-1
               arg-pat arg-tmp arg-loc
               arg-pur arg-man arg-deg
               arg-neg arg-mod arg-oth)))))


;;; Helpers
(defun show (translations)
  (mapcar (lambda (tr)
            (format t "--~%Sentence: '~A'~%" (sentence tr))
            (dolist (frame (frames tr))
              (let ((matched? (matched? frame)))
                (format t "-- '~A' (~A) matched? ~A~%"
                        (pred frame) (fillers frame) matched?)
                (when matched?
                  (format t "C: ~A, P: ~A, M: ~A, N: ~A~%"
                          (correct   frame)
                          (partial   frame)
                          (incorrect frame)
                          (no-match  frame)
                          )))))
          translations))

(defun load-raw-corpus ()
  (when-let ((path (prompt "Path to the file" "data/ref.txt"))
             (name (symbolicate
                    (string-upcase
                     (prompt "Name of the corpus")))))
    (setf (gethash name *corpuses*)
          (make-instance
           'corpus
           :name name
           :sentences (read-file-to-lines-list path)))))

(defun toplevel ()  
  (do ((form "Welcome to HMEANT toolkit.
Type :help for help."
             (progn
               (princ "HMEANT> ")
               (finish-output)
               (read))))
      ((eql form :quit) T)
    (case form
      (:help (format t "Available options:

:lc – load raw corpus with translations from file
:an – annotate corpus

:la – load annotated corpus from file [not implemented yet]
:sa – save annotated corpus to file   [not implemented yet]

:cf – compare frames from REF and MT annotated corpuses
:cm – compute HMEANT metric

:help – help menu (this menu!)
:quit – exit the program~%~%"))
      (:lc (load-raw-corpus))
      (:an (annotate-sentences
            (menu "Select corpus to annotate"
                  (hash-table-keys *corpuses*))))

      (:cf (annotate-framesets
            (menu "Select reference translation corpus"
                  (hash-table-keys *corpuses*))
            (menu "Select machine translation corpus"
                  (hash-table-keys *corpuses*))))
      (:cm (format t "Score is ~A~%"
            (compute-hmeant
             (gethash
              (menu "Select aligned corpus to compute"
                    (hash-table-keys *corpuses*))
              *corpuses*))))
      (otherwise (princ (eval form))
                 (terpri)))))
