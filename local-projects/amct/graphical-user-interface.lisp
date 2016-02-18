(in-package #:amct)


;;; custom widget modifications

;; list select
(defun list-select-append (lb values)
  (let ((data (append (data lb)
                      (ensure-list values))))
    (setf (data lb) data)))

(defun list-select-delete (lb value)
  (let ((data (remove value (data lb) :test #'equal)))
    (setf (data lb) data)))

(defmethod list-select-display ((select list-select)
                                (item   slr-frame))
  (format nil "~a" (pred item)))

(defmethod list-select-display ((select list-select)
                                (item   corpus))
  (format nil "~A" item))

(defmethod selected ((slist list-select))
  (ensure-car (selected-elements slist)))

(defmethod delete-selected ((slist list-select))
  (when-let ((selected (selected slist)))
    (list-select-delete slist selected)))

(defmethod combobox-get-selection ((l combobox))
  (format-wish "senddata \"([~a current])\"" (widget-path l))
  (ltk::read-data))

;;; helper macros
(defmacro defbutton (name (master &optional label) &body body)
  (let ((label (or label (symbol-name name))))
    `(make-instance 'button :master ,master :text ,label
                    :command (lambda ()
                               ,@body))))

(defun make-label (master text)
  (make-instance 'label :master master  :text text))

(defparameter *slr-arg-symbols*
  '(pred    arg-0   arg-1
    arg-pat arg-tmp arg-loc
    arg-pur arg-man arg-deg
    arg-neg arg-mod arg-oth))

(defparameter *annotation-entries* (make-hash-table)
  "Keeps widgets containing entries (name label-1 label-2)")

(defun arg-value (arg)
  (text (car (gethash arg *annotation-entries*))))

(defun (setf arg-value) (val arg)
  (setf (text (car (gethash arg *annotation-entries*))) val))

(defun init-annotation-widgets (master)
  (mapcar #'(lambda (name label-1 label-2)
              (ensure-gethash name *annotation-entries*)
              (setf (gethash name *annotation-entries*)
                    (list (make-instance 'entry :master master)
                          (make-label master label-1)
                          (make-label master label-2))))
          *slr-arg-symbols*
          (list #t"predicate" #t"agent"    #t"experiencer"
                #t"patient"   #t"temporal" #t"location"
                #t"purpose"   #t"manner"   #t"degree/extent"
                #t"negation"  #t"modal"    #t"other adv arg")
          (list #t"(verb)"    #t"Who?"     #t"What?"
                #t"Whom?"     #t"When?"    #t"Where?"
                #t"Why?"      #t"How? (1)" #t"How? (2)"
                #t"How? (3)"  #t"How? (4)" #t"How? (5)")))


;;; annotation widget
(defun button-add-predicate (frame lb)
  (defbutton add-predicate (frame "Add predicate")
    (if (emptyp (arg-value 'pred))
        (message-box "To add a new predicate you have to fill the current one."
                     "Fill the predicate" "ok" "warning")
        (let ((counter (multiple-value-list (make-counter #'arg-value))))
          (list-select-append lb
                              (make-instance
                               'slr-frame
                               :pred  (arg-value 'pred)
                               :arg-0 (funcall (car counter) 'arg-0)
                               :arg-1 (funcall (car counter) 'arg-1)
                               :arg-pat (funcall (car counter) 'arg-pat)
                               :arg-tmp (funcall (car counter) 'arg-tmp)
                               :arg-loc (funcall (car counter) 'arg-loc)
                               :arg-pur (funcall (car counter) 'arg-pur)
                               :arg-man (funcall (car counter) 'arg-man)
                               :arg-deg (funcall (car counter) 'arg-deg)
                               :arg-neg (funcall (car counter) 'arg-neg)
                               :arg-mod (funcall (car counter) 'arg-mod)
                               :arg-oth (funcall (car counter) 'arg-oth)
                               :fillers (funcall (cadr counter))))
          (clear-annotation-window)))))

(defun button-del-predicate (frame lb)
  (defbutton del-predicate (frame "Delete predicate")
    (delete-selected lb)))

(defun button-sentence-annotated (frame lb tr)
  (defbutton annotate (frame "Sentence annotated")
    (setf (frames tr) (data lb))
    (setf *exit-mainloop* t)))

(defun list-predicates (frame)
  (let ((lb (make-instance 'list-select :master frame)))
    (bind lb "<<ListboxSelect>>"
          (lambda (event)
            (declare (ignore event))
            (when-let ((slr-frame (selected lb)))
              (setf (arg-value 'pred)
                    (slot-value slr-frame 'pred))
              (mapcar #'(lambda (arg)
                          (setf (arg-value arg)
                                (or
                                 (cdr (slot-value slr-frame arg)) "")))
                      (cdr *slr-arg-symbols*))
              (format t "selected frame ~A~%" (selected lb)))))))

(defun clear-annotation-window ()
  (mapcar #'(lambda (arg)
              (setf (arg-value arg) ""))
          *slr-arg-symbols*))

(defun pack-sentence-widget (master text)
  (pack (make-instance 'entry  :master master :text text)
        :fill :x :expand t))

(defun pack-annotate-widget (frame)
  (init-annotation-widgets frame)
  (let ((row 0))
    (mapcar (lambda (name)
              (destructuring-bind (entry label-1 label-2)
                  (gethash name *annotation-entries*)
                (grid label-1 row 0 :padx 10 :sticky "wesn")
                (grid label-2 row 1 :padx 10 :sticky "wesn")
                (grid entry   row 2 :sticky "wesn"))
              (incf row))
            *slr-arg-symbols*)
    (grid-columnconfigure frame 2 "weight" 1)
    (configure frame :borderwidth 1 :relief :solid)))

(defun pack-controls-widget (frame tf)
  (let* ((lb (list-predicates frame))
         (ap (button-add-predicate frame lb))
         (dp (button-del-predicate frame lb))
         (sentence-done (button-sentence-annotated frame lb tf)))
    (setf (data lb) (frames tf))
    (grid lb 0 0 :sticky "wesn" :rowspan 4)
    (grid ap 0 1 :sticky "wens")
    (grid dp 1 1 :sticky "wens")
    (grid sentence-done 3 1 :sticky "wens"))


  (grid (defbutton dummy (frame "")
          (clear-annotation-window))
        2 1 :sticky "wens")

  (grid-columnconfigure frame 0 "weight" 1)
  (configure frame :borderwidth 1 :relief :solid))

(defgeneric annotate-single-sentence (tf)
  (:method ((tf string))
    (let ((tf (make-instance 'translation-frame :sentence tf)))
      (prog1 tf
        (annotate-single-sentence tf))))
  (:method ((tf translation-frame))
    (with-ltk ()
      (let* ((master-f (make-instance 'frame))
             (sentence-f (make-instance 'frame :master master-f))
             (annotate-f (make-instance 'frame :master master-f))
             (controls-f (make-instance 'frame :master master-f)))

        ;; Main frame layout
        (pack master-f   :fill :both :expand t)
        (pack sentence-f :side :top :fill :x :pady 5 :padx 10)
        (pack annotate-f :side :top :fill :x)
        (pack controls-f :side :bottom :fill :x)

        (pack-sentence-widget sentence-f (sentence tf))
        (pack-annotate-widget annotate-f)
        (pack-controls-widget controls-f tf)))))




;;; frame alignment
(defun align-single-frame (sentence m-sentence predicate machine-slr setter)
  (with-ltk ()
    (let* ((frame (make-instance 'frame))
           (ref-l (make-instance
                   'label :master frame
                   :text "Reference translation:"
                   :justify :left))

           (ref-s (make-instance
                   'label :master frame
                   :text (format nil "\"~A\"" sentence)
                   :justify :left))

           (mt-l (make-instance
                  'label :master frame
                  :text "Machine translation:"
                  :justify :left))

           (mt-s (make-instance
                  'label :master frame
                  :text (format nil "\"~A\"" m-sentence)
                  :justify :left))

           (match (make-instance
                   'mw-listbox :master frame
                   :label (format nil "Match predicate \"~A\"." predicate)
                   :key #'(lambda (elt)
                            (if (eql elt :no-match)
                                "[no match]"
                                (pred elt)))
                   ;; :callback
                   ;; #'(lambda (selected)
                   ;;     (when selected
                   ;;       (format t "selected ~A~%" selected)
                   ;;       (funcall setter selected)
                   ;;       (setf *exit-mainloop* t))
                   ;;     )
                   )))
      (setf (data match) (cons :no-match machine-slr))
      (pack frame)
      (grid ref-l 0 0 :sticky "wesn" :padx 15 :pady 3)
      (grid ref-s 1 0 :sticky "wesn" :ipadx 15)
      (grid mt-l  2 0 :sticky "wesn" :padx 15 :pady 3)
      (grid mt-s  3 0 :sticky "wesn" :ipadx 15)

      (grid (defbutton okay (frame "Confirm match")
              (let ((selected (selected match)))
               (if (null selected)
                   (message-box
                    (format nil "Please select match for \"~A\"." predicate)
                    "Fill the predicate" "ok" "warning")
                   (progn
                     (format t "selected ~A~%" selected)
                     (funcall setter selected)
                     (setf *exit-mainloop* t)))))
            4 0)
      
      (grid match 0 1 :rowspan 5)

      (grid-columnconfigure frame 1 "weight" 1)
      (configure frame :borderwidth 1 :relief :solid))))

(defun make-matchbox (master name command)
  (let* ((frame (make-instance 'frame :master master))
         (name (string-downcase (symbol-name name)))
         (radio-1 (make-instance 'radio-button
                                 :text "correct"
                                 :value 1
                                 :variable name
                                 :command command
                                 :master frame))
         (radio-2 (make-instance 'radio-button
                                 :text "partial"
                                 :value 2
                                 :variable name
                                 :command command
                                 :master frame))
         (radio-3 (make-instance 'radio-button
                                 :text "incorrect"
                                 :value 3
                                 :variable name
                                 :command command
                                 :master frame)))
    (prog1 frame
      (grid radio-1 0 0 :sticky "wesn")
      (grid radio-2 0 1 :sticky "wesn")
      (grid radio-3 0 2 :sticky "wesn")
      (configure frame :borderwidth 1 :relief :groove))))

;;; Match labels
(defun match-labels (reference machine)
  (if (symbolp machine)                    ; nil is :no-match
      nil
      (with-ltk ()
        (let* ((row 2)
               (matches nil)
               (frame (make-instance 'frame))
               (confirm (defbutton confirm (frame "Confirm alignment")
                          (setf (matched?  machine)   t
                                (correct   machine)   0
                                (partial   machine)   0
                                (incorrect machine)   0
                                (no-match  machine)   0
                                (fillers-ref machine) (fillers reference))
                          (let ((complete? t))
                            (mapcar #'(lambda (arg)
                                        (let ((mt-arg (slot-value machine arg))
                                              (mmatch (getf matches arg)))
                                          (case mmatch
                                            (1
                                             (incf (correct machine)
                                                   (car mt-arg)))
                                            (2
                                             (incf (partial machine)
                                                   (car mt-arg)))
                                            (3
                                             (incf (incorrect machine)))
                                            (4
                                             (when (slot-value reference arg)
                                               (incf (no-match machine)))))
                                          (unless mmatch
                                            (setf complete? nil))))
                                    (cdr *slr-arg-symbols*))
                            
                            (if complete?
                                (setf *exit-mainloop* t)
                                (progn
                                  (setf (matched? machine) nil)
                                  (message-box
                                   "Please align all arguments."
                                   "Not all arguments aligned" "ok" "warning")
                                  )))
                          )))
          (labels ((slot-to-label (object slot-name)
                     (let ((name (slot-value object slot-name)))
                       (cond ((emptyp name) nil)
                             ((atom name)   name)
                             (T             (cdr name)))))
                   (create-align-frame (name)
                     (let ((l1 (slot-to-label reference name))
                           (l2 (slot-to-label machine name))
                           (command #'(lambda (val)
                                        (format t "Setting ~A to ~A~%" name val)
                                        (setf (getf matches name) val))))
                       (if (and l1 l2)
                           (let* (;; (frame (make-instance 'frame :master frame))
                                  (label   (make-label frame (symbol-name name)))
                                  (entry-1 (make-label frame l1))
                                  (entry-2 (make-label frame l2)))
                             (grid label   row 0 :padx 10 :sticky "wesn")
                             (grid entry-1 row 1 :padx 10 :sticky "wesn")
                             (grid entry-2 row 2 :padx 10 :sticky "wesn")
                             (unless (eql name 'pred)
                               (grid (make-matchbox frame (gensym) command) row 3))
                             ;; (configure frame :borderwidth 1 :relief :solid)
                             (incf row))
                           (funcall command 4)
                           ))))
            (mapcar #'create-align-frame
                    *slr-arg-symbols*))
          (pack frame)
          (grid confirm row 0 :columnspan 4)
          (grid-columnconfigure frame 1 "weight" 1)
          (grid-columnconfigure frame 2 "weight" 1)
          (configure frame :borderwidth 1 :relief :solid)))))

(defmethod %annotate-framesets ((ui-type   (eql :ltk))
                                (reference corpus)
                                (machine   corpus))
  (labels ((match-frames (ref mt)
             (format t "
---
Sentence (reference): '~A'
Sentence (machine)  : '~A'~%" (sentence ref) (sentence mt))

             (do* ((predicates (frames ref)
                               (cdr predicates))
                   (options (frames mt)
                            (remove choice options))
                   (choice nil nil)
                   (setter #'(lambda (v)
                               (setf choice v))))
                  ((null predicates) 'done)
               (align-single-frame (sentence ref)
                                   (sentence mt)
                                   (pred (car predicates))
                                   options
                                   setter)
               (match-labels (car predicates) choice)
               (format t "~A~%" choice))
             (format t "~A ~A~%" ref mt)))
    (setf (reference machine) nil)
    (mapcar #'match-frames
            (annotations reference)
            (annotations machine))
    (setf (reference machine) reference
          (pred-count machine) (cons (predicates-count machine)
                                     (predicates-count reference)))
    (compute-hmeant machine)))


;;; main widget

(defun get-open-file* ()
  (let ((path (get-open-file)))
    (unless (emptyp path)
      path)))

(defun refresh-list (list)
  (setf (data list)
        (hash-table-values *corpuses*)))

(defun load-raw-corpus (file)
  (with-open-file (stream file)
    (do ((sentence (read-line stream)
                   (read-line stream nil nil))
         acc)
        ((null sentence) (nreverse acc))
      (push sentence acc))))

(defun create-lists (master)
  (let* ((reference-corpus nil)
         (frame    (make-instance 'frame :master master))
         (label-0  (make-label frame (format nil "HMEANT toolkit (v. ~A)" #.(get-universal-time))))
         (label-1  (make-label frame "Available corpuses"))
         (label-2  (make-label frame "Reference corpus: "))
         (corpuses (make-instance 'list-select :master frame))
         (button-annotate (defbutton annotate (frame "Annotate")
                            (when-let ((item (selected corpuses)))
                              (annotate-sentences item)
                              (refresh-list corpuses))))
         (button-ref (defbutton set-reference (frame "Set reference corpus")
                       (when-let (selected (selected corpuses))
                         (setf
                          reference-corpus selected
                          (text label-2)
                          (format nil "Reference corpus: ~A" selected)))
                       ))
         (button-align (defbutton align-frames (frame "Align corpuses")
                         (if (null reference-corpus)
                             (message-box
                              "Please select the reference corpus first."
                              "No reference corpus" "ok" "warning")
                             (when-let ((selected (selected corpuses)))
                               (annotate-framesets reference-corpus selected)
                               (refresh-list corpuses)))))
         (button-add-raw (defbutton add-raw (frame "Add raw corpus")
                           (when-let* ((path (get-open-file*))
                                       (name (input-box "Corpus name" :title "Name")))
                             (setf (gethash name *corpuses*)
                                   (make-instance 'corpus
                                                  :name name
                                                  :sentences (read-file-to-lines-list path)))
                             (refresh-list corpuses))))
         (button-save (defbutton button-save (frame "Save corpus")
                        (format t "saving corpus ~A~%" (selected corpuses))
                        (when-let* ((item (selected corpuses))
                                    (path (get-save-file)))
                          (cl-store:store item path))))
         (button-load (defbutton button-load (frame "Load corpus")
                        (when-let* ((path (get-open-file*))
                                    (item (cl-store:restore path)))
                          (setf (gethash (name item)
                                         *corpuses*) item)
                          (when-let ((ref (reference item)))
                            (setf (gethash (name ref)
                                           *corpuses*) ref))
                          (refresh-list corpuses))))
         (button-drop (defbutton drop (frame "Drop selected")
                        (when-let ((s (selected corpuses)))
                          (remhash (name s) *corpuses*)
                          (refresh-list corpuses)
                          (if (eql s reference-corpus)
                              (setf
                               reference-corpus nil
                               (text label-2) "Reference corpus:"))))))

    (pack frame :fill :both :expand t)
    (let ((row -1))
     (grid label-0         (incf row) 0)
     (grid label-1         (incf row) 0)
     (grid corpuses        (incf row) 0 :sticky "wesn")
     (grid label-2         (incf row) 0 :sticky "wesn")
     (grid button-ref      (incf row) 0 :sticky "wesn")
     (grid button-annotate (incf row) 0 :sticky "wesn")
     (grid button-align    (incf row) 0 :sticky "wesn")
     ;; (grid button-compute  6 0 :sticky "wesn")
     (grid button-add-raw  (incf row) 0 :sticky "wesn")
     (grid button-load     (incf row) 0 :sticky "wesn")
     (grid button-save     (incf row) 0 :sticky "wesn")
     (grid button-drop     (incf row) 0 :sticky "wesn")
     )
    (refresh-list corpuses)
    (grid-columnconfigure frame 0 "weight" 1)
    #+(or)
    (bind lb "<<ListboxSelect>>"
          (lambda (event)
            (declare (ignore event))))
    ))

(defmethod %start-user-interface ((user-interface (eql :ltk)))
  (with-ltk ()
    (let* ((master-f (make-instance 'frame)))
      (create-lists master-f)
      (pack master-f   :fill :both :expand t)
      #+(or)
      (pack (defbutton xxx (master-f "bah")
              (get-open-file))))))


(defmethod %annotate-sentences ((user-interface (eql :ltk))
                                (corpus list))
  (mapcar #'annotate-single-sentence corpus))


;; experiments
#+ (or)
(let* ((mb (make-menubar))
       (mp (make-menu mb "Popup"))
       (mp-1 (make-menubutton mb "Option 1" (lambda () (format t "Popup 1~&") (finish-output))))
       (mp-2 (make-menubutton mb "Option 2" (lambda () (format t "Popup 2~&") (finish-output))))
       (mp-3 (make-menubutton mb "Option 3" (lambda () (format t "Popup 3~&") (finish-output))))))
