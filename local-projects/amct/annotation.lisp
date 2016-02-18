;;;; annotation.lisp

(in-package #:amct)


;;; Parameters

(defparameter *ui-type* :ltk ;:tui
  "Preferred user interface type")

(defparameter *corpuses* (make-hash-table :test #'equal)
  "List of loaded corpuses")


;;; API

(deftype label-match ()
  "Type representing SRL match in the semantic frame."
  '(member :no-match :incorrect :partial :correct))

(defun start-user-interface ()
  "Starts the main window, toplevel, or whatever is the main entity of
a user interface."
  (%start-user-interface *ui-type*))

(defun annotate-sentences (corpus)
  "Annotate each translation in the provided corpus."
  (%annotate-sentences *ui-type* corpus))

(defun annotate-framesets (reference machine)
  (%annotate-framesets *ui-type* reference machine))


;;; Class definitions

;; Who did what (to) whom, when, where, why and how?
(defclass slr-frame ()
  ((matched-p       :initform nil :accessor matched?)
   (correct-matches :initform 0   :accessor correct) ; #fillers
   (partial-matches :initform 0   :accessor partial) ; #fillers
   (mismatches      :initform 0   :accessor incorrect)
   (no-matches      :initform 0   :accessor no-match)

   (fillers-total :initform 0 :initarg :fillers :accessor fillers)
   (fillers-ref   :initform 0 :accessor fillers-ref)

   ;; did (predicate/action)
   (pred  :initarg :pred :accessor pred)

   ;; (#fillers . string)
   (arg-0 :initarg :arg-0 :documentation "who?   (agent)")
   (arg-1 :initarg :arg-1 :documentation "what?  (experiencer)")
   (arg-pat :initarg :arg-pat :documentation "whom?  (patient)")
   (arg-tmp :initarg :arg-tmp :documentation "when?  (temopral)")
   (arg-loc :initarg :arg-loc :documentation "where? (location)")
   (arg-pur :initarg :arg-pur :documentation "why?   (purpose)")
   (arg-man :initarg :arg-man :documentation "how?   (manner)")
   (arg-deg :initarg :arg-deg :documentation "how?   (degree)")
   (arg-neg :initarg :arg-neg :documentation "how?   (negation)")
   (arg-mod :initarg :arg-mod :documentation "how?   (modal)")
   (arg-oth :initarg :arg-oth :documentation "how?   (other)"))
  (:documentation
   "Semantic labeling frame with some meta-information about predicate
match (only in the machine translation), number of the correct,
partial, incorrect and lack of matches and numbers of fillers."))

(defmethod print-object ((object slr-frame) stream)
  (format stream
          "#<SLR-FRAME '~A' :FILLERS ~A :CORRECT ~A :PARTIAL ~A>"
          (pred    object)
          (fillers object)
          (correct object)
          (partial object)))

(defclass translation-frame ()
  ((sentence   :initarg :sentence :accessor sentence)
   (slr-frames :initarg :frames   :accessor frames :initform nil))
  (:documentation
   "Translation frame may contain many semantic frames. After
annotating them such translation frame contains additionally
information about the partial-matches and the exact
matches. Encapsulates also the original sentence."))

(defmethod print-object ((object translation-frame) stream)
  (format stream
          "#<TRL-FRAME :SLR-FRAMES ~A>"
          (length (frames object))))

(defclass corpus ()
  ((reference :initform nil :accessor reference
              :documentation "Reference corpus.")
   (name :initform (error "Corpus name is obligatory.")
         :initarg :name :accessor name)
   (sentences :initform nil :initarg :sentences
              :accessor sentences
              :documentation "List of translated sentences")
   (annotations :accessor annotations)
   (pred-count  :accessor pred-count
                :documentation "Number of predicates in both the
reference and the translation (cons #mt #ref)")
   (meant-score :initform 0.0 :accessor meant-score))
  (:documentation "Corpus containing the inter-scores of each part of
computation (annotations, frame etc)."))

(defmethod print-object ((object corpus) stream)
  (format stream
          "#<~A \"~A\" (~A) :SCORE ~A :REF \"~A\">"
          (if (slot-boundp object 'annotations)
              "ANNOTATED-CORPUS" "CORPUS")
          (name object)
          (length (sentences object))
          (meant-score object)
          (when-let ((ref (reference object)))
            (name ref))))


;;; Generic methods

(defgeneric %start-user-interface (user-interface)
  (:documentation "Starts the main window, toplevel, or whatever is
the main entity of a user interface."))

(defgeneric %annotate-sentences (user-interface corpus)
  (:method (user-interface (hash-key symbol))
    (annotate-sentences (gethash hash-key *corpuses*)))
  (:method (user-interface (corpus corpus))
    (unless (slot-boundp corpus 'annotations)
      (setf (annotations corpus)
            (mapcar (lambda (s)
                      (make-instance 'translation-frame :sentence s))
                    (sentences corpus))))
    (annotate-sentences (annotations corpus)))
  (:documentation "Annotate each translation in the provided
corpus."))

(defgeneric %annotate-framesets (user-interface reference machine)
  (:method (user-interface (ref-hash-key symbol) (mt-hash-key symbol))
    (let ((ref (gethash ref-hash-key *corpuses*))
          (mt  (gethash mt-hash-key *corpuses*)))
      (annotate-framesets (annotations ref)
                          (annotations mt))
      (setf (pred-count mt)
            (cons (predicates-count mt)
                  (predicates-count ref)))))
  (:documentation "Annotate how aligned is the machine translation to
the reference translation."))


;;; Metric

(defun compute-hmeant (object &optional (w 0.5))
  (check-type object corpus)
  (let ((P 0.0)
        (R 0.0)
        (P-part 0.0)
        (R-part 0.0))
    (map-pred
     (lambda (predicate)
       (with-slots (matched-p
                    correct-matches partial-matches
                    fillers-total fillers-ref)
           predicate
         (unless (zerop fillers-total)
          (incf P      (/ correct-matches fillers-total))
          (incf P-part (/ partial-matches fillers-total)))
         (unless (zerop fillers-ref)
          (incf R      (/ correct-matches fillers-ref))
          (incf R-part (/ partial-matches fillers-ref)))))
     object)
    (unless (or (zerop (car (pred-count object)))
                (zerop (cdr (pred-count object))))
      (let* ((P-total (/ (+ P (* w P-part)) (car (pred-count object))))
             (R-total (/ (+ R (* w R-part)) (cdr (pred-count object)))))
        (setf (meant-score object) (/ (* 2 P-total R-total)
                                      (+ P-total R-total)))))))


;;; Utility functions
(defun read-file-to-lines-list (filespec)
    (with-open-file (stream filespec)
      (do ((sentence (read-line stream)
                     (read-line stream nil nil))
           acc)
          ((null sentence) (nreverse acc))
        (push sentence acc))))

(defun map-pred (function corpus)
  (check-type corpus corpus)
  (map nil (lambda (tf)
             (map nil function (frames tf)))
       (annotations corpus)))

(defun predicates-count (object)
  (check-type object corpus)
  (reduce #'+ (mapcar #'(lambda (tf)
                          (length (frames tf)))
                      (annotations object))))

(defun make-counter (function)
  "Creates wrapper for any function counting words of the result. The
second function contains total number of arguments"
  (let ((cnt 0))
    (values
     (lambda (&rest args)
       (let* ((result (apply function args))
              (fillers (length
                        (split-sequence:split-sequence
                         #\space result))))
         (unless (emptyp result)
           (incf cnt fillers)
           (cons fillers result))))
     (lambda () cnt))))
