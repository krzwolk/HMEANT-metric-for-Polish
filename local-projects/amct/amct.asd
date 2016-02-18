;;;; amct.asd

(asdf:defsystem #:amct
  :description "HMEANT Metric for Polish"
  :author "Krzysztof Wolk <krzysztof@wolk.pl>"
  :license "Free to use if cite."
  :serial t
  :depends-on (#:alexandria #:ltk #:ltk-mw #:split-sequence #:cl-store #:translate)
  :components ((:file "package")
               (:file "translations")
               (:file "annotation")
               ;; (:file "text-user-interface")
               (:file "graphical-user-interface")
               (:file "computation")
               (:file "amct")))
