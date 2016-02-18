;;;; translations.lisp

(in-package #:amct)

(translate:define-language 'pl
    "predicate" "predykat"
    "(verb)"    "(czasownik)"

    "agent" "mianownik"
    "Who?"  "kto? co?"
    
    "experiencer" "dopełniacz/biernik"
    "What?"       "kogo? czego? co?"

    "patient" "celownik"
    "Whom?"   "komu? czemu?"

    "temporal" "czas zdarzenia"
    "When?"    "kiedy?"

    "location" "miesce zdarzenia"
    "Where?"   "gdzie?"

    "purpose"  "cel"
    "Why?"     "dlaczego?"

    "manner"   "sposób"
    "How? (1)" "w jaki sposób?"

    "degree/extent" "stopień"
    "How? (2)" "jak bardzo?"

    "negation" "zaprzeczenie"
    "How? (3)" "(czy zanegowany?)"

    "modal"    "czasownik modalny"
    "How? (4)" "co?"

    "other adv arg" "inny argument"
    "How? (5)" "(inne)")

(setf translate:*language* 'pl)
