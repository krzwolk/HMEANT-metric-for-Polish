About
=====

Implementation of the HMEANT evaluation metric for Polish and other West-Slavic languages.

With GUI. Easily adaptable for other languages.

Calculated as presented in prec-rec-eq.png.


Installation
=====

Application requires wish application (CLI frontend for tk library). It is commonly installed on Linux systems. Binary is built for Linux. No special installation is required, just run pre-compiled HMEANT file either from console or directly with file manager.

Usage - Main GUI
=====

Main window has a few elements:
     
"Available corpuses" list:
          Each corpus's entry contains information about it's name,
          annotation state and number of sentences. Additionally, meant
          score (0.0 before computation) and reference corpus name are
          printed (NIL by default).
"Annotate" button:
          Opens annotation window for a selected corpus. If none is
          selected, then nothing happens. If corpus was annotated,
          then predicates are present in the annotation window, so
          this is also a convenient way to verify the
          annotation. After annotating first sentence window for the
          second one appears etc.
"Align corpuses" button:
          Aligning corpuses requires setting the reference corpus with
          the "Set reference corpus" button. Alignment is performed
          for each predicate in the reference corpus.
"Add raw corpus" button:
          Opens dialog window to pick a file with raw translations
          (each sentence in a new line, so the third translation will
          be in a third line of the file). User is asked about the
          corpus name (if corpus of the same name is already loaded,
          it is overwritten).
"Load corpus" / "Save corpus" buttons:
          Allows storing and restoring corpus from a file. The
          reference corpus (if present) and MEANT score are also
          stored in the file.
"Drop selection" button:
          Removes selected corpus from "Available corpuses" list.

Usage - Annotating corpus
=====

     In order to compute metric user has to load a raw corpus, select
     it in the list and press "Annotate" button. Annotation window
     should appear.

     Human annotator has to fill entries for each predicate found in
     the sentence (on the top). When he finishes one predicate, he has
     to press "Add predicate" button – created predicate should appear
     in the list on the left. If no more predicates is present in the
     sentence he has to press "Sentence annotated" – this will move
     him to the next sentence or finish the process (if all are
     already annotated).

     We have to annotate both reference and the machine translation
     corpuses to align them in the next step.

Notes for Polish language
=====

      Polish language has somewhat complicated grammar and many
      predicate arguments (or even a predicate!) may be absent, but
      context clearly shows it. It is annotators responsibility to add
      them anyway in the square brackets.

      Let’s consider a sample sentence:
      "Kupiliśmy wczoraj zielony gramofon, a sąsiedzi czarną wieżę stereo."

      Despite having only one verb ("kupiliśmy"), we have two
      predicates (it is indirect, that this verb applies also to the
      second half of the sentence). Also the suffix of the verb
      indicates, that "agent" of the first predicate is "my". So,
      annotation should look as follows (absent arguments are ommited):

      predicate           :: kupiliśmy
      agent         Who?  :: [my]
      experiencer   What? :: zielony gramofon
      temporal      When? :: wczoraj

      predicate           :: [kupić]
      agent         Who?  :: sąsiedzi
      experiencer   What? :: czarną wieżę stereo
      temporal      When? :: [wczoraj]

      As we can see, three fillers are "guessed" by the annotator, but
      are obvious for polish speaker.

Usage - Aligning corpuses
=====

     For more detailed usage instruction see howto.pdf.

     After annotating corpuses (they should appear as
     #<ANNOTATED-CORPUS …> in the list of the main window) user has to
     pick a reference corpus in the list and press "Set reference
     corpus". Next he selects any translation and presses "Align
     corpuses". New window should appear.

     In this window user has to match the predicates, because the
     order of them in the translation may differ. He has to select a
     matching predicate from the list on the right and click "Confirm
     match" button. If there is no match for the predicate, user
     should select "[no match]" option.

     This will open alignment window for two semantic frames, where
     user has to mark each argument alignment as "correct", "partial"
     or "incorrect". When finished, he has to press "Confirm
     alignment". The procedure repeats for the next predicate.

     When alignment is completed score is computed automatically and
     should appear in the "Available corpuses" window.

Building from sources
=====

   To build application Common Lisp implementation SBCL (tested on
   version 1.3.0) must be present on the system with configured
   quicklisp library manager (quicklisp.org).

   Running application requires wish (CLI backend for TK library).

   Standalone binary may be downloaded from here: http://prdownloads.sourceforge.net/sbcl/sbcl-1.3.2-x86-64-linux-binary.tar.bz2 (version from Ubuntu repositories is too old). Quicklisp installation is explained in detail here: https://www.quicklisp.org/beta/ .
The language used is Common Lisp – https://common-lisp.net/

Files:

amct.asd - system definition - denotes dependencies, system name as seen by runtime etc.
amct.lisp - a few helper functions for building standalone executable
package.lisp - defines a package
annotations.lisp - contains definitions of
-slr-frame - class embodying a predicate with it's arguments, number of fillers and numbers of correct/partial matches
-tlr-frame - class embodying a sentence and all associated predicates (slr-frames)
-corpus - class embodying a set of sentences (tlr-frames), a name, score

Additionally, there are a few utility functions and the function which computes the MEANT score. Moreover it contains definitions of generic functions implemented by graphical-user-interface.lisp (https://en.wikipedia.org/wiki/Generic_function).

- graphical-user-interface.lisp - implementations of the generic functions from the previous file. Basically this implements GUI for the application performed through wish program (command line interface to tk). So here are defined windows and their behavior.

- text-user-interface.lisp - currently inactive (commented out in asd file). Contains implementation of the generic functions from the annotations.lisp in a terminal manner (I gave you an application utilizing this on the previous iteration).

When all is ready just run ./build.sh

Sample annotations
=====

NOTE:
You must remember about the context of entire text. In following example agent in sentence 2 is take from sentence 1.

mijający rok dwa tysiące czternasty był bardzo szczególny dla naszego kraju.
[predicate]: był
[agent]: mijający rok dwa tysiące czternasty
[experiencer]: szczególny
[patient]: dla naszego kraju
[degree/extent]: bardzo

To był czas pamięci i refleksji.
[predicate]: był (2)
[agent]: [mijający rok dwa tysiące czternasty]
[experiencer]: czas pamięci i refleksji

Obchodziliśmy dwudziestą piątą rocznicę pierwszych wolnych wyborów i powołania rządu Tadeusza Mazowieckiego.
[predicate]: Obchodziliśmy
[agent]: [my]
[experiencer]: dwudziestą piątą rocznicę pierwszych wolnych wyborów i powołania rządu Tadeusza Mazowieckiego

Rządu który położył fundamenty pod budowę wolnej i demokratycznej Polski.
[predicate]: położył
[agent]: rządu
[experiencer]: fundamenty
[patient]: pod budowę wolnej i demokratycznej polski

Final info
=====

Feel free to use this tool if you cite: Wołk, K., Korzinek, D., & Marasek, K. (2017). Semi-automatic and Human-Aided Translation Evaluation Metric (HMEANT) for Polish Language in Re-speaking and MT Assessment. In Multimedia and Network Information Systems (pp. 241-249). Springer International Publishing.

http://arxiv.org/abs/1601.02789

For more detailed usage instruction see howto.pdf.

For any questions: | Krzysztof Wolk | krzysztof@wolk.pl
