#!/bin/sh

sbcl --load "bundle.lisp" --eval "(asdf:load-system '#:amct)" --eval "(amct::build-executable)"
