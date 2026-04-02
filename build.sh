#!/bin/sh

emacs -Q --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "other/publish/index.org")'

# Better: move this into the org tangle header, then you can drop chmod here.
# #+PROPERTY: header-args:emacs-lisp :comments link :mkdirp yes :tangle-mode (identity #o444)
chmod -w publish.el # publish.el as read only NOTE: can be done with a tangle header

emacs -Q --script publish.el
