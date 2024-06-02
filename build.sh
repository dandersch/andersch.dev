#!/bin/sh

emacs -Q --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "other/publish.org")'
emacs -Q --script publish.el
