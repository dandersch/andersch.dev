#!/bin/sh

emacs -Q --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "other/publish/index.org")'
emacs -Q --script publish.el
