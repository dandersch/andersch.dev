#!/bin/sh

emacs -Q --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "other/publish/index.org")'
chmod -w publish.el # publish.el as read only
emacs -Q --script publish.el
