#!/usr/bin/guile \
-L . -e main -s
coding: utf-8
!#

(load "libxdo.scm")

(use-modules (xdo libxdo)
             (rnrs bytevectors)) 

(define (main args)
  (let ((xdo (new-xdo)))
    (display "xdo-version: ")
    (display (lib:xdo-version)) (newline)
    (xdo-move-mouse xdo '(600 200))))
