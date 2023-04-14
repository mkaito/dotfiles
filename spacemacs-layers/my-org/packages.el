;;; packages.el --- my-org-mode Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Christian Hoeppner <sylvain.benner@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-org-packages
      '(
        ;; package names go here
        noflet
        ox-bb
        ))

;; I don't want no utf-8 bullets
(defun my-org/init-noflet ()
  "Initialize my package"
  (use-package noflet))

(defun my-org/init-ox-bb ()
  "Initialize da package"
  (use-package ox-bb))
