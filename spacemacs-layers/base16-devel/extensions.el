;;; extensions.el --- base16-devel Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq base16-devel-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq base16-devel-post-extensions
      '(
        ;; post extension names go here
        base16-theme
        ))

;; For each extension, define a function base16-devel/init-<extension-name>
;;
;; (defun base16-devel/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun base16-devel/init-base16-theme ()
  "Initialize stuff"
  (use-package base16-theme))
