;;; keybindings.el --- Personal Org-mode Layer key-bindings File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Christian Hoeppner <chris@mkaito.com>
;; URL: 
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(evil-leader/set-key
  "oa" 'org-agenda
  "og" 'helm-org-agenda-files-headings
  "oo" 'org-clock-out
  "oc" 'org-capture
  ;; "oC" 'helm-org-capture-templates ;requires templates to be defined.
  "ol" 'org-store-link)
(setq org-agenda-files (quote ("~/notes" "~/dev")))
