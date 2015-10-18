;;; packages.el --- nevermore Layer packages File for Spacemacs
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

(setq nevermore-packages
      '(notmuch)
)

(setq nevermore-excluded-packages '())

(defun nevermore/init-notmuch ()
  "Initialize nevermore"
  (use-package notmuch
    :commands (notmuch notmuch-search)
    :init
    (evil-leader/set-key "am" (lambda()
                                (interactive)
                                (notmuch-search "tag:inbox")))
    :config
    (progn
      ;; Ask notmuch to deal with encrypted/signed message parts
      (setq notmuch-crypto-process-mime t)

      ;; New mail at the top
      (setq-default notmuch-search-oldest-first nil)

      ;; Configure signature
      (setq fortune-dir "/usr/share/fortune"
            fortune-file "/usr/share/fortune/bofh-excuses")
      (add-hook 'message-signature-setup-hook 'fortune-to-signature)

      ;; Configure signing
      (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
      (setq mml-smime-encrypt-to-self t)

      ;; Outgoing mail settings
      (setq message-send-mail-function 'message-send-mail-with-sendmail
            message-sendmail-f-is-evil 't
            sendmail-program "/usr/bin/msmtp"
            message-sendmail-extra-arguments '("--read-envelope-from")
            notmuch-fcc-dirs "outbox"
            message-auto-save-directory "~/Maildir/drafts")

      ;; If we opened a nevermore frame, and are burying from within it,
      ;; close the frame.
      (defadvice nm-bury (after delete-nm-frame activate)
        (if (equal "nevermore" (frame-parameter nil 'name))
            (delete-frame)))

      ;; (defadvice notmuch-bury-or-kill-this-buffer (after delete-nm-frame activate)
      ;;   (if (equal "nevermore" (frame-parameter nil 'name))
      ;;       (delete-frame)))

      ;; Make sure we get an up to date list when invoking nm
      (defadvice nm (after refresh-nm-view activate)
        (nm-refresh))

      ;; Switch to message pane after opening a message
      (defadvice nm-open (after focus-message-pane activate)
        (other-window 1))

      ;; Fix the key map
      (evilify nm-mode nm-mode-map
               ;; (kbd "q")        'toggle-maximize-buffer
               (kbd "RET")      'nm-open
               ;; (kbd " ")        'nm-scroll-msg-up
               ;; (kbd "DEL")      'nm-scroll-msg-down
               ;; (kbd "\C-c\C-c") 'nm-interrupt
               ;; (kbd "\C-c\C-g") 'nm-reset
               (kbd "/")        'nm-incrementally
               ;; (kbd "a")        'nm-archive
               ;; (kbd "d")        'nm-delete
               ;; (kbd "f")        'nm-forward
               ;; (kbd "g")        'nm-refresh
               ;; (kbd "J")        'nm-junk
               ;; (kbd "m")        'notmuch-mua-new-mail
               ;; (kbd "M")        'nm-toggle-query-mode
               (kbd "j")        'next-line
               (kbd "k")        'previous-line
               (kbd "TAB")      'other-window
               ;; (kbd "q")        'nm-bury
               ;; (kbd "r")        'nm-reply
               ;; (kbd "R")        'nm-reply-all
               ;; (kbd "s")        'nm-snooze
               ;; (kbd "S")        'nm-toggle-sort-order
               ;; (kbd "t")        'nm-tag
               ;; (kbd "T")        'nm-focus-thread
               ;; (kbd "W")        'nm-wakeup
               )

      (require 'notmuch-address)

      ;; Fix helm
      (setq notmuch-address-selection-function
            (lambda (prompt collection initial-input)
              (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

      ;; requires aur/notmuch-addrlookup-c
      (setq notmuch-address-command "notmuch-addrlookup")
      (notmuch-address-message-insinuate)

      ;; Nevermore makes use of notmuch for a lot of things
      (evil-leader/set-key-for-mode 'notmuch-show-mode
        "mc" 'notmuch-show-stack-cc
        "m|" 'notmuch-show-pipe-message
        "mw" 'notmuch-show-save-attachments
        "mV" 'notmuch-show-view-raw-message)

      (evilify notmuch-hello-mode notmuch-hello-mode-map)
      (evilify notmuch-show-mode notmuch-show-stash-map)
      (evilify notmuch-show-mode notmuch-show-part-map)
      (evilify notmuch-show-mode notmuch-show-mode-map
               (kbd "N")   'notmuch-show-next-message
               (kbd "n")   'notmuch-show-next-open-message
               (kbd "TAB") 'other-window
               (kbd "q")   'notmuch-bury-or-kill-this-buffer
               (kbd "d")   (lambda ()
                             "Mark the current message as deleted"
                             (interactive)
                             (notmuch-show-tag-message "+deleted"))
               (kbd "RET") 'goto-address-at-point)
      (evilify notmuch-tree-mode notmuch-tree-mode-map)
      (evilify notmuch-search-mode notmuch-search-mode-map
               (kbd "d") (lambda (&optional beg end)
                           "mark thread as deleted"
                           (interactive (notmuch-search-interactive-region))
                           (notmuch-search-tag (list "+deleted" "-inbox") beg end))
               (kbd "s") 'notmuch-jump-search
               (kbd "S") 'notmuch-search)
      (evil-define-key 'visual notmuch-search-mode-map
               (kbd "*") 'notmuch-search-tag-all
               (kbd "a") 'notmuch-search-archive-thread
               (kbd "-") 'notmuch-search-remove-tag
               (kbd "+") 'notmuch-search-add-tag)
      (evil-define-key 'insert message-mode-map
               (kbd "TAB") 'message-tab)
      )))
