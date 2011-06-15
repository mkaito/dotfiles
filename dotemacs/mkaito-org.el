;; {{{ Org Mode
(require 'org-install)
(setq org-directory "~/.org/")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Misc Configs
(setq org-log-done t)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; MobileOrg
;; Set to the name of the file where new notes will be stored
;;(setq org-mobile-inbox-for-pull "~/.org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;;(setq org-mobile-directory "~/Dropbox/MobileOrg")

;;(setq org-agenda-custom-commands
;;      '(("w" todo "TODO")
;;       ("h" agenda "" ((org-agenda-show-all-dates nil)))
;;       ("W" agenda "" ((org-agenda-ndays 21)
;;                       (org-agenda-show-all-dates nil)))
;;       ("A" agenda ""
;;        ((org-agenda-ndays 1)
;;         (org-agenda-overriding-header "Today")))))

;; Org-mode customization
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WAITING" "DONE")
        (sequence "NOTE(n)" "|" "REVIEWED")
        (sequence "DOWNLOAD(d)" "DOWNLOADING" "|" "DOWNLOADED")
        (sequence "READ(r)" "READING" "|" "DONE")
	(sequence "TO WATCH(w)" "WATCHING" "|" "WATCHED")
        (sequence "CALL(c)" "|" "WAITING" "CALLED")
        (sequence "|" "CANCELED(x)")))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup . nil)
                      ("@phone" . ?p)
                      ("@errands" . ?e)
                      ("@computer" . ?c)))

;; Files that are included in org-mode agenda
(setq org-agenda-files
      (list "~/.org/personal.org" "~/.org/notes.org" "~/.org/permanent.org" "~/.org/work.org" "~/.org/shows.org")
      )

;; Refile targets
(setq org-refile-targets
      (quote
       ((org-agenda-files :maxlevel . 5))))

;; }}}

;; {{{ Remember mode
(require 'remember)
(org-remember-insinuate)

;; Notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; Notes templates
(setq org-remember-templates
      '(("Note"     ?n "*** NOTE %?\n    %u\n    %i\n    %a"      "~/.org/notes.org" "Notes")
        ("Download" ?d "*** DOWNLOAD %?\n    %u\n    %i\n    %a"  "~/.org/notes.org" "Notes")
        ("Read"     ?r "*** READ %?\n    %u\n    %i\n    %a"      "~/.org/notes.org" "Notes")
        ("Todo"     ?t "*** TODO %?\n    %t\n    %i\n    %a"      "~/.org/notes.org" "Notes")
        ("Call"     ?c "*** CALL %?\n    %T\n    %i\n    %a"      "~/.org/notes.org" "Notes")))

;; Remember frames
;;   - $ emacsclient -e '(make-remember-frame)'
;;
;; Org-remember splits windows, force it to a single window
(add-hook 'remember-mode-hook  'delete-other-windows)

;; Automatic closing of remember frames
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
      (delete-frame))
  )
(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
      (delete-frame))
  )

;; Initialization of remember frames
(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))
  (select-frame-by-name "*Remember*")
  (org-remember)
  )
;; }}}

;; {{{ Calendar settings
;;
(setq
 diary-show-holidays-flag               t
 calendar-mark-holidays-flag            t
 calendar-christian-all-holidays-flag   t
 calendar-islamic-all-holidays-flag   nil
 calendar-hebrew-all-holidays-flag    nil
 calendar-date-style           "european"
                                        ;display-time-24hr-format              t
 display-time-day-and-date            nil
                                        ;display-time-format                 nil
                                        ;display-time-use-mail-icon          nil
                                        ;calendar-latitude                  45.21
                                        ;calendar-longitude                 14.26
                                        ;calendar-location-name "Rijeka, Croatia"
 )
;; }}}
