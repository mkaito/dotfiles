(setq org-directory "~/notes/")
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

;; key based encryption of node contents
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "6866852C")		; me@mkaito.com
;;(add-hook 'org-mode-hook '(lambda () (auto-save-mode 0)))

;; Custom key bindings
(add-hook 'org-mode-hook 
          '(lambda ()
            (local-set-key (kbd "C-c d") 'org-decrypt-entry)
	    (local-set-key "\M-n" 'outline-next-visible-heading)
            (local-set-key "\M-p" 'outline-previous-visible-heading)))

(setq org-agenda-custom-commands
     '(("w" todo "TODO")
       ("n" todo "NOTE")
       ("h" agenda "" ((org-agenda-show-all-dates nil)))
       ("W" agenda "" ((org-agenda-ndays 21)
                      (org-agenda-show-all-dates nil)))
       ("A" agenda ""
	((org-agenda-ndays 1)
	 (org-agenda-overriding-header "Today")))))

;; Org-mode customization
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WAITING" "DONE")
        (sequence "|" "NOTE(n)")
        (sequence "|" "CANCELED(x)")))

(setq org-todo-keyword-faces
      '(("WAITING" . "yellow")
	("DROPPED" . "red")
	("CANCELED" . "red")
	("NOTE" . "yellow")))

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w)
                      ("@home" . ?h)
                      (:endgroup . nil)
                      ("@phone" . ?p)
                      ("@errands" . ?e)
                      ("@computer" . ?c)))

;; Clocking options
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Files that are included in org-mode agenda
(setq org-agenda-files
      (list "~/notes/personal.org" "~/notes/refile.org" "~/notes/work.org")
      )

;; Refile targets
(setq org-refile-targets
      (quote
       ((org-agenda-files :maxlevel . 5))))


;; Notes file
(setq org-default-notes-file (concat org-directory "refile.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Tarea" entry (file+headline "personal.org" "Tareas urgentes")
	 "* TODO %?\n  %^{Due date}t\n  %x")
	("n" "Nota" entry (file+headline "personal.org" "Notas")
	 "* %?\n  %i\n  %x")))

;; Capture frames
;;   - $ emacsclient -e '(make-capture-frame)'
;;
(defadvice org-capture-finalize
  (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
  (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
	  'delete-other-windows)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
		(width . 120)
		(height . 15)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture))  

(setq
 diary-show-holidays-flag               t
 calendar-mark-holidays-flag            t
 calendar-christian-all-holidays-flag   t
 calendar-islamic-all-holidays-flag   nil
 calendar-hebrew-all-holidays-flag    nil
 calendar-date-style           "european"
 display-time-24hr-format              t
 display-time-day-and-date            nil
 )
