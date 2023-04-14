(with-eval-after-load 'org
	;; (require 'org-notmuch)
  (require 'org-tempo)
  (require 'ox-bb)
	(setq calendar-week-start-day 1)
	(setq org-directory "~/notes/")
	(setq org-default-notes-file (concat org-directory "inbox.org"))
	(setq org-refile-targets '((nil :level . 2)))
	(setq org-read-date-prefer-future 'time)
	(setq org-clock-persist 'history)
	(org-clock-persistence-insinuate)
	(setq org-agenda-files (append
                          '("~/notes" "~/dev")
                          (file-expand-wildcards "~/dev/**/*.org")
                          (file-expand-wildcards "~/notes/**/*.org")))
	(setq org-hide-leading-stars t)
	(setq org-startup-indented t)
	(setq org-startup-folded t)
	(setq org-startup-with-inline-images t)
  (setq org-duration-format (quote h:mm))

  (setq org-agenda-clock-consistency-checks
        '(:max-duration "10:00"
        :min-duration "0:10"
        :max-gap "0:05"
        :gap-ok-around ("4:00")
        :default-face ((:background "DarkRed") (:foreground "white"))
        :overlap-face ((:weight bold))
        :gap-face ((:weight bold))
        :no-end-time-face ((:weight bold))
        :long-face ((:weight bold))
        :short-face ((:weight bold))))

  ;; Capture templates
	(setq org-capture-templates
				'(
					;; Algo que requiere una acción en el futuro
					("t" "Tarea" entry (file+headline "BuJo.org" "Tareas")
					 "* TODO %?\nSCHEDULED: %^{Schedule}t\n %i\n %a\n %x")

					;; Una entrada de tiempo para Serokell
					("s" "Serokell Task" entry (file+headline "serokell/tasks.org" "Inbox")
					 "* TODO %?\nSCHEDULED: %^{Schedule}t\n%i\n%a\n%x")

					;; Algo que me gustaría recordar, que no requiere acción.
					;; ("n" "Nota" entry (file+headline "BuJo.org" "Notas")
					;;  "* %?\n%i\n%a\n%x")

					;; Algo que requiere un bloque de tiempo definido en el calendario
					;; ("c" "Calendario" entry (file+headline "Calendario.org" "Calendario")
					;;  "* %?\n%^{Cuándo?}T\n%i\n%x")
				))

	;; Popup capture window
	(defadvice org-capture-finalize (after delete-capture-frame activate)
		"Advise capture-finalize to close the frame"
		(if (equal "capture" (frame-parameter nil 'name))
				(delete-frame)))

	(defadvice org-capture-select-template (around delete-capture-frame activate)
		"Advise org-capture-select-template to close the frame on abort"
		;; We get an error with C-g
		(unless (ignore-errors ad-do-it t)
			(setq ad-return-value "q"))
		(if (and
				 (equal "q" ad-return-value)
				 (equal "capture" (frame-parameter nil 'name)))
				(delete-frame)))

	;; Rebuild the agenda when I save an org-mode buffer
	;; (defun my-redo-all-agenda-buffers ()
	;;   (interactive)
	;;   (dolist (buffer (buffer-list))
	;;     (with-current-buffer buffer
	;;       (when (derived-mode-p 'org-agenda-mode)
	;;         (org-agenda-redo t)))))

	;; (add-hook 'org-mode-hook
	;;           (lambda()
	;;             (add-hook 'after-save-hook 'my-redo-all-agenda-buffers nil 'make-it-local)))

  ;; Reload files from disk when changed
	;; (add-hook 'org-mode'hook
	;; 					(lambda()
	;; 						(auto-revert-mode 1)))

	;; emacsclient -e '(make-capture-frame)'
	(defun make-capture-frame ()
		"Create a new frame and run org-capture."
		(interactive)
		(make-frame '((name . "capture")
									(width . 120)
									(height. 15)))
		(select-frame-by-name "capture")
		(setq word-wrap 1
					truncate-lines nil)
		(delete-other-windows)
		(noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
		(org-capture)))

	;; Colorize entries in the agenda
	;; (add-hook 'org-finalize-agenda-hook
	;;           (lambda ()
	;;             (save-excursion
	;;               (color-org-header "superguay:"  "orange"))))

	;; (defun color-org-header (tag col)
	;;   ""
	;;   (interactive)
	;;   (goto-char (point-min))
	;;   (while (re-search-forward tag nil t)
	;;     (add-text-properties (match-beginning 0) (point-at-eol)
	;;                          `(face (:foreground ,col)))))

	;; Entry encryption
	(require 'org-crypt)
	(org-crypt-use-before-save-magic)
	(setq org-tags-exclude-from-inheritance (quote ("crypt")))

	;; GPG key to use for encryption
	;; Either the Key ID or set to nil to use symmetric encryption.
	(setq org-crypt-key "57E4E044D36A04DC3550344256CAA6D26866852C")
	(setq auto-save-default nil)
	)
