(require 'org-notmuch)
(setq calendar-week-start-day 1)
(setq org-directory "~/notes/")
(setq org-default-notes-file (concat org-directory "refile.org"))
;; (setq org-read-date-prefer-future 'time)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-capture-templates
      '(
        ;; Algo que requiere una acción en el futuro
        ("t" "Tarea" entry (file+headline (concat org-directory "BuJo.org") "Tareas")
				 "** TODO %?\n%^{Para cuándo}t\n%x")

        ;; Algo que me gustaría recordar, que no requiere acción.
				("n" "Nota" entry (file+headline (concat org-directory "BuJo.org") "Notas")
				 "** %?\n%i\n%x")

        ;; Algo que requiere un bloque de tiempo definido en el calendario
				("e" "Entrada" entry (file+headline (concat org-directory "Calendario.org") "Calendario")
				 "** %?\n%^{Cuándo?}T\n%i\n%x")
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
(defun my-redo-all-agenda-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))

(add-hook 'org-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'my-redo-all-agenda-buffers nil 'make-it-local)))

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
