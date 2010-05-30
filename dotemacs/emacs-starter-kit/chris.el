;; mkaito's dot-emacs on Arch Linux.
;;
;; Large chunks shamelessly copied from anrxc's dotemacs:
;;   http://git.sysphere.org/dotfiles/tree/emacs
;; My emacs wouldn't be half as pleasant without his help.
;;
;; Latest modification: May 30 2010.

;; {{{ IDO
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(defadvice completing-read
  (around foo activate)
  (if (boundp 'ido-cur-list)
      ad-do-it
    (setq ad-return-value
          (ido-completing-read
           prompt
           (all-completions "" collection predicate)
           nil require-match initial-input hist def))))

(global-set-key
 "\M-x"
 (lambda ()
        (interactive)
        (call-interactively
         (intern
          (ido-completing-read
           "M-x "
           (all-completions "" obarray 'commandp))))))
;; }}}

;; {{{ Colours
;; way better than... wtf!? white? really?
(color-theme-zenburn)

;; Mumamo chunks looks weird...
(setq mumamo-chunk-coloring 1)

(custom-set-faces 
 '(mumamo-background-chunk-submode1 ((((class color)
				      (min-colors 88)
				      (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color)
				      (min-colors 88)
				      (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color)
				      (min-colors 88)
				      (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color)
				      (min-colors 88)
				      (background dark)) nil)))
 '(mumamo-background-chunk-major ((((class color)
				    (min-colors 88) 
				    (background dark)) nil))))
;; }}}

;; {{{ Modes

;; {{{ LUA Mode
;; Lua mode for awesome wm config files
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
;; }}}

;; {{{ Org Mode
(setq org-directory "~/.org/")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; Misc Configs
(setq org-log-done t)
(setq org-completion-use-ido t)
(setq org-return-follows-link t)
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; Files that are included in org-mode agenda
(setq org-agenda-files
 (list "~/.org/personal.org")
)
;; }}}

;; {{{ Remember mode
(require 'remember)
(org-remember-insinuate)

;; Notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; Notes templates
(setq org-remember-templates
 '(("Note" ?n   "* NOTE %?\n %i\n %a" "~/.org/notes.org" "Notes")
   ("Download" ?d "* DL %?\n %i\n %a" "~/.org/notes.org" "Download")
   ("Login" ?l "* LOGIN %?\n %i\n %a" "~/.org/notes.org" "Logins")
   ("Music" ?m "* MUSIC %?\n %i\n %a" "~/.org/notes.org" "Music")
   ("Idea" ?i "* %^{Title}\n %i\n %a" "~/.org/notes.org" "Brainstorm")))

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
  holidays-in-diary-buffer               t
  mark-holidays-in-calendar              t
  all-christian-calendar-holidays        t
  all-islamic-calendar-holidays        nil
  all-hebrew-calendar-holidays         nil
  european-calendar-style                t
  ;display-time-24hr-format              t
  display-time-day-and-date            nil
  ;display-time-format                 nil
  ;display-time-use-mail-icon          nil
  ;calendar-latitude                  45.21
  ;calendar-longitude                 14.26
  ;calendar-location-name "Rijeka, Croatia"
)
;; }}}
;; }}}

;; {{{ Code Folding
;;   - http://www.emacswiki.org/emacs/FoldIngo
(require 'foldingo)
(fold-enter-mode)
;; }}}

;; {{{ Shortcut a few commonly used functions
;;
(defalias 'cr            'comment-region)
(defalias 'ucr           'uncomment-region)
(defalias 'eb            'eval-buffer)
(defalias 'er            'eval-region)
(defalias 'ee            'eval-expression)
(defalias 'day           'color-theme-vim-colors)
(defalias 'night         'color-theme-zenburn)
(defalias 'fold          'fold-enter-fold-mode-close-all-folds)
;;}}}

;;{{{ Key bindings

;; Smart comments. Original idea from:
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
  If no region is selected and current line is not blank and we are not at the end of the line,
  then comment current line.
  Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)
(global-set-key (kbd "\C-c \C-c") 'comment-dwim-line)
;; }}}
