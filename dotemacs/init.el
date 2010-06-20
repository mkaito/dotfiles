;; mkaito's dot-emacs on Arch Linux.
;;
;; Large chunks shamelessly copied from anrxc's dotemacs:
;;   http://git.sysphere.org/dotfiles/tree/emacs
;; My emacs wouldn't be half as pleasant without his help.
;;
;; Latest modification: May 30 2010.

;{{{ Initialization
;
;; Define the load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; Turn off the toolbar
(tool-bar-mode -1)
;;
;; Turn off the menu bar
(menu-bar-mode -1)
;;
;; Turn off the scrollbar
(scroll-bar-mode -1)
;;
;; Ispell
(add-hook 'text-mode-hook
      '(lambda () "Defauts for Text mode."
	 (setq ispell-personal-dictionary "~/.emacs.d/ispell-dico-perso")
	 (ispell-change-dictionary "british-ize")
	 (ispell-minor-mode)
	 ))
;; Can't stand the beeping, jfc!
(setq visible-bell t)
;}}}

;{{{ Look & Feel
;
;; Default font
; Font is set in .Xdefaults
;(set-default-font "-xos4-terminus-medium-r-normal-*-12-120-72-72-c-60-iso8859-2")


;; Color theme initialization
;;   - http://emacswiki.org/cgi-bin/wiki/ColorTheme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;;
;; Load preferred theme
;;   - http://www.brockman.se/software/zenburn/zenburn.el
(require 'zenburn)
(color-theme-zenburn)

;; Support 256 colors in screen
;;   - http://article.gmane.org/gmane.emacs.devel/109504/
(if (not (window-system)) (load "term/rxvt"))
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the rxvt color initialization code.
  (rxvt-register-default-colors)
  (tty-set-up-initial-frame-faces)
)


;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Shut off message buffer
(setq message-log-max nil)
(kill-buffer "*Messages*")

;; Show column number in modeline
(setq column-number-mode t)

;; Modeline setup
;;   - somewhat cleaner than default
(setq default-mode-line-format
      '("-"
       mode-line-mule-info
       mode-line-modified
       mode-line-frame-identification
       mode-line-buffer-identification
       "  "
       global-mode-string
       "   %[(" mode-name mode-line-process minor-mode-alist "%n"")%]--"
       (line-number-mode "L%l--")
       (column-number-mode "C%c--")
       (-3 . "%p")
       "-%-")
)


;; Syntax coloring (font-lock-mode)
(global-font-lock-mode t)

;; Always flash for parens and define a more distinctive color
(show-paren-mode 1)
(set-face-foreground 'show-paren-match-face "#bc8383")

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

;; Darkroom mode
(require 'darkroom-mode)
;}}}

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

;; }}}

;; {{{ Modes

;; {{{ Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\.markdown" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'darkroom-mode)
;; }}}

;; {{{ LUA Mode
;; Lua mode for awesome wm config files
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; }}}

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

;; Files that are included in org-mode agenda
(setq org-agenda-files
 (list "~/.org/personal.org" "~/.org/notes.org")
)
;; }}}

;; {{{ Remember mode
(require 'remember)
(org-remember-insinuate)

;; Notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; Notes templates
(setq org-remember-templates
 '(("Note" ?n   "** NOTE %?\n %i\n %a" "~/.org/notes.org" "Notes")
   ("Download" ?d "** DL %?\n %i\n %a" "~/.org/notes.org" "Download")
   ("Login" ?l "** LOGIN %?\n %i\n %a" "~/.org/notes.org" "Logins")
   ("Music" ?m "** MUSIC %?\n %i\n %a" "~/.org/notes.org" "Music")
   ("Idea" ?i "** %^{Title}\n %i\n %a" "~/.org/notes.org" "Brainstorm")))

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

;; {{{ Post Mode
;    - http://post-mode.sourceforge.net/
;
(autoload 'post-mode "~/.emacs.d/post.el" "Major mode for editing e-mail and journal articles" t)
(add-to-list 'auto-mode-alist 
  '("\\.*mutt-*\\|\\.*pico.*\\|.article\\|\\.*200\\(T\\)?\\|\\.followup" . post-mode))
;; }}}

;; {{{ Custom modes for some custom files
;
;; Shell script mode for Arch PKGBUILDs
(setq auto-mode-alist (cons '("\\PKGBUILD$" . sh-mode) auto-mode-alist))
;;
;; Conf mode for personal config files
(when (locate-library "conf-mode")
  (autoload 'conf-mode "conf-mode" "Major-mode for editing config files." t)
  (add-to-list 'auto-mode-alist '("\\awesomerc$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\gitconfig$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\screenrc$"  . conf-mode))
  (add-to-list 'auto-mode-alist '("\\pinerc$"    . conf-mode))
  (add-to-list 'auto-mode-alist '("\\zshrc$"     . conf-mode))
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

(global-set-key (kbd "\M-;") 'comment-dwim-line)
(global-set-key (kbd "\C-c \C-c") 'comment-dwim-line)

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs, 
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))
;; }}}
