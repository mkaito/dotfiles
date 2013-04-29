;; mkaito's dot-emacs on Arch Linux.
;;
;; Large chunks shamelessly copied from anrxc's dotemacs:
;;	 http://git.sysphere.org/dotfiles/tree/emacs
;; My Emacs wouldn't be half as pleasant without his help.
;;
(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar wanted-packages
  '(coffee-mode expand-region smart-tabs-mode php-mode
		gist haml-mode haskell-mode inf-ruby lua-mode
		markdown-mode paredit projectile js2-mode
		sass-mode rainbow-mode scss-mode ack-and-a-half
		yaml-mode zenburn-theme nginx-mode)
  "A list of packages to ensure are installed at launch.")

(defun wanted-packages-installed-p ()
  (loop for p in wanted-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (wanted-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p wanted-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Clean initial buffers, windows.
(defun buffer-exists (bufname) (not (eq nil (get-buffer bufname)))) 
(if (buffer-exists "*Compile-Log*") (kill-buffer "*Compile-Log*"))
(if (buffer-exists "*Backtrace*") (kill-buffer "*Backtrace*"))
(delete-other-windows)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;;Auto save and backups
(setq
backup-by-copying t
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

(setq backup-directory-alist
			`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
			`((".*" ,temporary-file-directory t)))

;; Fix dead keys
(load-library "iso-transl")

;; Colour theme initialization
(load-theme 'zenburn t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#353535")

;; Support 256 colours in screen
;;	 - http://article.gmane.org/gmane.emacs.devel/109504/
(if (not (window-system)) (load "term/rxvt"))
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  Use the rxvt color initialization code.
  (rxvt-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; Don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Shut off message buffer
(setq message-log-max nil)
(kill-buffer "*Messages*")

;; Show column number in modeline
(setq column-number-mode t)

;; Friendlier copy/paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Modeline setup
;;	 - somewhat cleaner than default
(setq mode-line-format
      '("-"
       	mode-line-mule-info
       	mode-line-modified
       	mode-line-frame-identification
       	mode-line-buffer-identification
       	"	 "
       	global-mode-string
       	"		%[(" mode-name mode-line-process minor-mode-alist "%n"")%]--"
       	(line-number-mode "L%l--")
       	(column-number-mode "C%c--")
       	(-3 . "%p")
       	"-%-")
      )

;; Always flash for parens and define a more distinctive color
(show-paren-mode 1)
(set-face-foreground 'show-paren-match-face "#bc8383")

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set executable bit on scripts after saving
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Spell correction
(require 'ispell)
(setq ispell-prefer-aspell t
      ispell-program-name "aspell"
      ispell-list-command "list"
      ispell-extra-args '("--sug-mode=fast")
      ispell-dictionary "british-ize")
      
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(global-set-key (kbd "\C-c C-4") 'flyspell-correct-word-before-point)
(global-set-key (kbd "\C-c C-\\") 'flyspell-correct-word-before-point)

(global-set-key (kbd "C-=") 'er/expand-region)

;; Split window navigation: S-arrow
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Encrypted file editing
(epa-file-enable)

;; Fuzzy matching and all that jazz
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

(projectile-global-mode)
(setq projectile-use-native-indexing t)
(setq projectile-enable-caching t)
;; ;;(recentf-mode 1)

(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;; ;; SH mode
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-c") '(lambda ()
					       "Pipe the buffer through zsh -vx"
					       (interactive)
					       (shell-command-on-region (point-min) (point-max) "zsh -vx")))))

;; Coffee Mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Haskell mode
;;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; YAML mode
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'\\|\\.mkd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Lua mode for awesome wm config files
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(load "~/.emacs.d/settings-org-mode.el")

;; Post Mode
(add-to-list 'auto-mode-alist
	     '("\\.*mutt-*\\|\\.*pico.*\\|\\.*200\\(T\\)?\\|\\.followup" . mail-mode))
(add-hook 'mail-mode-hook 'auto-fill-mode)
(add-hook 'mail-mode-hook 'flyspell-mode)

;; Custom modes for some custom files
;; Shell script mode for Arch PKGBUILDs
(add-to-list 'auto-mode-alist '("\\PKGBUILD$" . sh-mode))

;; Conf mode for personal config files
(when (locate-library "conf-mode")
  (autoload 'conf-mode "conf-mode" "Major-mode for editing config files." t)
  (add-to-list 'auto-mode-alist '("\\gitconfig$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\screenrc$"	 . conf-mode))
  (add-to-list 'auto-mode-alist '("\\pinerc$"	 . conf-mode))
  (add-to-list 'auto-mode-alist '("\\zshrc$"	 . conf-mode))
  )
;;
;; Ruby mode for Gemfile and config.ru
(add-to-list 'auto-mode-alist '("\\Gemfile$"	 . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$"	 . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"	 . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$"	 . ruby-mode))
(add-to-list 'auto-mode-alist '("\\config\.ru$"  . ruby-mode))

;; Ruby mode hooks
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "M--") 'hippie-expand)

(setq scss-compile-at-save nil
      scss-output-directory "../static/css")

(add-hook 'scss-mode-hook 'flyspell-prog-mode)
;; Smart comments.
;; - http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
 (defun comment-dwim-line (&optional arg)
   "Replacement for the comment-dwim command.  If no region is
    selected and current line is not blank and we are not at the end
    of the line, then comment current line.  Replaces default
    behaviour of comment-dwim, when it inserts comment at the end of
    the line."
   (interactive "*P")
   (comment-normalize-vars)
   (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
       (comment-or-uncomment-region (line-beginning-position) (line-end-position))
     (comment-dwim arg)))

 (global-set-key (kbd "M-;") 'comment-dwim-line)
 (global-set-key (kbd "C-c C-c") 'comment-dwim-line)

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
	indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

;; Renames the current file and updates the buffer
(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
	(set-buffer (dired-noselect file))
	(dired-do-rename)
	(kill-buffer nil))))
  nil)

;; \C-x \C-w = Save as
;; \C-x w    = Rename
(global-set-key (kbd "\C-xw") 'rename-current-file-or-buffer)

;; Indentation
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")

(setq-default indent-tabs-mode t)
(setq tab-width 2)

;; C/C++
(add-hook 'c-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

;; JavaScript
(add-hook 'js2-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice js2-indent-line js2-basic-offset)

;; Perl (cperl-mode)
(add-hook 'cperl-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice cperl-indent-line cperl-indent-level)

;; Python
(add-hook 'python-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice python-indent-line-1 python-indent)

;; Ruby
(add-hook 'ruby-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice ruby-indent-line ruby-indent-level)

;; Shell
(add-hook 'sh-mode-hook 'smart-tabs-mode-enable)
(smart-tabs-advice sh-indent-line sh-indentation)

;; This should cover mostly everything...
(define-key global-map (kbd "RET") 'newline-and-indent)

;; ... but ruby-mode thinks otherwise.
(add-hook 'ruby-mode-hook
	  '(lambda()
	     (local-set-key (kbd "RET") 'newline-and-indent)))

;; When killing the end of a line, eliminate any indentation of the next line
(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1))))

(defun open-line-below-and-go-there ()
  "Open new line below current and place cursor indented"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "M-RET") 'open-line-below-and-go-there)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max)))

;{{{ Reload or edit .emacs on the fly
;    - key bindings defined below
;
(defun aic-reload-dot-emacs ()
  "Reload user configuration from .emacs"
  (interactive)
  ;; Fails on killing the Messages buffer, workaround:
  (get-buffer-create "*Messages*")
  (load-file "~/.emacs.d/init.el")
)
(defun aic-edit-dot-emacs ()
  "Edit user configuration in .emacs"
  (interactive)
  (find-file "~/.emacs.d/init.el")
)
;}}}

;; Reload or edit .emacs as defined above
(global-set-key "\C-c\C-r" 'aic-reload-dot-emacs)
(global-set-key "\C-c\C-e" 'aic-edit-dot-emacs)

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)

(defun shutdown-emacs-server () (interactive)
  (when (not (eq window-system 'x))
    (message "Initializing x windows system.")
    (x-initialize-window-system)
    (when (not x-display-name) (setq x-display-name (getenv "DISPLAY")))
    (select-frame (make-frame-on-display x-display-name '((window-system . x))))
    )
  (let ((last-nonmenu-event nil)(window-system "x"))(save-buffers-kill-emacs)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
