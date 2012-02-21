;; mkaito's dot-emacs on Arch Linux.
;;
;; Large chunks shamelessly copied from anrxc's dotemacs:
;;	 http://git.sysphere.org/dotfiles/tree/emacs
;; My Emacs wouldn't be half as pleasant without his help.
;;
;; Latest modification: May 15 2011.

;; Actually, what do I want from Emacs?
;;
;; * Syntax highlighting for: Markdown, Ruby, JavaScript, Lua, HAML,
;;	 SASS/SCSS/CSS, HTML.
;; * Code folding with markers (I don't like the automatic ones)
;; * Clean interface, low contrast colour theme.
;; * Snippet expansion
;;
;; Organization:
;;
;; init.el should contain all common configuration. One file per major
;; mode, containing all and any configuration relating to this mode,
;; and one file per major configuration topic if necesary. Avoid
;; having 250 config files.
;;

(setq load-path
      (append
       (list
	"~/.emacs.d"
	"~/.emacs.d/vendor"
	"~/.emacs.d/vendor/color-theme-ir-black"
	"~/.emacs.d/vendor/haml-mode"
	"~/.emacs.d/vendor/sass-mode"
	"~/.emacs.d/vendor/coffee-mode"
	"~/.emacs.d/vendor/smart-tabs-mode"
	"~/.emacs.d/vendor/nginx-mode"
	"~/.emacs.d/vendor/yasnippet"
	)
       load-path))

;; Check for stale .elc on startup. Slow, but I only restart Emacs after reboots.
(byte-recompile-directory "~/.emacs.d/vendor" 0)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;; Fix dead keys
(load-library "iso-transl")

;; Colour theme initialization
;;	 - http://emacswiki.org/cgi-bin/wiki/ColorTheme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(require 'color-theme-ir-black)
(color-theme-ir-black)

;; Support 256 colours in screen
;;	 - http://article.gmane.org/gmane.emacs.devel/109504/
(if (not (window-system)) (load "term/rxvt"))
(defun terminal-init-screen ()
  "Terminal initialization function for screen."
  ;; Use the rxvt color initialization code.
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

;; Syntax coloring (font-lock-mode)
(global-font-lock-mode t)

;; Always flash for parens and define a more distinctive color
(show-paren-mode 1)
;; (set-face-foreground 'show-paren-match-face "#bc8383")

;; Answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use ANSI colors within shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Mumamo chunks looks weird...
(setq mumamo-chunk-coloring 1)

(custom-set-faces
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil))))

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

;; Move tempfiles and auto saves elsewhere
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
	 `(("." . ,user-temporary-file-directory)
	(,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
	 (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
	 `((".*" ,user-temporary-file-directory t)))

;; Auto save options.
(setq auto-save-visited-file-name t
      auto-save-interval 0
      auto-save-timeout 3)

;; Split window navigation: S-arrow
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; (require 'epa-file)
;; (epa-file-enable)

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

(require 'find-file-in-project)
(setq ffip-find-options "-not -regex \".*vendor.*\"")

;; Assume default-directory if no project root could be found
(setq ffip-project-root-function '(lambda ()
				    (or
				     (locate-dominating-file default-directory ffip-project-file)
				     default-directory)))

(setq ffip-patterns
      (append
       (list
	"Gemfile"
	"Procfile"
	"Rakefile"
	"*.gpg"
	"*.org"
	"config.ru"
	"*.yaml"
	"*.yml"
	"*.watchr"
	"*.rake"
	"*.gemspec"
	)
       ffip-patterns))

(global-set-key (kbd "C-c C-f") 'find-file-in-project)

;; Modes

;; nginx mode
;; Not going to associate it, just activate with file variables.
(autoload 'nginx-mode "nginx-mode" "A major mode for editing nginx config files" t)

;; SH mode
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-c") '(lambda ()
					       "Pipe the buffer through zsh -vx"
					       (interactive)
					       (shell-command-on-region (point-min) (point-max) "zsh -vx")))))

;; Coffee Mode
(autoload 'coffee-mode "coffee-mode.el" "Coffee mode" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 4))

(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Haskell mode
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; YAML mode
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'\\|\\.mkd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Lua mode for awesome wm config files
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(load-library "mkaito-org")

;; Post Mode
;;		- http://post-mode.sourceforge.net/
(autoload 'post-mode "~/.emacs.d/vendor/post.el" "Major mode for editing e-mail and journal articles" t)
(add-to-list 'auto-mode-alist
	     '("\\.*mutt-*\\|\\.*pico.*\\|\\.*200\\(T\\)?\\|\\.followup" . post-mode))

;; Custom modes for some custom files
;; Shell script mode for Arch PKGBUILDs
(add-to-list 'auto-mode-alist '("\\PKGBUILD$" . sh-mode))

;; Conf mode for personal config files
(when (locate-library "conf-mode")
  (autoload 'conf-mode "conf-mode" "Major-mode for editing config files." t)
  (add-to-list 'auto-mode-alist '("\\awesomerc$" . conf-mode))
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
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-indent-level 4)
	     (setq tab-width 4)))

;; Haml, Sass & Less
(autoload 'haml-mode "haml-mode" "HAML mode" t)
(autoload 'sass-mode "sass-mode" "SASS mode" t)
(add-to-list 'auto-mode-alist '("\\.haml$"	. haml-mode))
(add-to-list 'auto-mode-alist '("\\.scss$"	. sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass$"	. sass-mode))
(add-to-list 'auto-mode-alist '("\\.less$"	. css-mode))

;; Code Folding
;; - http://www.emacswiki.org/emacs/FoldingMode
;;
;; (autoload 'folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode	"folding" "Folding mode" t)
;; (if (load "folding" 'nomessage 'noerror)
;;     (folding-mode-add-find-file-hook))

;; Shortcuts
(defalias 'eb						 'eval-buffer)
(defalias 'er						 'eval-region)
(defalias 'ee						 'eval-expression)
(defalias 'day					 'color-theme-vim-colors)
(defalias 'night				 'color-theme-zenburn)
;; (defalias 'fold					'fold-enter-fold-mode-close-all-folds)

(global-set-key (kbd "M--") 'hippie-expand)

;; Smart comments.
;; - http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
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
(require 'smart-tabs-mode)
(setq-default indent-tabs-mode t)
(setq tab-width 4)

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

;; Indent when pasting
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode		 scheme-mode
						     haskell-mode		 ruby-mode
						     rspec-mode			 python-mode
						     c-mode					 c++-mode
						     objc-mode			 latex-mode
						     plain-tex-mode	 sass-mode
						     haml-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

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

(defun edit-dot-emacs ()
  "Load the .emacs file into a buffer for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-dot-emacs ()
  "Save .emacs, if it is in a buffer, and reload it."
  (interactive)
  (if (bufferp (get-file-buffer "~/.emacs.d/init.el"))
      (save-buffer (get-buffer "~/.emacs.d/init.el")))
  (load-file "~/.emacs.d/init.el"))

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

;; Yasnippet
(require 'yasnippet)
(yas/global-mode 1)
;; (yas/initialize)

;; Place my own snippet stuff in mysnippets
(setq yas/root-directory '("~/.emacs.d/snippets"
			   "~/.emacs.d/vendor/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

;; Fix up for markdown-mode
(add-hook 'markdown-mode-hook
	  (let ((original-command (lookup-key org-mode-map [tab])))
	    `(lambda ()
	       (setq yas/fallback-behavior
		     '(apply ,original-command))
	       (local-set-key [tab] 'yas/expand))))