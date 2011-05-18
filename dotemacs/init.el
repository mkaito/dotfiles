;; mkaito's dot-emacs on Arch Linux.
;;
;; Large chunks shamelessly copied from anrxc's dotemacs:
;;   http://git.sysphere.org/dotfiles/tree/emacs
;; My Emacs wouldn't be half as pleasant without his help.
;;
;; Latest modification: May 15 2011.

;; Actually, what do I want from Emacs?
;;
;; * Syntax highlighting for: Markdown, Ruby, JavaScript, Lua, HAML,
;;   SASS/SCSS/CSS, HTML.
;; * Code folding with markers (I don't like the automatic ones)
;; * Clean interface, low contrast colour theme.
;; * Snippet expansion
;; 
;; Organization:
;; 
;; init.el should contain all common configuration. One file per major
;; mode, containing all and any configuration relating to this mode,
;; and one file per major configuration topic.

;; {{{ Base configuration
;;   {{{ Load path
(setq load-path
      (append
       (list
	"~/.emacs.d"
        "~/.emacs.d/vendor"
        "~/.emacs.d/vendor/twittering-mode"
        "~/.emacs.d/vendor/haml-mode"
        "~/.emacs.d/vendor/sass-mode"
        )
        load-path))
;;   }}}

;;   {{{ Recompile packages
;; Check for stale .elc on startup. Slow, but I only restart Emacs after reboots.
(byte-recompile-directory "~/.emacs.d/vendor")
;;   }}}

;;   {{{ Visuals
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;; Colour theme initialization
;;   - http://emacswiki.org/cgi-bin/wiki/ColorTheme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;;
;; Load preferred theme
;;   - http://www.brockman.se/software/zenburn/zenburn.el
(require 'color-theme-wombat)
(color-theme-wombat)

;; Support 256 colours in screen
;;   - http://article.gmane.org/gmane.emacs.devel/109504/
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

;; Modeline setup
;;   - somewhat cleaner than default
(setq mode-line-format
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil))))
;;   }}}

;;   {{{ Spell correction
(require 'ispell)
(setq ispell-prefer-aspell t)
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast"))
(setq ispell-dictionary "british-ize")

(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(global-set-key (kbd "\C-c C-4") 'flyspell-correct-word-before-point)
(global-set-key (kbd "\C-c C-\\") 'flyspell-correct-word-before-point)
;; TODO: Add flyspell-mode and flyspell-prog-mode hooks
;;   }}}

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

;; Proper auto save
(require 'real-auto-save)
;; TODO: Move these hooks to wherever their mode is configured
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'ruby-mode-hook 'turn-on-real-auto-save)
(add-hook 'markdown-mode-hook 'turn-on-real-auto-save)

(setq real-auto-save-interval 2) ;; in seconds

;; Split window navigation: S-arrow
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; }}}

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

;; {{{ Modes

;; {{{ YAML mode
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
;; }}}

;; {{{ Textmate (Minor) Mode
(autoload 'textmate-mode "textmate" "Textmate mode" t)
;; TODO: Call textmate-mode off hooks for programming modes
;; (textmate-mode)
;; }}}

;; {{{ Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'\\|\\.mkd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
;; }}}

;; {{{ LUA Mode
;; Lua mode for awesome wm config files
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
;; }}}

(load-library "mkaito-org")

;; {{{ Post Mode
;;    - http://post-mode.sourceforge.net/
(autoload 'post-mode "~/.emacs.d/post.el" "Major mode for editing e-mail and journal articles" t)
(add-to-list 'auto-mode-alist
             '("\\.*mutt-*\\|\\.*pico.*\\|\\.*200\\(T\\)?\\|\\.followup" . post-mode))
;; }}}

;; {{{ Custom modes for some custom files
                                        ;
;; Shell script mode for Arch PKGBUILDs
(add-to-list 'auto-mode-alist '("\\PKGBUILD$" . sh-mode))

;; Conf mode for personal config files
(when (locate-library "conf-mode")
  (autoload 'conf-mode "conf-mode" "Major-mode for editing config files." t)
  (add-to-list 'auto-mode-alist '("\\awesomerc$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\gitconfig$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\screenrc$"  . conf-mode))
  (add-to-list 'auto-mode-alist '("\\pinerc$"    . conf-mode))
  (add-to-list 'auto-mode-alist '("\\zshrc$"     . conf-mode))
  )
;;
;; Ruby mode for Gemfile and config.ru
(add-to-list 'auto-mode-alist '("\\Gemfile$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\config\.ru$"  . ruby-mode))

;; Ruby mode hooks
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook 'textmate-mode)
;; }}}

;; {{{ Haml, Sass & Less
(autoload 'haml-mode "haml-mode" "HAML mode" t)
(autoload 'sass-mode "sass-mode" "SASS mode" t)
(add-to-list 'auto-mode-alist '("\\.haml$"        . haml-mode))
(add-to-list 'auto-mode-alist '("\\.scss$"        . sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass$"        . sass-mode))
(add-to-list 'auto-mode-alist '("\\.less$"        . css-mode))
;; }}}

;; {{{ Twittering mode
;; https://github.com/hayamiz/twittering-mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/twittering-mode")
(autoload 'twit "twittering-mode" "Twittering mode" t)
(setq twittering-use-master-password t      ; Don't bother me with the damn PIN.
      twittering-icon-mode t                ; Show icons
      twittering-scroll-mode t              ; Keep the cursor in place upon updates
      twittering-timer-interval 60          ; Update your timeline each 60 seconds (1 minute)
      twittering-url-show-status nil        ; Keeps the echo area from showing all the http processes
      twittering-status-format "%i %s (%S),  %@:\n%FOLD[  ]{%T // from %f%L%r%R}\n "
      twittering-number-of-tweets-on-retrieval 30

      twittering-tinyurl-service 'is.gd)

;; Spell checking on tweet editing
(add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode)))
;;
;; Display a desktop notification upon tweet arrival.
(autoload 'todochiku-message "todochiku" "Todochiku" t)
(add-hook 'twittering-new-tweets-hook (lambda ()
                                        (let ((n twittering-new-tweets-count))
                                          (if (> n 2)
                                              (todochiku-message
                                               (twittering-timeline-spec-to-string twittering-new-tweets-spec)
                                               (format "You have %d new tweet%s"
                                                       n (if (> n 1) "s" ""))
                                               (todochiku-icon 'social))
                                            (dolist (el twittering-new-tweets-statuses)
                                              (todochiku-message
                                               (twittering-timeline-spec-to-string twittering-new-tweets-spec)
                                               (concat (cdr (assoc 'user-screen-name el))
                                                       " said: "
                                                       (cdr (assoc 'text el)))
                                               (todochiku-icon 'social)))))))
;;
(setq twittering-initial-timeline-spec-string
      '("michishigekaito/fansubs"
	"michishigekaito/animanga"
	"michishigekaito/design"
	"michishigekaito/code"
        ":direct_messages"
        ":mentions"
        ":home"))

;; This should cause the pinentry dialog to occur in the minibuffer
;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))
;; }}}

;; }}}

;; {{{ Code Folding
;; http://www.emacswiki.org/emacs/FoldingMode
;;
;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
(if (load "folding" 'nomessage 'noerror)
    (folding-mode-add-find-file-hook))
;; }}}

;; {{{ Shortcuts
;;
(defalias 'eb            'eval-buffer)
(defalias 'er            'eval-region)
(defalias 'ee            'eval-expression)
(defalias 'day           'color-theme-vim-colors)
(defalias 'night         'color-theme-zenburn)
;; (defalias 'fold          'fold-enter-fold-mode-close-all-folds)
;; }}}

;; {{{ Misc

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

;; {{{ Auto-indent stuff

;; This should cover mostly everything...
(define-key global-map (kbd "RET") 'newline-and-indent)
;; ... but ruby-mode thinks otherwise.
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'ruby-mode-hook 'set-newline-and-indent)

;; Indent when pasting
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode  sass-mode
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
(define-key global-map (kbd "M-RET") 'open-line-below-and-go-there)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;; }}}

;; {{{ Yasnippet
(require 'yasnippet)
(yas/initialize)

;; Place my own snippet stuff in mysnippets
(setq yas/root-directory '("~/.emacs.d/mysnippets"
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
;; }}}
;; }}}
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "/usr/bin/firefox"))