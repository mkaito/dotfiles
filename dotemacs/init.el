;;; dotemacs. --- do stuff
;;; commentary:
;;
;; large chunks shamelessly copied from anrxc's dotemacs:
;;	 http://git.sysphere.org/dotfiles/tree/emacs
;; my emacs wouldn't be half as pleasant without his help.
;;

;;; code:
(with-no-warnings (require 'cl))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defvar wanted-packages
  '(color-theme-approximate expand-region smart-tabs-mode d-mode
  flx-ido gist magit haml-mode haskell-mode inf-ruby lua-mode
  markdown-mode paredit projectile js2-mode sass-mode rainbow-mode
  scss-mode ack-and-a-half yaml-mode nginx-mode flycheck helm
  helm-projectile flycheck-d-unittest flycheck-dmd-dub ledger-mode
  flycheck-ledger flycheck-haskell flycheck-hdevtools evil evil-leader
  evil-numbers evil-surround evil-nerd-commenter key-chord smartparens
  exec-path-from-shell guide-key slime indent-guide)
  "A list of packages to ensure are installed at launch.")

(defun wanted-packages-installed-p ()
  "Check whether wanted-packages are all installed."
  (loop for p in wanted-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (wanted-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p wanted-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Get relevant environment variables from the shell
(exec-path-from-shell-initialize)

;; clean initial buffers, windows.
(setq message-log-max nil)
(defun buffer-exists (bufname)
  "Check whether a buffer BUFNAME exists."
  (not (eq nil (get-buffer bufname))))
(if (buffer-exists "*compile-log*") (kill-buffer "*compile-log*"))
(if (buffer-exists "*backtrace*") (kill-buffer "*backtrace*"))
(if (buffer-exists "*Messages*") (kill-buffer "*Messages*"))
(if (buffer-exists "*Warnings*") (kill-buffer "*Warnings*"))
(delete-other-windows)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;auto save and backups
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

(setq auto-save-interval 0
      auto-save-timeout 4
;;      auto-save-default t
      auto-save-visited-file-name t)

;; fancy automatic buffer cleanup
(require 'midnight)

;; restore cursor position
(setq-default save-place-file (concat user-emacs-directory "saveplace.el")
	      save-place t)
(require 'saveplace)

;; fix dead keys
(load-library "iso-transl")

;; colour theme initialization
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16")
(load-theme 'base16-chalk-dark t)
(color-theme-approximate-on)

(global-hl-line-mode 1)
;; (set-face-background 'hl-line   "#0f0f0f")
;; (set-face-background 'default   "#1f1f1f")

;; always flash for parens and define a more distinctive color
(show-paren-mode 1)
;; (set-face-foreground 'show-paren-match-face "#bc8383")
;; (set-face-background 'show-paren-match-face "#1f1f1f")

;; support 256 colours in screen
;;	 - http://article.gmane.org/gmane.emacs.devel/109504/
(if (not (window-system)) (load "term/rxvt"))
(defun terminal-init-screen ()
  "terminal initialization function for screen."
  ;; use the rxvt color initialization code.
  (rxvt-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; don't show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; show column number in modeline
(setq column-number-mode t)

;; Indent guide
(setq indent-guide-recursive t)

;; friendlier copy/paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; answer y or n instead of yes or no at prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; set executable bit on scripts after saving
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; spell correction
(require 'ispell)
(defvar ispell-prefer-aspell)
(defvar ispell-list-command)
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

;; evil mode
(global-evil-leader-mode)
(evil-mode +1)
(global-evil-surround-mode +1)
(key-chord-mode +1)

;; make the linter shut up
(defvar evil-normal-state-map)
(defvar evil-visual-state-map)
(defvar evil-insert-state-map)
(defvar evil-motion-state-map)
(defvar evil-auto-indent)
(defvar evil-regexp-search)
(defvar evil-want-C-i-jump)

;; Evil settings
(setq evil-auto-indent t)
(setq evil-regexp-search t)
(setq evil-want-C-i-jump t)

;; set the leader key
(evil-leader/set-leader "<SPC>")

;; escape quits everything
(defun minibuffer-keyboard-quit ()
  "abort recursive edit.
in delete selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*completions*") (delete-windows-on "*completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map           [escape] #'keyboard-quit)
(define-key evil-visual-state-map           [escape] #'keyboard-quit)
(define-key minibuffer-local-map            [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] #'minibuffer-keyboard-quit)

;; miscelaneous bindings
(evil-leader/set-key
  "f" 'helm-for-files     ; I have no idea what the difference is between this and the next
  "e" 'helm-find-files
  "b" 'helm-mini          ; Apparently, 'mini' means 'buffers'.
  "t" 'helm-projectile    ; Find file in project
  "y" 'helm-show-killring ; Recent kills
  "k" 'kill-buffer        ; What it says on the tin
  "g" 'magit-status       ; I have yet to get into magit.
  "c" 'evilnc-comment-or-uncomment-lines
  "oa" 'org-agenda-list
  "ob" 'org-switchb)

;; jj calls normal mode
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; C-e to swap to last buffer
(define-key evil-normal-state-map (kbd "C-e") 'evil-buffer)
(define-key evil-motion-state-map (kbd "C-e") 'evil-buffer)
(define-key evil-insert-state-map (kbd "C-e") 'evil-buffer)

;; Sane window navigation
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

;; Good ole number nudging
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-killring)

;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Ledger mode
(defvar ledger-mode-map)
(evil-define-key 'normal ledger-mode-map
  (kbd "C-t") 'ledger-toggle-current-transaction
  (kbd "C-d") 'ledger-insert-effective-date)

(defvar org-mode-map)
(evil-define-key 'normal org-mode-map
  (kbd "<tab>") 'org-cycle
  (kbd "<M-RET>") 'org-insert-heading
  (kbd "C-c s i") 'org-insert-src-block)

;; Enter insert mode in capture windows
;; (evil-set-initial-state 'org-capture-mode 'insert)
;; Seems the above doesn't work...
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Encrypted file editing
(epa-file-enable)

;; Fuzzy matching and all that jazz
(defvar ido-enable-flex-matching)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Increase the GC threshold to 20Mb
(setq gc-cons-threshold 20000000)

;; disable ido faces to see flx highlights
(defvar ido-use-faces)
(setq ido-use-faces nil)

;; Projectile mode
(projectile-global-mode)
(defvar projectile-use-native-indexing)
(setq projectile-use-native-indexing t)
(defvar projectile-enable-caching)
(setq projectile-enable-caching t)
;;(recentf-mode 1)

;; Coffee Mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "Coffee-mode-hook."
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Haskell mode
;; (load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; YAML mode
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Markdown mode
(add-to-list 'auto-mode-alist '("\\.markdown\\'\\|\\.mkd\\|\\.md\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'tab-width) 2)))

;; Lua mode for awesome wm config files
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(load "~/.emacs.d/settings-org-mode.el")

;; Post Mode
(add-to-list 'auto-mode-alist
	     '("\\.*mutt-*" . mail-mode))
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
(add-hook 'ruby-mode-hook 'indent-guide-mode)

;; Python mode
(add-hook 'python-mode-hook 'indent-guide-mode)
(setq python-indent-offset nil)

(global-set-key (kbd "M--") 'hippie-expand)

(defvar scss-compile-at-save)
(defvar scss-output-directory)
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

;; Renames the current file and updates the buffer
(defun rename-current-file-or-buffer ()
  "Rename current file or buffer."
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

;; Tabs, thank you.
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

;; This should cover mostly everything...
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; ... but ruby-mode thinks otherwise.
;; (add-hook 'ruby-mode-hook
;; 	  '(lambda()
;; 	     (setq tab-width 2)
;; 	     (local-set-key (kbd "RET") 'newline-and-indent)))

;; (defadvice kill-line (before check-position activate)
;;   "When killing the end of a line, eliminate any indentation of the next line."
;;   (if (and (eolp) (not (bolp)))
;;       (progn (forward-char 1)
;; 	     (just-one-space 0)
;; 	     (backward-char 1))))

;; (defun open-line-below-and-go-there ()
;;   "Open new line below current and place cursor indented."
;;   (interactive)
;;   (move-end-of-line nil)
;;   (newline-and-indent))
;; (global-set-key (kbd "M-RET") 'open-line-below-and-go-there)

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (tabify (point-min) (point-max)))

;{{{ Reload or edit .emacs on the fly
;    - key bindings defined below
;
(defun aic-reload-dot-emacs ()
  "Reload user configuration from .emacs."
  (interactive)
  ;; Fails on killing the Messages buffer, workaround:
  (get-buffer-create "*Messages*")
  (load-file "~/.emacs.d/init.el")
)
(defun aic-edit-dot-emacs ()
  "Edit user configuration in .emacs."
  (interactive)
  (find-file "~/.emacs.d/init.el")
)
;}}}

;; Reload or edit .emacs as defined above
(global-set-key "\C-c\C-r" 'aic-reload-dot-emacs)
(global-set-key "\C-c\C-e" 'aic-edit-dot-emacs)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)
;; (eval-after-load 'flycheck '(require 'flycheck-ledger))

;; Ledger
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ldg$"    . ledger-mode))

;;;;;;;;;;;;;;
;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;; Disable overlay highlighting
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

;; Helper to right-align modeline values
(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

;; Clean modeline
(setq-default mode-line-format
      (list
       ;; Current buffer name
       "%b "
       ;; Major mode
       "[%m] "
       ;; Modified status
       "[%*] "
       ;; Pad for right-aligning
       (mode-line-fill 'mode-line 15)
       ;; Cursor position
       "[l:%4l | c:%2c]"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("1e53a10139245e5fbce825610f967d993fb36ba7154d4eea754c5191a9464b70" "d341a4b6446371a5267627fcb3e21798a49112244b2ee9f36ce67f2d900ca46f" "ac263b162c91d0df61285c927b830fb70092946381200be3686a3c9b98e43413" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "d4ed3b0f53e4b867dcd6afe1e8224c8568bee2295b8e6c5cecca99392cca5bda" "7c2fdd4d512b1fe016292708c454031979e5c38a11a7365efdd12aa4c6ad000e" default)))
 '(inhibit-startup-echo-area-message "chris")
 '(ledger-post-auto-adjust-amounts t)
 '(ledger-post-use-completion-engine :ido)
 '(markdown-content-type "text/html")
 '(python-shell-interpreter "/home/chris/.pyenv/shims/python")
 '(safe-local-variable-values
	 (quote
		((org-archive-location . "archive.org::* %s")
		 (org-archive-location . "~/notes/archive/archived_%s")
		 (org-archive-location . "archive/archived_%s")
		 (nil)
		 (ispell-local-dictionary . spanish)
		 (markdown-content-type . "text/html")
		 (encoding . utf-8)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
