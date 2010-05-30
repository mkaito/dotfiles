;; mkaito's dot-emacs on Arch Linux.
;; Latest modification: May 28 2010.

;;{{{ IDO
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
;;}}}

;;{{{ Colours
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
;;}}}

;{{{ Modes

;; Lua mode for awesome wm config files
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
;}}}

;;{{{ Code Folding
;;   - http://www.emacswiki.org/emacs/FoldIngo
(require 'foldingo)
(fold-enter-mode)
;;}}}

;{{{ Shortcut a few commonly used functions
;
(defalias 'cr            'comment-region)
(defalias 'ucr           'uncomment-region)
(defalias 'eb            'eval-buffer)
(defalias 'er            'eval-region)
(defalias 'ee            'eval-expression)
(defalias 'day           'color-theme-vim-colors)
(defalias 'night         'color-theme-zenburn)
(defalias 'fold          'fold-enter-fold-mode-close-all-folds)
;}}}

;{{{ Key bindings

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
;}}}
