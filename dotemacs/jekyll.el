;; mkaito's jekyll.el
;;
;; Emacs glue for Jekyll blogs.
;; Based on Metajack's jekyll.el
;; 
;; This file provides a few handy functions to create new posts for
;; your Jekyll-powered blog. The primary function is
;; jekyll-make-new-post, which will prompt you for a title, insert it
;; in the yaml preface, and use a slugified version for the file
;; name. All other data my functions insert are subjective, like the
;; current date in the meta property, or extracting links from the
;; clipboard when creating a link-type post. The code is pretty
;; simple, just dive in.
;; 
;; Creating a blog post, will create a file in _posts instead of
;; _drafts with "published" set to false. This is because I tend to
;; forget keeping up with my drafts. YMMV. I use a shell alias to grep
;; my posts for "published: false" and return a file list, so I know
;; what I have pending.
;;
;; To use, just put this file somewhere in the load path and
;; (require 'jekyll)
;;
;; You might want to redefine the templates below.
;;
;; Default key bindings:
;;   C-c j p - create new post
;;   C-c j l - create new link

(provide 'jekyll)

(defvar jekyll-directory "~/src/web/mkaito/"
  "Path to Jekyll blog.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".markdown"
  "File extension of Jekyll posts.")
(defvar jekyll-post-template
  "---
title: %s
meta: %s &mdash;
byline: 
layout: post
published: false
---
"
  "Default template for Jekyll posts.")
(defvar jekyll-link-template
    "---
title: %s
meta: %s &mdash; 
link: %s
byline: 
---\n"
    "Default template for link posts.")

(defun current-date (&optional format locale)
  "Inserts the current date"
  (if locale
    (setq system-time-locale locale))
  (unless format 
    (setq format "%B %d %Y"))
  (capitalize (format-time-string format)))

(defun urlp (string)
  (and (stringp string)
       (string-match (rx (* white) (group (| "http" "https" "ftp" "file")
                                          "://" (* (not (any white)))))
                     string)
       (substring-no-properties (match-string 1 string))))

(defun get-url-from-clipboard-if ()
  (interactive)
  (or (urlp (and (x-selection-exists-p) (x-get-selection)))
                          (urlp (and (x-selection-exists-p 'CLIPBOARD)
                                     (x-get-selection 'CLIPBOARD)))))

(defun jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun jekyll-make-new-post (title)
  (interactive "sPost Title: ")
  (let ((post-file (concat jekyll-directory jekyll-posts-dir
			   (format-time-string "%Y-%m-%d-")
			   (jekyll-make-slug title)
			   jekyll-post-ext)))
    (if (file-exists-p post-file)
        (find-file post-file)
      (find-file post-file)
      (insert (format jekyll-post-template
		      (jekyll-yaml-escape title)
		      (current-date nil "C"))))))

(defun jekyll-make-new-link (title)
  (interactive "sLink Title: ")
  (let ((link-file (concat jekyll-directory jekyll-posts-dir
			   (format-time-string "%Y-%m-%d-")
			   (jekyll-make-slug title)
			   jekyll-post-ext)))
    (if (file-exists-p link-file)
        (find-file link-file)
      (find-file link-file)
      (insert (format jekyll-link-template
		      (jekyll-yaml-escape title)
		      (current-date nil "C")
		      (or (get-url-from-clipboard-if) "" ))))))

(global-set-key (kbd "\C-c j p") 'jekyll-make-new-post)
(global-set-key (kbd "\C-c j l") 'jekyll-make-new-link)
