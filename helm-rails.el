;;; helm-rails.el --- Helm extension for Rails projects.

;; Copyright (C) 2012 Adam Sokolnicki

;; Author   : Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL      : https://github.com/asok/helm-rails
;; Version  : 0.1
;; Keywords : helm, rails, git

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Helm Rails extension provides snappy navigation through rails
;; projects. It is depending on magit for quering Git binary
;; for a list of candidates. It is possible to traverse through
;; resource and files related to the current file.
;;
;;; Code:

(require 'helm)
(require 'magit)
(require 'inflections)

(defvar helm-rails-resources-schema
  '((models  "app/models/")
    (views  "app/views/")
    (controllers  "app/controllers/")
    (helpers  "app/helpers/")
    (mailers  "app/mailers/")
    (specs  "spec/" "_spec\\.rb$")
    (libs  "lib/")
    (javascripts  "public/javascripts/")
    (stylesheets  "public/stylesheets/")
    ))

(defvar helm-rails-other-files-exclude-regexp
  (format "^\\(%s\\).+\n" (mapconcat
			   'identity
			   (mapcar 'second helm-rails-resources-schema)
			   "\\|"))
  )

(defvar helm-rails-other-c-source
  '((name . "Other files")
    (disable-shortcuts)
    (init . (lambda ()
	      (helm-init-candidates-in-buffer 'local (helm-rails-other-files))))
    (candidates-in-buffer)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 10)
    (mode-line . helm-generic-file-mode-line-string)
    (action . (lambda (c)
		(find-file (concat (helm-rails-root) c))))
    (type . file))
  )

(defmacro helm-rails-def-c-source (name path &optional regexp)
  `(defvar ,(intern (format "helm-rails-%S-c-source" name))
     '((name . ,(format "%S" name))
       (disable-shortcuts)
       (init . (lambda ()
		 (helm-init-candidates-in-buffer 'local
						 (mapcar (lambda (c) (substring c (length ,path)))
							 (helm-rails-files ,path ,regexp)))))
       (candidates-in-buffer)
       (help-message . helm-generic-file-help-message)
       (candidate-number-limit . 10)
       (mode-line . helm-generic-file-mode-line-string)
       (action . (lambda (c)
		   (find-file (concat (helm-rails-root) ,path c))))
       (type . file)))
  )

(defmacro helm-rails-def-current-scope-c-source (name)
  `(defvar ,(intern (format "helm-rails-current-scope-%S-c-source" name))
     '((name . "current scope")
       (disable-shortcuts)
       (init . (lambda ()
  		 (helm-init-candidates-in-buffer 'local
						 (helm-rails-current-scope-files (quote ,(intern (format "%S" name)))))))
       (candidates-in-buffer)
       (help-message . helm-generic-file-help-message)
       (candidate-number-limit . 10)
       (mode-line . helm-generic-file-mode-line-string)
       (action . (lambda (c)
  		   (find-file (concat (helm-rails-root) c))))
       (type . file)))
  )

(defmacro helm-rails-def-command (name)
  `(defun ,(intern (format "helm-rails-%S" name)) ()
     ,(format "Search for %S" name)
     (interactive)
     (unless (helm-rails-project-p)
       (error "Not inside a rails git repository"))
     (helm :sources (list
		     ,(intern (format "helm-rails-current-scope-%S-c-source" name))
		     ,(intern (format "helm-rails-%S-c-source" name))
		     )
	   :prompt ,(format "%S: " name))
     )
  )

(defun helm-rails-current-resource ()
  "Returns a resource name extracted from the name of the currently visiting file"
  (let ((file-name (buffer-file-name)))
    (if file-name
	(singularize-string (catch 'break (loop for re in '("app/models/\\(.+\\)\\.rb$"
							    "/\\([a-z]+\\)_controller\\.rb$"
							    "app/views/\\(.+\\)/[^/]+$"
							    "app/helpers/\\(.+\\)_helper\\.rb$"
							    "spec/.*/\\([a-z]+\\)\\(_controller\\)?_spec\\.rb$")
						do (if (string-match re file-name)
						       (throw 'break (match-string 1 file-name)))
						))))
    )
  )

(defun helm-rails-root ()
  "Returns root of the rails git project"
  (expand-file-name "../" (magit-git-dir)))

;todo: this should return output from magit-git-output but grepped against REGEXP (but how?)
(defun helm-rails-files (path &optional regexp)
  "Returns a *list* of the files from supplied PATH and matched against supplied REGEXP"
  (let ((list (magit-split-lines (helm-rails-sub-magit-output path))))
    (if regexp
	(delete-if-not (lambda (c) (string-match-p regexp c)) list)
      list)))

(defun helm-rails-sub-magit-output (&optional subpath)
  "Returns output of git ls-files from supplied SUBPATH called via magit"
  (magit-git-output `("ls-files" "--full-name" "--" ,(concat (helm-rails-root) subpath))))

(defun helm-rails-other-files ()
  "Returns git output for all other files than the ones from `helm-rails-resources-schema'"
  (replace-regexp-in-string
   helm-rails-other-files-exclude-regexp
   ""
   (helm-rails-sub-magit-output)))

(defun helm-rails-current-scope-files (target)
  (let ((current-resource (helm-rails-current-resource)))
    (if current-resource
  	(apply
	 'helm-rails-files
	 (cond ((equal target 'models)
		`("app/models/" ,(concat current-resource "\\.rb$")))
	       ((equal target 'controllers)
		`("app/controllers/" ,(concat (pluralize-string current-resource) "_controller\\.rb$")))
	       ((equal target 'helpers)
		`("app/helpers/" ,(concat (pluralize-string current-resource) "_helper\\.rb$")))
	       ((equal target 'views)
		`("app/views/" ,(concat (pluralize-string current-resource) "/[^/]+$")))
	       ((equal target 'specs)
		`("spec/" ,(format "\\(%s_controller\\|%s\\|%s_helper\\)_spec\\.rb"
				   (pluralize-string current-resource)
				   current-resource
				   (pluralize-string current-resource))))
	       ))
      '()
      )
    )
  )

(defun helm-rails-project-p ()
  "Returns t if we are inside a rails git repository"
  (condition-case nil
      (file-exists-p (expand-file-name
		      "environment.rb" (expand-file-name "../config" (magit-git-dir))))
    (error nil)))

(loop for resource in 
      helm-rails-resources-schema
      do (eval
	  `(progn
	     (helm-rails-def-c-source ,(first resource) ,(second resource) ,(third resource))
	     (helm-rails-def-current-scope-c-source ,(first resource))
	     (helm-rails-def-command ,(first resource) ))
	  )
      )

(defun helm-rails-all ()
  "Search for all files in the rails project"
  (interactive)
  (unless (helm-rails-project-p)
    (error "Not inside a rails git repository"))
  (helm :sources '(helm-rails-models-c-source
		   helm-rails-views-c-source
		   helm-rails-controllers-c-source
		   helm-rails-helpers-c-source
		   helm-rails-libs-c-source
		   helm-rails-specs-c-source
		   helm-rails-javascripts-c-source
		   helm-rails-stylesheets-c-source
		   helm-rails-other-c-source)
	)
  )

(provide 'helm-rails)

;;; helm-rails.el ends here
