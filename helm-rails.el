;;; helm-rails.el --- Helm extension for Rails projects.

;; Copyright (C) 2012 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/helm-rails
;; Version:           0.1
;; Keywords:          helm, rails, git
;; Package-Requires:  ((helm "1.5.1") (magit "1.2.0") (inflections "1.1"))

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
  '((:name models
	   :re "^app/models/(.+)$"
	   :path "app/models/")
    (:name views
	   :re "^app/views/(.+)$"
	   :path "app/views/")
    (:name controllers
	   :re "^app/controllers/(.+)$"
	   :path "app/controllers/")
    (:name helpers
	   :re "^app/helpers/(.+)$"
	   :path "app/helpers/")
    (:name mailers
	   :re "^app/mailers/(.+)$"
	   :path "app/mailers/")
    (:name specs
	   :re "^spec/(.+_spec\.rb)$"
	   :path "spec/")
    (:name libs
	   :re "^lib/(.+)$"
	   :path "lib/")
    (:name javascripts
	   :re "^(public/javascripts/.+|app/assets/javascripts/.+|lib/assets/javascripts/.+|vendor/assets/javascripts/.+)$"
	   :path "")
    (:name stylesheets
	   :re "^(public/stylesheets/.+|app/assets/stylesheets/.+)$"
	   :path "")
    (:name all
	   :re "^(.+)$"
	   :path "")
    )
  )

(defmacro helm-rails-def-c-source (name path regexp)
  `(defvar ,(intern (format "helm-rails-%S-c-source" name))
     '((name . ,(format "%S" name))
       (disable-shortcuts)
       (init . (lambda ()
		 (helm-init-candidates-in-buffer
		  'local
		  (helm-rails-seded-files ,regexp))))
       (candidates-in-buffer)
       (help-message . helm-generic-file-help-message)
       (candidate-number-limit . 30)
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
						 (helm-rails-current-scope-files
						  (quote ,(intern (format "%S" name)))))))
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
	(singularize-string
	 (catch 'break (loop
			for re in '("app/models/\\(.+\\)\\.rb$"
				    "/\\([a-z_]+\\)_controller\\.rb$"
				    "app/views/\\(.+\\)/[^/]+$"
				    "app/helpers/\\(.+\\)_helper\\.rb$"
				    "spec/.*/\\([a-z_]+?\\)\\(_controller\\)?_spec\\.rb$")
			do (if (string-match re file-name)
			       (throw 'break (match-string 1 file-name)))
			))))
    )
  )

(defun helm-rails-root ()
  "Returns root of the rails git project"
  (expand-file-name "../" (magit-git-dir)))

(defun helm-rails-current-file-relative-path ()
  (let ((file-name (buffer-file-name)))
    (if file-name
	(substring (file-truename (buffer-file-name)) (length (helm-rails-root))))))

(defun helm-rails-git-output (command)
  (let ((file-path (helm-rails-current-file-relative-path))
	 (args (format "git ls-files --full-name -- %s | %s" (helm-rails-root) command))
	 (shell-file-name "/bin/bash"))
     (shell-command-to-string (if file-path (concat args " | grep -v " file-path) args))))

(defun helm-rails-seded-files (regexp)
  "Returns output of git ls-files sed-ed against given regexp.
The regexp should include one match group. Each line of the output
will be truncated to hold only the contents of the match group.
It excludes the currently visiting file."
  (helm-rails-git-output (format "sed -nE 's;%s;\\1;p'" regexp)))

(defun helm-rails-greped-files (regexp)
  "Returns output of git ls-files greped against given regexp.
It excludes the currently visiting file."
  (helm-rails-git-output (format "grep -E %s" regexp)))

(defun helm-rails-current-scope-files (target)
  (let ((current-resource (helm-rails-current-resource)))
    (if current-resource
  	(helm-rails-greped-files
	 (cond ((equal target 'models)
		(format "app/models/%s\.rb" current-resource))
	       ((equal target 'javascripts)
	       	(format "app/assets/javascripts/\\(.+/\\)?%s\\..+" (pluralize-string current-resource)))
	       ((equal target 'stylesheets)
	       	(format "app/assets/stylesheets/\\(.+/\\)?%s\\..+" (pluralize-string current-resource)))
	       ((equal target 'controllers)
	       	(format "app/controllers/\\(.+/\\)?%s_controller\\.rb" (pluralize-string current-resource)))
	       ((equal target 'helpers)
	       	(format "app/helpers/%s_helper\.rb" (pluralize-string current-resource)))
	       ((equal target 'views)
	       	(format "app/views/\\(.+/\\)?%s/[^/]+" (pluralize-string current-resource)))
	       ((equal target 'specs)
	       	(format "spec/.*\\(%s_controller\\|%s\\|%s_helper\\)_spec\\.rb"
			(pluralize-string current-resource)
			current-resource
			(pluralize-string current-resource)))
	       )
	 )
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
	     (helm-rails-def-c-source
	      ,(plist-get resource :name)
	      ,(plist-get resource :path)
	      ,(plist-get resource :re))

	     (helm-rails-def-current-scope-c-source
	      ,(plist-get resource :name))

	     (helm-rails-def-command
	      ,(plist-get resource :name) ))
	  )
      )

(provide 'helm-rails)

;;; helm-rails.el ends here
