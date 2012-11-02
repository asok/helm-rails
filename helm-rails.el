(require 'magit)
(require 'helm)
(require 'inflections)

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
							 (helm-rails-files ,path)))))
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
     ;; (unless (magit-git-repo-p default-directory)
     ;;   (error "Not inside git repository"))
     (helm :sources (list
		     ,(intern (format "helm-rails-current-scope-%S-c-source" name))
		     ,(intern (format "helm-rails-%S-c-source" name))
		     )
	   :prompt ,(format "%S: " name))
     )
  )

(defun helm-rails-all ()
  "Search for all files in the rails projecte"
  (interactive)
  ;; (unless (magit-git-repo-p default-directory)
  ;;   (error "Not inside git repository"))
  (helm :sources '(helm-rails-models-c-source
		   helm-rails-views-c-source
		   helm-rails-controllers-c-source
		   helm-rails-helpers-c-source
		   helm-rails-libs-c-source
		   helm-rails-specs-c-source
		   helm-rails-other-c-source)
	)
  )

(loop for args in 
      '((models  "app/models/" "\\.rb$")
	(views  "app/views/" "\\.\\(haml\\|erb\\)$")
	(controllers  "app/controllers/" "\\.rb$")
	(helpers  "app/helpers/" "\\.rb$")
	(mailers  "app/mailers/" "\\.rb$")
	(specs  "spec/" "_spec\\.rb$")
	(libs  "lib/" "\\.rb$")
	(javascripts  "public/javascripts/" "\\.js$")
	(stylesheets  "public/stylesheets/" "\\.css$")
	)
      do (eval
	  `(progn
	     (helm-rails-def-c-source ,(first args) ,(second args) ,(third args))
	     (helm-rails-def-current-scope-c-source ,(first args))
	     (helm-rails-def-command ,(first args) ))
	  )
      )

(defun helm-rails-current-resource ()
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
  (expand-file-name "../" (magit-git-dir)))

(defun helm-rails-files (path &optional regexp)
  (let ((list (helm-rails-sub-magit-lines path)))
    (if regexp
	(delete-if-not (lambda (c) (string-match-p regexp c)) list)
      list)))

(defun helm-rails-sub-magit-lines (subpath)
  (magit-git-lines "ls-files" "--full-name" "--" (concat (helm-rails-root) subpath)))

(defun helm-rails-other-files ()
  (replace-regexp-in-string
   "^\\(app/models/\\|app/controllers/\\|app/views/\\|app/helpers/\\|lib/\\|spec/\\).+\n"
   ""
   (magit-git-output `("ls-files" "--full-name" "--" ,(helm-rails-root)))))

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

(provide 'helm-rails)
