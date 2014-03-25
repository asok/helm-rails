(require 'magit)

 ;; stolen from https://github.com/maio/helm-git/blob/master/tests/test-helpers.el
(defmacro with-temp-git-repo (repo &rest body)
  (declare (indent 1) (debug t))
  `(let* ((,repo (make-temp-file "tmp_git" t))
          (default-directory (concat ,repo "/")))
     (unwind-protect
         (progn
           (magit-init repo)
           ,@body)
       (delete-directory ,repo t)
       )))

(defun create-file-in-repo (repo dummy-file)
  (with-temp-buffer
    (write-file (format "%s/%s" repo dummy-file))
    (cd (expand-file-name ".." (magit-git-dir)))
    (magit-run-git "add" dummy-file)))

(defun helm-rails-split-lines (lines)
  (split-string
   (replace-regexp-in-string "\n$" "" lines) "\n"))
