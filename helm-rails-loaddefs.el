(loop for name in '(models
                    views
                    controllers
                    helpers
                    mailers
                    specs
                    libs
                    javascripts
                    styleshetts
                    all)
      do (progn
           (autoload (intern (format "helm-rails-%S" name)) "helm-rails" nil t)
           (autoload (intern (format "helm-rails-grep-%S" name)) "helm-rails" nil t)))

(autoload 'helm-rails-def-resource "helm-rails" nil t)

(provide 'helm-rails-loaddefs)
