(loop for name in '(models
                    views
                    controllers
                    helpers
                    mailers
                    specs
                    libs
                    javascripts
                    styleshetts
                    all
                    def-resource)
      do (autoload (intern (format "helm-rails-%S" name)) "helm-rails" nil t))

(provide 'helm-rails-loaddefs)
