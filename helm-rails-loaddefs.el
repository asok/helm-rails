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
      do (autoload (intern (format "helm-rails-%S" name)) "helm-rails" nil t))
