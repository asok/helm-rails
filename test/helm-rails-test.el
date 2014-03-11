(expectations
 (desc "helm-rails-def-resource"
       (expect '(t t t t)
               (helm-rails-def-resource 'jobs "app/jobs/" "^app/jobs/(.+)$")
               (list
                (fboundp 'helm-rails-jobs)
                (boundp 'helm-rails-current-scope-jobs-c-source)
                (boundp 'helm-rails-jobs-c-source)
                (fboundp 'helm-rails-grep-jobs)))
       )

 (desc "helm-rails-current-file-relative-path"
       (expect "spec/models/user_spec.rb"
               (find-file "spec/models/user_spec.rb")
               (helm-rails-file-relative-path (buffer-file-name)))
       )

 (desc "helm-rails-seded-files"
       (expect "profile.rb\ncriterion.rb\nuser.rb\n"
	       (find-file "app/models/user.rb")
	       (helm-rails-seded-files "app/models/(.+)"))
       )

 (desc "helm-rails-greped-files"
       (expect "app/models/user.rb\n"
               (find-file "app/models/criterion.rb")
               (helm-rails-greped-files "app/models/user.rb"))
       )

 (desc "helm-rails-current-resource"
       (desc "being in models scope"
             (expect "user"
        	     (find-file "app/models/user.rb")
        	     (helm-rails-current-resource)))
       (desc "being in helpers scope"
             (expect "user"
        	     (find-file "app/helpers/users_helper.rb")
        	     (helm-rails-current-resource)))
       (desc "being in controllers scope"
             (expect "user"
        	     (find-file "app/controllers/users_controller.rb")
        	     (helm-rails-current-resource)))
       (desc "being in namespaced controllers scope"
             (expect "user"
        	     (find-file "app/controllers/foo/bar/users_controller.rb")
        	     (helm-rails-current-resource)))
       (desc "being in erb views scope"
             (expect "user"
        	     (find-file "app/views/users/index.html.erb")
        	     (helm-rails-current-resource)))
       (desc "being in haml views scope"
             (expect "user"
        	     (find-file "app/views/users/index.html.haml")
        	     (helm-rails-current-resource)))
       (desc "being in model specs scope"
             (expect "user"
        	     (find-file "spec/models/user_spec.rb")
        	     (helm-rails-current-resource)))
       (desc "being in controllers specs scope"
             (expect "user"
        	     (find-file "spec/controllers/users_controller_spec.rb")
        	     (helm-rails-current-resource)))
       (desc "being in controllers with irregular noun specs scope"
             (expect "person"
        	     (find-file "spec/controllers/people_controller_spec.rb")
        	     (helm-rails-current-resource)))
       )

 (desc "helm-rails-current-scope-files"
       (desc "finding controller from model"
             (expect '("app/controllers/admin/users_controller.rb"
        	       "app/controllers/users_controller.rb")
        	     (find-file "app/models/user.rb")
        	     (split-string
		      (replace-regexp-in-string "\n$" "" (helm-rails-current-scope-files 'controllers))
		      "\n")))
       (desc "finding model from controller"
             (expect "app/models/user.rb\n"
        	     (find-file "app/controllers/users_controller.rb")
        	     (helm-rails-current-scope-files 'models)))
       (desc "finding model from namespaced controller"
             (expect "app/models/user.rb\n"
        	     (find-file "app/controllers/admin/users_controller.rb")
        	     (helm-rails-current-scope-files 'models)))
       (desc "finding helper from controller"
             (expect "app/helpers/users_helper.rb\n"
        	     (find-file "app/controllers/users_controller.rb")
        	     (helm-rails-current-scope-files 'helpers)))
       (desc "finding view from controller"
             (expect '("app/views/users/index.html.erb"
        	       "app/views/users/show.html.erb")
        	     (find-file "app/controllers/users_controller.rb")
		     (split-string
        	      (replace-regexp-in-string "\n$" "" (helm-rails-current-scope-files 'views))
		      "\n")))
       (desc "finding javascripts from controller"
             (expect "app/assets/javascripts/users.js.coffee.erb\n"
        	     (find-file "app/controllers/users_controller.rb")
        	      (helm-rails-current-scope-files 'javascripts)))
       (desc "finding stylesheets from controller"
             (expect "app/assets/stylesheets/users.css.scss.erb\n"
        	     (find-file "app/controllers/users_controller.rb")
        	      (helm-rails-current-scope-files 'stylesheets)))
       (desc "finding spec from controller"
             (expect '("spec/controllers/users_controller_spec.rb"
        	       "spec/helpers/users_helper_spec.rb"
        	       "spec/models/user_spec.rb")
        	     (find-file "app/controllers/users_controller.rb")
		     (split-string
		      (replace-regexp-in-string "\n$" "" (helm-rails-current-scope-files 'specs))
		      "\n")))

       (desc "finding non-existant spec from controller"
             (expect ""
        	     (find-file "app/controllers/people_controller.rb")
        	     (helm-rails-current-scope-files 'specs))
             )
       (desc "finding spec from spec"
             (expect '("spec/controllers/users_controller_spec.rb"
        	       "spec/helpers/users_helper_spec.rb")
        	     (find-file "spec/models/user_spec.rb")
        	     (split-string
        	      (replace-regexp-in-string "\n$" "" (helm-rails-current-scope-files 'specs))
		      "\n"))
             )
       )
 )
