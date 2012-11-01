(expectations
 (desc "helm-rails-files"
       (desc "without regexp"
	     (expect '("app/models/criterion.rb"
		       "app/models/user.rb")
		     (helm-rails-files "app/models/")))
       (desc "with regexp"
       	     (expect '("app/models/criterion.rb"
       		       "app/models/user.rb")
       		     (helm-rails-files "app/models/" "\\.rb$"))
       	     (expect '()
       		     (helm-rails-files "app/models/" "\\.yaml$")))
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
		     (helm-rails-current-scope-files 'controllers)))
       (desc "finding model from controller"
	     (expect '("app/models/user.rb")
		     (find-file "app/controllers/users_controller.rb")
		     (helm-rails-current-scope-files 'models)))
       (desc "finding model from namespaced controller"
	     (expect '("app/models/user.rb")
		     (find-file "app/controllers/admin/users_controller.rb")
		     (helm-rails-current-scope-files 'models)))
       (desc "finding helper from controller"
	     (expect '("app/helpers/users_helper.rb")
		     (find-file "app/controllers/users_controller.rb")
		     (helm-rails-current-scope-files 'helpers)))
       (desc "finding view from controller"
	     (expect '("app/views/users/index.html.erb"
		       "app/views/users/show.html.erb")
		     (find-file "app/controllers/users_controller.rb")
		     (helm-rails-current-scope-files 'views)))
       (desc "finding spec from controller"
	     (expect '("spec/controllers/users_controller_spec.rb"
		       "spec/helpers/users_helper_spec.rb"
		       "spec/models/user_spec.rb")
		     (find-file "app/controllers/users_controller.rb")
		     (helm-rails-current-scope-files 'specs)))
       (desc "finding non-existant spec from controller"
	     (expect '()
		     (find-file "app/controllers/people_controller.rb")
		     (helm-rails-current-scope-files 'specs))
	     )
       )
 )
