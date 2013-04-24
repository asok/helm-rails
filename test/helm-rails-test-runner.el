(let ((current-directory (file-name-directory load-file-name)))
  (setq helm-rails-test-path (expand-file-name "." current-directory))
  (setq helm-rails-root-path (expand-file-name ".." current-directory))
  (add-to-list 'load-path helm-rails-root-path)
  (add-to-list 'load-path helm-rails-test-path)
  (load (expand-file-name "test-helper.el" helm-rails-test-path) nil t))

(require 'helm-rails)
(require 'ert-expectations)

(load (expand-file-name "helm-rails-test.el" helm-rails-test-path) nil t)

(with-temp-git-repo repo
		    (loop for directory in '("app/"
					     "app/assets/"
					     "app/assets/javascripts/"
					     "app/assets/stylesheets/"
					     "app/models/"
					     "app/controllers/"
					     "app/controllers/admin/"
					     "app/helpers/"
					     "app/views/"
					     "app/views/users/"
					     "lib/"
					     "config/"
					     "spec/"
					     "spec/models/"
					     "spec/controllers/"
					     "spec/helpers/")
			  do (make-directory directory))

		    (loop for file-path in '("app/models/criterion.rb"
					     "app/models/user.rb"
					     "app/assets/javascripts/users.js.coffee.erb"
					     "app/assets/stylesheets/users.css.scss.erb"
					     "app/controllers/users_controller.rb"
					     "app/controllers/people_controller.rb"
					     "app/controllers/admin/users_controller.rb"
					     "app/helpers/users_helper.rb"
					     "app/views/users/index.html.erb"
					     "app/views/users/show.html.erb"
					     "spec/spec_helper.rb"
					     "spec/controllers/users_controller_spec.rb"
					     "spec/helpers/users_helper_spec.rb"
					     "spec/models/user_spec.rb"
					     "Gemfile"
					     "config/environment.rb"
					     )
			  do (create-file-in-repo repo file-path))
                    (shell-command "touch app/models/profile.rb")
		    (ert-run-tests-batch t))
