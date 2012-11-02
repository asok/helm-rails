# Synopsis

helm-rails is an extension for [helm](https://github.com/emacs-helm/helm) for working with Rails project. It is borrowing idea from [helm-git](https://github.com/maio/helm-git) that is asking git for a list of candidates.

You can find any files that are tracked by git. There are commands `helm-rails-*` (like `helm-rails-models`) for traversing resources. And there are also `helm-rails-*-c-source` that shows files that are revelant to the buffer's filename.

# Installation

Clone the repository somewhere on your hard drive. And add this to your emacs setup:

	(add-to-list 'load-path "/path/to/helm-rails/")
	(require 'helm-rails)

There are no keybindings set. For example me, I'm using something like this:

	(define-key global-map (kbd "s-t") 'helm-rails-controllers)
	(define-key global-map (kbd "s-y") 'helm-rails-models)
	(define-key global-map (kbd "s-u") 'helm-rails-views)
	(define-key global-map (kbd "s-o") 'helm-rails-specs)
	(define-key global-map (kbd "s-r") 'helm-rails-all)

# Limitations

* current scope c source may also grab something extra (to be fixed)
* no notion of `tests/` and `app/assets/` directory (to implemented)

# Contribution

Install [carton](https://github.com/rejeep/carton) if you haven't already, then:

	$ cd /path/to/helm-rails
	$ carton install
	
Run all tests with:

	$ carton exec make
	
