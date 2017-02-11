# WARNING

I am not using this package myself. Instead I've created and I'm using right now [projectile-rails](https://github.com/asok/projectile-rails) which serves me the same purpose as helm-rails did.
That means I'm not maintaining helm-rails anymore.
If anyone would like to start maintaining it please express it by creating an issue and letting me know.

# Synopsis

helm-rails is an extension for [helm](https://github.com/emacs-helm/helm) for working with Rails project. It is borrowing idea from [helm-git](https://github.com/maio/helm-git) that is asking git for a list of candidates.

You can find any files that are tracked by git. There are commands `helm-rails-*` (like `helm-rails-models`) for traversing resources. And there are also `helm-rails-current-scope-*-c-source` that show files that are revelant to the buffer's filename.
It is possible to grep each of the resources with `helm-rails-grep-*` (like `helm-rails-grep-models`).

# Installation

### el-get

Just do `M-x el-get-install RET helm-rails RET`.

### melpa

`M-x package-install RET helm-rails RET`

### Manual

Clone the repository somewhere on your hard drive. And add this to your emacs setup:

	(add-to-list 'load-path "/path/to/helm-rails/")
	(require 'helm-rails-loaddefs)

There are no keybindings set. For example me, I'm using something like this:

	(define-key global-map (kbd "s-t") 'helm-rails-controllers)
	(define-key global-map (kbd "s-y") 'helm-rails-models)
	(define-key global-map (kbd "s-u") 'helm-rails-views)
	(define-key global-map (kbd "s-o") 'helm-rails-specs)
	(define-key global-map (kbd "s-r") 'helm-rails-all)

# Limitations

* no notion of `tests/` directory (to implement)
* depends on `git`, `grep` and `sed` binaries

# Tricks

### Defining a new resource

For example if you store your `Delayed::Job` jobs under `app/jobs/` directory you can do something like this to create a new command `helm-rails-jobs`:

    (helm-rails-def-resource 'jobs "app/jobs/" "^app/jobs/(.+)$")

The first argument will be the suffix of the name of the command. Second argument is the relative path to the directory where the files are stored. And the third argument is the regular expression to find the files (note: it is a bash not emacs regular expression).

# Contribution

Install [cask](https://github.com/cask/cask) if you haven't already, then:

	$ cd /path/to/helm-rails
	$ cask install
	
Run all tests with:

	$ make test
	
