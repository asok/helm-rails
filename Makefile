all: test

test:
	carton exec /usr/bin/env emacs -Q --script test/helm-rails-test-runner.el

.PHONY: test
