all: test

test:
	cd test ; ./helm-rails-test

.PHONY: test
