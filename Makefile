# Adapted from: http://www.greghendershott.com/2017/04/racket-makefiles.html
SHELL=/bin/bash

PACKAGE-NAME=raqit

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

help:
	@echo "Run common development actions."
	@echo
	@echo "    Usage: make <target>"
	@echo "    where <target> is one of:"
	@echo
	@echo "help - show this menu"
	@echo "install - install package along with dependencies"
	@echo "remove - remove package"
	@echo "build - Compile libraries"
	@echo "clean - remove all build artifacts"
	@echo "check-deps - check dependencies"
	@echo "test - run tests"
	@echo "test-with-errortrace - run tests with error tracing"
	@echo "errortrace - alias for test-with-errortrace"


# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto --link $(PWD)/$(PACKAGE-NAME)

remove:
	raco pkg remove $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Build libraries from source.
build:
	raco setup --no-docs --pkgs $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

.PHONY:	help install remove build clean check-deps
