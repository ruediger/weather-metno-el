NAME  := weather-metno
DESCRIPTION := Weather data from met.no in Emacs

EMACS := emacs
BATCH := $(EMACS) -Q --batch --eval '(add-to-list '"'"'load-path ".")'

TESTS   := $(wildcard *-test.el)
EL 	:= $(wildcard *.el)
SOURCES := $(filter-out $(TESTS),$(EL))
ELC     := $(patsubst %.el, %.elc, $(SOURCES))

DATE := $(shell date +%Y%m%d)

ifneq ($(wildcard .git),)
	GITVERSION ?= $(shell git describe --tags --dirty --always)
	VERSION ?= $(shell git describe --tags --exact-match 2> /dev/null | sed 's/^v//')
endif

ifeq ($(VERSION),)
	ifneq ($(wildcard version),)
		VERSION := $(shell cat version)
	else
		VERSION := $(DATE)
	endif
endif

PACKAGE := $(NAME)-$(VERSION)
TARBALL := $(PACKAGE).tar
PACKAGE_CONTENT := $(SOURCES) Makefile README.org README.html images
PKG_EL := $(NAME)-pkg.el

.PHONY: all test doc package clean distclean
all: $(ELC)
	$(info $(NAME) Version: $(VERSION))

test: $(TESTS)
	@$(BATCH) -l ert.el $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit

clean:
	$(info Cleaning up)
	@$(RM) $(ELC) $(NAME)-pkg.el README.html
	@$(RM) -r $(PACKAGE)

distclean: clean
	@$(RM) -r $(TARBALL)

README.html: README.org
	$(info Creating documentation: $@)
	@$(BATCH) -l org.el --visit=$< -f org-export-as-html-batch

doc: README.html

$(PKG_EL):
	@echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DESCRIPTION)\")" > $@

$(TARBALL): test $(PKG_EL) doc
	$(info Creating package tarball $(TARBALL))
	@mkdir -p $(PACKAGE)
	@cp -r $(PACKAGE_CONTENT) $(PKG_EL) $(PACKAGE)/
	@echo "$(VERSION)" > $(PACKAGE)/version
ifneq ($(GITVERSION),)
	@echo "$(GITVERSION)" > $(PACKAGE)/git-version
endif
	@tar cf $(TARBALL) $(PACKAGE)

package: $(TARBALL)

$(ELC): %.elc: %.el
	$(info Building $(abspath $<))
	@$(BATCH) --eval '(batch-byte-compile)' $<
