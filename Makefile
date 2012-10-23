NAME  := weather-metno-el
DESCRIPTION := Weather data from met.no in Emacs

EMACS := emacs
BATCH := $(EMACS) -Q --batch --eval '(add-to-list '"'"'load-path ".")'

SOURCES := $(wildcard *.el)
ELC     := $(patsubst %.el, %.elc, $(SOURCES))
TESTS   := $(wildcard *-test.el)

ifneq ($(wildcard .git),)
	VERSION ?= $(shell git describe --tags --dirty --always)
else
	VERSION ?= $(shell cat version)
endif

PACKAGE := $(NAME)-$(VERSION)
TARBALL := $(PACKAGE).tar
PACKAGE_CONTENT := $(SOURCES) Makefile README.org README.html images
PKG_EL := $(NAME)-pkg.el

.PHONY: all test doc package clean dist-clean
all: $(ELC)
	$(info $(NAME) Version: $(VERSION))

test: $(TESTS)
	@$(BATCH) -l ert.el $(foreach file,$^,-l $(file)) -f ert-run-tests-batch-and-exit

clean:
	$(info Cleaning up)
	@$(RM) $(ELC) $(NAME)-pkg.el README.html
	@$(RM) -r $(PACKAGE)

dist-clean: clean
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
	@tar cf $(TARBALL) $(PACKAGE)

package: $(TARBALL)

$(ELC): %.elc: %.el
	$(info Building $(abspath $<))
	@$(BATCH) --eval '(batch-byte-compile)' $<
