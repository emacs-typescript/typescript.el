EMACS ?= emacs
EASK ?= eask
ELS = \
  typescript-mode.el \
  typescript-mode-test-utilities.el \
  typescript-mode-general-tests.el \
  typescript-mode-jsdoc-tests.el \
  typescript-mode-tests.el
ELCS = $(ELS:.el=.elc)

clean:
	rm -f $(ELCS)

build: clean
	$(EASK) compile

test:
	+ $(EMACS) -Q -batch -L . -l typescript-mode-tests.el -f ert-run-tests-batch-and-exit

# end
