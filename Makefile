EMACS=$(shell which emacs) -Q -batch -L .

cask:
	cask build

test: cask
	+ $(EMACS) -l typescript-mode-tests.el -f ert-run-tests-batch-and-exit

# end
