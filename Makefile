.PHONY: test lint byte-compile clean

EMACS ?= emacs
BATCH := $(EMACS) -Q --batch

EL_FILES := org-milestone-table.el
TEST_FILES := org-milestone-table-test.el

byte-compile: $(EL_FILES:.el=.elc)

%.elc: %.el
	$(BATCH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $<

test:
	$(BATCH) \
	  -l ert \
	  -l org-milestone-table.el \
	  -l org-milestone-table-test.el \
	  -f ert-run-tests-batch-and-exit

lint:
	$(BATCH) \
	  --eval '(require (quote package))' \
	  --eval '(package-initialize)' \
	  --eval '(require (quote package-lint) nil t)' \
	  --eval '(if (fboundp (quote package-lint-batch-and-exit)) \
	            (package-lint-batch-and-exit) \
	          (message "package-lint not available, skipping") \
	          (kill-emacs 0))' \
	  $(EL_FILES)

clean:
	rm -f *.elc
