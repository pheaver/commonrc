#ELS=$(wildcard *.el)
SRCS=phil-file-modes phil-parent-dirs phil-utils
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

LOADDEF_SRCS=
LOADDEFS=$(foreach x, $(LOADDEF_SRCS), $x-loaddefs.el)

SUBDIRS=

all: $(ELCS) $(LOADDEFS) subdirs install-package-managers

.PHONY: subdirs
subdirs:
	@for d in $(SUBDIRS); do echo "[C]" $$d; $(MAKE) -C $$d; done


EMACS=emacs
BATCH=$(EMACS) -batch -q -no-site-file -eval \
  '(setq load-path (cons (expand-file-name ".") load-path))'

%.elc: %.el
	@echo "[C] $<"
	@$(BATCH) -f batch-byte-compile "$<"

# borrowed from org-mode's Makefile
%-loaddefs.el: %.el
	@$(BATCH) --eval "(require 'autoload)" \
                  --eval '(find-file (expand-file-name "$@"))' \
                  --eval '(erase-buffer)' \
                  --eval '(generate-file-autoloads (expand-file-name "$<"))' \
                  --eval '(insert "\n(provide (quote $*-loaddefs))\n")' \
                  --eval '(save-buffer)'

install-package-managers:
	@$(BATCH) --eval "(setq phil/auto-install-packages t)" --eval "(require 'phil-packages)"

clean:
	rm -fr $(ELCS)
	rm -fr $(LOADDEFS)
