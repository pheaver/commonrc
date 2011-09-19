#ELS=$(wildcard *.el)
SRCS=auto-mark browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace paredit \
	phil-file-modes phil-parent-dirs phil-utils phil-wspace
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

LOADDEF_SRCS=phil-utils
LOADDEFS=$(foreach x, $(LOADDEF_SRCS), $x-loaddefs.el)

SUBDIRS=anything-config.git auto-complete.git breadcrumb.git ghc-mod.git/elisp haskell-mode.git magit.git org-mode.git media-files.el.git

all: $(ELCS) $(LOADDEFS)

.PHONY: subdirs
subdirs:
	@for d in $(SUBDIRS); do echo "[C]" $$d; $(MAKE) -C $$d; done
	@echo "[C]" haskell-mode.git; $(MAKE) -C haskell-mode.git compile


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

clean:
	rm -fr $(ELCS)
	rm -fr $(fLOADDEFS)
