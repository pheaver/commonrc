#ELS=$(wildcard *.el)
SRCS=auto-mark browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace \
	phil-parent-dirs phil-utils phil-wspace
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

LOADDEF_SRCS=phil-utils
LOADDEFS=$(foreach x, $(LOADDEF_SRCS), $x-loaddefs.el)

SUBDIRS=auto-complete.git breadcrumb.git ghc-mod.git/elisp haskell-mode.git magit.git org-mode.git

all: $(ELCS) $(LOADDEFS)

.PHONY: subdirs
subdirs:
	@for d in $(SUBDIRS); do echo "[C]" $$d; $(MAKE) -C $$d; done

EMACS=emacs
BATCH=$(EMACS) -batch -q -no-site-file -eval \
  '(setq load-path (cons (expand-file-name ".") load-path))'

%.elc: %.el
	@echo "[C] $<"
	@$(BATCH) --eval '(byte-compile-file (expand-file-name "$<"))'

%-loaddefs.el: %.el
	@$(BATCH) --eval '(update-file-autoloads (expand-file-name "$<") t)'
	@touch $@

clean:
	rm -fr $(ELCS)
	rm -fr $(fLOADDEFS)
