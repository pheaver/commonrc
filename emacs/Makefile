#ELS=$(wildcard *.el)
SRCS=auto-mark browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace \
	phil-parent-dirs phil-utils phil-wspace
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

LOADDEF_SRCS=phil-utils
LOADDEFS=$(foreach x, $(LOADDEF_SRCS), $x-loaddefs.el)
all: $(ELCS) $(LOADDEFS)

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
