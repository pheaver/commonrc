#ELS=$(wildcard *.el)
SRCS=auto-mark breadcrumb browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace \
	phil-parent-dirs phil-utils
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

LOADDEF_SRCS=phil-utils
LOADDEFS=$(foreach x, $(LOADDEF_SRCS), $x-loaddefs.el)
all: $(ELCS) $(LOADDEFS)

EMACS:=emacs
DO_EMACS:=$(EMACS) --batch --eval "(add-to-list 'load-path \"$(CURDIR)\")"

%.elc: %.el
	@echo "[C] $<"
	@$(DO_EMACS) --eval '(byte-compile-file "$<")'

%-loaddefs.el: %.el
	@$(DO_EMACS) --eval '(update-file-autoloads "$<" t)'
	@touch $@

clean:
	rm -fr $(ELCS)
	rm -fr $(fLOADDEFS)
