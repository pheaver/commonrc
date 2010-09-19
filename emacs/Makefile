#ELS=$(wildcard *.el)
SRCS=auto-mark breadcrumb browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace \
	phil-parent-dirs
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@echo "[C] $<"
	@emacs --batch --eval "(add-to-list 'load-path \"$(CURDIR)\")" \
	              --eval '(byte-compile-file "$<")'


clean:
	rm -fr $(ELCS)
