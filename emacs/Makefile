#ELS=$(wildcard *.el)
SRCS=auto-mark browse-kill-ring linum multi-term shell-toggle \
	show-wspace tail visible-mark whitespace
ELS=$(foreach x,$(SRCS),$x.el)
ELCS=$(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	@echo "[C] $<"
	@emacs --batch --eval "(add-to-list 'load-path \"$(CURDIR)\")" \
	              --eval '(byte-compile-file "$<")'


clean:
	rm -fr $(ELCS)
