.PHONY:: hpaste.el
executable-find = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(PATH)))))
wget = $(if $(call executable-find,wget),wget -O $@ $1,curl -LRo $@ $1)
hpaste.el::
	$(call wget,http://www.haskell.org/haskellwiki/?title=Hpaste.el\&action=raw)
	sed -i '1,/<pre-lisp>/d;\|</pre-lisp>|,$$d' $@
