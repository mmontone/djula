.PHONY: all clean compile doc test

SBCL = sbcl --noinform \
            --disable-ldb \
	    --lose-on-corruption \
	    --end-runtime-options \
	    --noprint \
	    --non-interactive

all:

clean:
	$(RM) -r $(HOME)/.cache/common-lisp
	$(RM) doc/index.html
	find . -name "*~" -delete
	find . -name "*.fasl" -delete

compile:
	$(SBCL) --eval "(require $(PACKAGE))"

doc: doc/index.html

doc/index.html:
	$(SBCL) --eval "(require $(PACKAGE))" \
	        --eval "(require :sb-introspect)" \
		--eval "(ql:quickload :cl-gendoc)" \
		--eval "(gendoc:gendoc (:output-filename \"doc/index.html\" :css \"simple.css\") (:apiref $(PACKAGE)))"

test:
	script/test
