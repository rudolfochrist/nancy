SBCL ?= sbcl --no-sysinit --no-userinit --non-interactive

all: info html

.PHONY: clean
clean:
	-rm -rf include/ *.texinfo *.info *.html

nancy.texinfo: ../src/*.lisp ../*.asd
	$(SBCL) \
	--eval '(require :asdf)' \
	--eval '(load #p"~/quicklisp/setup.lisp")' \
	--eval '(pushnew #p"../" asdf:*central-registry*)' \
	--eval '(ql:quickload :sb-texinfo)' \
	--eval '(ql:quickload :nancy)' \
	--eval '(sb-texinfo:document-package :nancy :output-file "nancy.texinfo")' \

%.info: %.texinfo
	makeinfo --no-split $<

%.html: %.texinfo
	makeinfo --html --no-split $<

info: nancy.info
html: nancy.html
