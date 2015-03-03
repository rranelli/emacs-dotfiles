EMACS := emacs

.PHONY: test

build:
	./setup_dotfiles
	sudo ./setup_shortcut

test:
	${EMACS} -Q --batch -L . -L ./lisp -L ./vendor \
		--eval "(progn (require 'init-bootstrap) (rr/unsafe-load-init-files))"
