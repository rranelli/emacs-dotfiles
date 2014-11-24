EMACS := emacs

.PHONY: test

test: .downloads
	${EMACS} -Q --batch -L . -L ./lisp -L ./vendor \
		--eval "(progn (require 'init-bootstrap) (rr-unsafe-load-init-files))"
