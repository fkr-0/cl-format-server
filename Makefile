LISP ?= sbcl

all: test

serve:
	ros run --load ~/.sbclrc -- --load run.lisp --serve 8081

client:
	ros run --load ~/.sbclrc --  --load run.lisp --client-stdin 8081
build:
	$(LISP)	--non-interactive \
		--load cl-format-server.asd \
		--eval '(ql:quickload :cl-format-server)' \
		--eval '(asdf:make :cl-format-server)'

test:
	ros run --load ~/.sbclrc --non-interactive \
		--load run-tests.lisp
