CSS_FILES := $(patsubst %.less, %.css, $(wildcard ./css/*.less))
LESSC = npm x lessc

all: css publish

css: $(CSS_FILES)


./css/%.css: css/%.less
	@echo "$< -> $@"
	$(LESSC) $< $@

publish:
	@echo "Publishing..."
	emacs --quick --batch --load lisp/publish.el --funcall org-publish-all t t

serve: all
	@miniserve public

clean:
	@echo "Cleaning up..."
	@rm -rvf public/
	@rm -rvf .cache/
