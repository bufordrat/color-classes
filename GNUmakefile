define SAVE
(save-lisp-and-die "color-classes" :toplevel (function cc-lib:main) \
:executable t :compression t :save-runtime-options t)
endef

color-classes: color-classes.lisp
	sbcl --noinform --disable-debugger --load color-classes.lisp --eval '$(SAVE)'

clean:
	rm color-classes
