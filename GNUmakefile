NAME := color-classes
EXE = $(NAME)
SBCL := sbcl --noinform --no-userinit --disable-debugger

.PHONY: exe
exe: $(EXE)

$(EXE): $(FILES)
	$(SBCL) --load setup.lisp --eval '(build "$(NAME)")'

# define SAVE
# (save-lisp-and-die "color-classes" :toplevel (function color-classes:main) \
# :executable t :compression t :save-runtime-options t)
# endef

# color-classes: color-classes.lisp
# 	sbcl --noinform --disable-debugger --load color-classes.lisp --eval '$(SAVE)'

# clean:
# 	rm color-classes
