bin = ./node_modules/.bin
bsb = $(bin)/bsb

build:
	@$(bsb) -make-world

watch:
	@$(bsb) -make-world -w

clean:
	@$(bsb) -clean-world
