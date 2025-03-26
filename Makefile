build:
	cargo build

run:
	cargo run "a(b|c)*"
	cargo run "(a|b)(c|d)"

.PHONY clean:
	rm *.jpg
	rm *.dot
