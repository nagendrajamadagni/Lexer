docker run -it --rm \
  -v $(pwd):/app \
  -w /app \
  rust-valgrind-profiler cargo build --release

docker run -it --rm \
  -v $(pwd):/app \
  -w /app \
  rust-valgrind-profiler valgrind --tool=massif --stacks=yes ./target/release/Lexer "a(b|c)*"

docker run -it --rm \
    -v $(pwd):/app \
    -w /app \
    rust-valgrind-profiler ms_print massif.out > analysis.txt

docker run -it --rm \
    -v $(pwd):/app \
    -w /app \
    rust-valgrind-profiler valgrind --tool=callgrind --cache-sim=yes ./target/release/Lexer "a(b|c)*"

