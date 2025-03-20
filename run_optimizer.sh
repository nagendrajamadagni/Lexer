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

mv massif.out.1 ./massif_output/massif_$(date +"%d_%m_%Y_%H_%M_%S").out
mv callgrind.out.1 ./callgrind_output/callgrind_$(date +"%d_%m_%Y_%H_%M_%S").out
