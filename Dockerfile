FROM debian:stable-slim

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    clang \
    valgrind \
    && rm -rf /var/lib/apt/lists/*

# Install Rust toolchain
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
    sh -s -- -y --default-toolchain stable --profile minimal

# Install cargo-valgrind for enhanced integration
RUN cargo install cargo-valgrind

# Configure workspace and permissions
WORKDIR /app
RUN chmod 777 /app
