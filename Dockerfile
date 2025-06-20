# Multi-stage Docker build for minimal Pact executable
# Build stage with all dependencies - use latest Debian slim with OpenSSL 3 support
FROM debian:bookworm-slim AS builder

# Install system dependencies needed for building Haskell with OpenSSL 3
RUN apt-get update && apt-get install -y \
    curl \
    git \
    pkg-config \
    libgmp-dev \
    libmpfr-dev \
    libsqlite3-dev \
    libtinfo-dev \
    zlib1g-dev \
    libssl-dev \
    libffi-dev \
    build-essential \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install GHC and Cabal using ghcup (ensures latest versions)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y
ENV PATH="/root/.ghcup/bin:$PATH"

# Install specific GHC and Cabal versions with OpenSSL 3 support
RUN ghcup install ghc 9.6.7 && \
    ghcup install cabal latest && \
    ghcup set ghc 9.6.7

# Set working directory
WORKDIR /app

# Copy cabal files first for better caching
COPY cabal.project .
COPY pact-tng.cabal .

# Set environment variables for OpenSSL 3
ENV PKG_CONFIG_PATH="/usr/lib/x86_64-linux-gnu/pkgconfig"
ENV OPENSSL_INCLUDE_PATH="/usr/include/openssl"
ENV OPENSSL_LIB_PATH="/usr/lib/x86_64-linux-gnu"

# Update cabal with OpenSSL 3 flags
RUN cabal update

# Configure with validated OpenSSL 3 support and optimization
# Use the exact constraints that worked on the host
RUN cabal configure \
    --enable-optimization=2 \
    --constraint="crypton >= 1.0" \
    --constraint="memory >= 0.18" \
    --constraint="hashes >= 0.3" \
    --extra-include-dirs=/usr/include \
    --extra-lib-dirs=/usr/lib/x86_64-linux-gnu

# Copy source code
COPY . .

# Build the pact executable with validated configuration
RUN cabal build exe:pact && cp $(cabal list-bin exe:pact) /opt/pact

# Runtime stage - use latest Debian slim for minimal size
FROM debian:bookworm-slim AS runtime

# Install only runtime dependencies - Debian bookworm has OpenSSL 3.0 by default
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libmpfr6 \
    libsqlite3-0 \
    libtinfo6 \
    libssl3 \
    libffi8 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* && apt-get clean

# Create non-root user
RUN useradd -r -s /bin/false pact

# Copy the built executable
COPY --from=builder /opt/pact /usr/local/bin/pact

# Set permissions
RUN chmod +x /usr/local/bin/pact

# Switch to non-root user
USER pact

# Set entrypoint
ENTRYPOINT ["/usr/local/bin/pact"]
CMD ["--help"]
