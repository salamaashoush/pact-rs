# Multi-stage Docker build for minimal Pact executable
# Build stage with all dependencies
FROM debian:bookworm-slim AS builder

# Install system dependencies needed for building Haskell
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

# Install GHC and Cabal using ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y
ENV PATH="/root/.ghcup/bin:$PATH"

# Install specific GHC and Cabal versions
RUN ghcup install ghc 9.6.7 && \
    ghcup install cabal latest && \
    ghcup set ghc 9.6.7

# Set working directory
WORKDIR /app

# Set locale to UTF-8
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# Copy cabal files first for better caching
COPY cabal.project .
COPY pact-tng.cabal .

# Update cabal
RUN cabal update

# Configure without optimization first to speed up builds
RUN cabal configure \
    --disable-optimization \
    --constraint="crypton >= 1.0" \
    --constraint="memory >= 0.18" \
    --constraint="hashes >= 0.3"

# Copy source code
COPY . .

# Build the executable
RUN rm -f cabal.project.local* && \
    cabal build exe:pact && \
    cp $(cabal list-bin exe:pact) /opt/pact

# Runtime stage - use latest Debian slim for minimal size
FROM debian:bookworm-slim AS runtime

# Install only runtime dependencies
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
