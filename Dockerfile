FROM haskell:9.2.8-slim as builder

# Install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    g++ \
    gcc \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    make \
    xz-utils \
    zlib1g-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /opt/haskell-eval-server

# Create project structure
RUN mkdir -p app src

# Set stack to use system GHC
RUN stack config set system-ghc --global true

# Copy configuration files first (for better caching)
COPY stack.yaml package.yaml ./
COPY README.md ./

# Build dependencies
RUN stack build --only-dependencies --system-ghc

# Copy source code
COPY app app/
COPY src src/

# Build the application
RUN stack build --system-ghc --ghc-options="-O2"

# Copy the binary to a known location
RUN mkdir -p /opt/bin && \
    find $(stack path --local-install-root)/bin -name "haskell-eval-server" -type f -exec cp {} /opt/bin/ \;

# Create a smaller runtime image
FROM debian:bullseye-slim as runtime

# Install minimal runtime dependencies including GHC
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    libnuma1 \
    ghc \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user
RUN useradd -m haskell-eval

# Create workdir and tmp directory
WORKDIR /opt/haskell-eval-server
RUN mkdir -p /tmp/haskell-eval && \
    chown -R haskell-eval:haskell-eval /tmp/haskell-eval

# Copy the compiled executable with the correct name
COPY --from=builder /opt/haskell-eval-server/.stack-work/install/aarch64-linux/*/9.2.8/bin/haskell-eval-server-exe /opt/haskell-eval-server/haskell-eval-server


# Set ownership and permissions
RUN chown -R haskell-eval:haskell-eval /opt/haskell-eval-server && \
    chmod +x /opt/haskell-eval-server/haskell-eval-server

# allow prelude to be trusted
RUN ghc-pkg trust base

# Switch to non-root user
USER haskell-eval

# Expose the port
EXPOSE 3000

# Entrypoint
ENTRYPOINT ["/opt/haskell-eval-server/haskell-eval-server"]
