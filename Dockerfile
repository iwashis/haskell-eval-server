FROM haskell:9.2.8-slim as builder

# Set working directory to match your project structure
WORKDIR /opt/haskell-eval-server

# Copy only the files needed for stack dependencies
COPY stack.yaml stack.yaml.lock* package.yaml ./

# Create project structure
RUN mkdir -p app src

# Install system dependencies needed for GHC and Stack
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

# Force using system GHC to avoid downloading a specific version
RUN stack config set system-ghc --global true

# Build dependencies only (this layer is cached)
RUN stack build --only-dependencies --system-ghc

# Copy source code with the correct project structure
COPY app app/
COPY src src/
COPY LICENSE README.md ./

# Build the actual application
RUN stack build --system-ghc --copy-bins --ghc-options="-O2"

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

# Create a non-root user for running the application
RUN useradd -m haskell-eval

WORKDIR /opt/haskell-eval-server

# Copy the compiled executable
COPY --from=builder /root/.local/bin/haskell-eval-server /opt/haskell-eval-server/

# Set ownership and permissions
RUN chown -R haskell-eval:haskell-eval /opt/haskell-eval-server && \
    chmod +x /opt/haskell-eval-server/haskell-eval-server

# Needed for temporary file operations
RUN mkdir -p /tmp/haskell-eval && \
    chown -R haskell-eval:haskell-eval /tmp/haskell-eval

# Switch to non-root user
USER haskell-eval

# Expose the port
EXPOSE 3000

# Entrypoint
ENTRYPOINT ["/opt/haskell-eval-server/haskell-eval-server"]
