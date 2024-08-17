# Build stage
FROM erlang:24-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git

# Set working directory
WORKDIR /app

# Copy the entire application
COPY . .

# Install dependencies and compile the release
RUN rebar3 get-deps
RUN rebar3 as prod release

# Final stage
FROM erlang:24-alpine

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs libstdc++

# Set working directory
WORKDIR /app

# Copy the release from the builder stage
COPY --from=builder /app/_build/prod/rel/webserver ./

# Expose the application port
EXPOSE 8080

# Set the entrypoint to start the release
CMD ["bin/webserver", "foreground"]