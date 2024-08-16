# Build stage
FROM erlang:24-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git

# Set working directory
WORKDIR /app

# Copy only the files needed for dependency installation
COPY rebar.config rebar.lock ./

# Install dependencies
RUN rebar3 get-deps

# Copy the rest of the application code
COPY . .

# Compile the release
RUN rebar3 as prod release

# Final stage
FROM alpine:3.14

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs libstdc++

# Set working directory
WORKDIR /app

# Copy the release from the builder stage
COPY --from=builder /app/_build/prod/rel/webserver ./

# Expose the application port
EXPOSE 8080

# Set the entrypoint to start the release
ENTRYPOINT ["/app/bin/webserver"]
CMD ["foreground"]