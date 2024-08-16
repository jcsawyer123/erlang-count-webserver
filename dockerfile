# Use the official Erlang image as a parent image
FROM erlang:24

# Set the working directory in the container
WORKDIR /app

# Copy the rebar.config file
COPY rebar.config .

# Copy the source code
COPY src/ src/

# Copy the release configuration
COPY webserver.config .

# Build the release
RUN rebar3 as prod release

# Expose the port the app runs on
EXPOSE 8080

# Command to run the application
CMD ["_build/prod/rel/webserver/bin/webserver", "foreground"]