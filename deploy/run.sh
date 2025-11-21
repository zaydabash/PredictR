#!/bin/bash

# Build the image
docker build -t predictr -f deploy/Dockerfile .

# Run the container
echo "Starting PredictR on http://localhost:3838"
docker run -p 3838:3838 predictr
