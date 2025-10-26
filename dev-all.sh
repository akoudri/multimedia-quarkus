#!/bin/bash

# Script to run all three microservices in dev mode simultaneously
# Each service runs in its own terminal window using gnome-terminal

echo "Starting all microservices in dev mode..."
echo "======================================"

# Check if gnome-terminal is available
if ! command -v gnome-terminal &> /dev/null; then
    echo "Error: gnome-terminal not found. Please install it or run services manually in separate terminals:"
    echo "  Terminal 1: ./gradlew :catalog:quarkusDev"
    echo "  Terminal 2: ./gradlew :users:quarkusDev"
    echo "  Terminal 3: ./gradlew :reviews:quarkusDev"
    exit 1
fi

# Start catalog service
echo "Starting Catalog service on port 8081..."
gnome-terminal --title="Catalog Service (8081)" -- bash -c "./gradlew :catalog:quarkusDev; exec bash"

# Wait a bit before starting next service
sleep 2

# Start users service
echo "Starting Users service on port 8082..."
gnome-terminal --title="Users Service (8082)" -- bash -c "./gradlew :users:quarkusDev; exec bash"

# Wait a bit before starting next service
sleep 2

# Start reviews service
echo "Starting Reviews service on port 8083..."
gnome-terminal --title="Reviews Service (8083)" -- bash -c "./gradlew :reviews:quarkusDev; exec bash"

echo ""
echo "======================================"
echo "All services started in separate terminals!"
echo ""
echo "Services running on:"
echo "  - Catalog:  http://localhost:8081"
echo "  - Users:    http://localhost:8082"
echo "  - Reviews:  http://localhost:8083"
echo ""
echo "To stop all services, close the terminal windows or press Ctrl+C in each terminal."
echo "======================================"
