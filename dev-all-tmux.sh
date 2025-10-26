#!/bin/bash

# Script to run all three microservices in dev mode using tmux
# Creates a tmux session with three panes, one for each service

SESSION_NAME="quarkus-dev"

echo "Starting all microservices in dev mode with tmux..."
echo "======================================"

# Check if tmux is available
if ! command -v tmux &> /dev/null; then
    echo "Error: tmux not found. Please install it with:"
    echo "  sudo apt-get install tmux"
    echo ""
    echo "Or run services manually in separate terminals:"
    echo "  Terminal 1: ./gradlew :catalog:quarkusDev"
    echo "  Terminal 2: ./gradlew :users:quarkusDev"
    echo "  Terminal 3: ./gradlew :reviews:quarkusDev"
    exit 1
fi

# Kill existing session if it exists
tmux kill-session -t $SESSION_NAME 2>/dev/null

# Create new tmux session with catalog service
echo "Creating tmux session '$SESSION_NAME'..."
tmux new-session -d -s $SESSION_NAME -n "services"

# Split window into 3 panes
tmux split-window -h -t $SESSION_NAME
tmux split-window -v -t $SESSION_NAME

# Layout the panes nicely
tmux select-layout -t $SESSION_NAME tiled

# Start catalog service in first pane
tmux send-keys -t $SESSION_NAME:0.0 "cd $(pwd) && echo 'Starting Catalog Service (8081)...' && ./gradlew :catalog:quarkusDev" C-m

# Start users service in second pane
tmux send-keys -t $SESSION_NAME:0.1 "cd $(pwd) && echo 'Starting Users Service (8082)...' && ./gradlew :users:quarkusDev" C-m

# Start reviews service in third pane
tmux send-keys -t $SESSION_NAME:0.2 "cd $(pwd) && echo 'Starting Reviews Service (8083)...' && ./gradlew :reviews:quarkusDev" C-m

echo ""
echo "======================================"
echo "All services started in tmux session!"
echo ""
echo "Services running on:"
echo "  - Catalog:  http://localhost:8081"
echo "  - Users:    http://localhost:8082"
echo "  - Reviews:  http://localhost:8083"
echo ""
echo "To attach to the tmux session:"
echo "  tmux attach -t $SESSION_NAME"
echo ""
echo "Tmux commands (inside session):"
echo "  - Switch panes: Ctrl+b + arrow keys"
echo "  - Detach: Ctrl+b + d"
echo "  - Zoom pane: Ctrl+b + z"
echo "  - Scroll: Ctrl+b + ["
echo ""
echo "To stop all services:"
echo "  tmux kill-session -t $SESSION_NAME"
echo "======================================"

# Attach to the session
tmux attach -t $SESSION_NAME
