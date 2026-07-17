#!/bin/bash
set -e

echo "Building site..."
cabal run site rebuild

if [ ! -d "docs" ]; then
    echo "Error: docs directory not found. Make sure the build was successful."
    exit 1
fi

# jj auto-snapshots the working copy; commit it if there's anything new
if [ "$(jj log -r @ --no-graph -T 'if(empty, "empty", "nonempty")')" = "nonempty" ]; then
    echo "Committing changes..."
    jj commit -m "Deploy site - $(date '+%Y-%m-%d %H:%M:%S')"
else
    echo "No new changes to commit."
fi

echo "Pointing main at the latest commit..."
jj bookmark set main -r @-

echo "Pushing to GitHub..."
jj git push --bookmark main

echo "Deployment complete!"
