#!/bin/bash
set -e

echo "Building site..."
cabal run site build

echo "Checking if _site directory exists..."
if [ ! -d "_site" ]; then
    echo "Error: _site directory not found. Make sure the build was successful."
    exit 1
fi

echo "Switching to gh-pages branch..."
git checkout gh-pages

echo "Removing old files..."
# Remove all files except .git directory
find . -maxdepth 1 -not -name '.git' -not -name '.' -exec rm -rf {} \;

echo "Copying new site files..."
cp -r _site/* .

echo "Adding files to git..."
git add .

echo "Committing changes..."
if git diff --staged --quiet; then
    echo "No changes to commit."
else
    git commit -m "Deploy site - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "Pushing to GitHub..."
    git push origin gh-pages
    echo "Site deployed successfully!"
fi

echo "Switching back to main branch..."
git checkout main

echo "Deployment complete!"