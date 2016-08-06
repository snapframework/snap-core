#!/usr/bin/env bash

set -e # Exit with nonzero exit code if anything fails

GHCVER=$1
SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"
OUT=out

rm -rf $OUT

function doCompile {
  ./runTestsAndCoverage.sh
}

# Pull requests and commits to other branches shouldn't try to deploy, just build to verify
if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "$SOURCE_BRANCH" ]; then
  echo "Skipping deploy; just doing a build."
  doCompile
  exit 0
fi

# Save some useful information
REPO="https://github.com/snapframework/snap-code-coverage.git"
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

echo REPO: $REPO
echo SSH_REPO: $SSH_REPO
echo SHA: $SHA

# Clone the existing gh-pages for this repo into $OUT/
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deply)
git clone $REPO $OUT
cd $OUT
git checkout $TARGET_BRANCH || git checkout --orphan $TARGET_BRANCH
cd ..

# Clean out existing contents
# rm -rf $OUT/**/* || exit 0

# Run our compile script
doCompile
git pull 

# Now let's go have some fun with the cloned repo
cd $OUT

git config user.name "Travis CI"
git config user.email "$COMMIT_AUTHOR_EMAIL"

# TODO this test seems to fail to detect changes
# If there are no changes to the compiled out (e.g. this is a README update) then just bail.
# if [ -z `git diff --exit-code` ]; then
#     echo "No changes to the output on this push; exiting."
#     exit 0
# fi

# Commit the "changes", i.e. the new version.
# The delta will show diffs between new and old versions.

# Last ditch pull just in case.  There should be no conflicts because each GHC
# version is writing to a different directory.
git pull -s ours origin $TARGET_BRANCH

rm -fr snap-core/hpc-ghc-$GHCVER
mv hpc snap-core/hpc-ghc-$GHCVER
git add -A
git commit -m "Deploy to GitHub Pages: ${SHA}" || true

# Get the deploy key by using Travis's stored variables to decrypt deploy_key_snap_code_coverage.enc
ENCRYPTED_KEY_VAR="encrypted_${ENCRYPTION_LABEL}_key"
ENCRYPTED_IV_VAR="encrypted_${ENCRYPTION_LABEL}_iv"
ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}
openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in ../deploy_key_snap_code_coverage.enc -out deploy_key_snap_code_coverage -d
chmod 600 deploy_key_snap_code_coverage
eval `ssh-agent -s`
ssh-add deploy_key_snap_code_coverage

# Now that we're all set up, we can push.
git push $SSH_REPO $TARGET_BRANCH
