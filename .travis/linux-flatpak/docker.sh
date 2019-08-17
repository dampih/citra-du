#!/bin/bash -ex

# Converts "citra-emu/citra-nightly" to "citra-nightly"
REPO_NAME=$(echo $BUILD_REPOSITORY_NAME | cut -d'/' -f 2)
CITRA_SRC_DIR="$BUILD_SOURCESDIRECTORY/"
BUILD_DIR="$CITRA_SRC_DIR/build"
REPO_DIR="$CITRA_SRC_DIR/repo"
STATE_DIR="$CITRA_SRC_DIR/.flatpak-builder"
GPG_KEY="${AZP_TMP_DIR}/flatpak_gpg.key"

# Configure GPG keys
gpg2 --import "$GPG_KEY"

mkdir -p "$REPO_DIR"

# setup ccache location
# mkdir -p "$STATE_DIR"
# ln -sv /root/.ccache "$STATE_DIR/ccache"

# Build the citra flatpak
sudo flatpak-builder -v --jobs=4 --ccache --force-clean --state-dir="$STATE_DIR" --gpg-sign="$FLATPAK_GPG_KEY_ID" --repo="$REPO_DIR" "$BUILD_DIR" "/tmp/org.citra.$REPO_NAME.json"
