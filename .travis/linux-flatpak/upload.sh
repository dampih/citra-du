#!/bin/bash -ex

CITRA_SRC_DIR="$BUILD_SOURCESDIRECTORY"
REPO_DIR="$CITRA_SRC_DIR/repo"
UPLOAD_DIR="$CITRA_SRC_DIR/upload/"
SSH_DIR="/upload"
SSH_KEY="${AZP_TMP_DIR}/flatpak_ssh.key"

# Configure SSH keys
mkdir -p "$HOME/.ssh"
chmod 0700 "$HOME/.ssh"
eval "$(ssh-agent -s)"
chmod 0600 "$SSH_KEY"
ssh-add "$SSH_KEY"
echo "[$FLATPAK_SSH_HOSTNAME]:$FLATPAK_SSH_PORT,[$(dig +short $FLATPAK_SSH_HOSTNAME)]:$FLATPAK_SSH_PORT $FLATPAK_SSH_HOST_FINGERPRINT" > "$HOME/.ssh/known_hosts"

# Mount our flatpak repository
mkdir -p "$UPLOAD_DIR"
sshfs "$FLATPAK_SSH_USER@$FLATPAK_SSH_HOSTNAME:$SSH_DIR" "$UPLOAD_DIR" -o idmap=user -C -p "$FLATPAK_SSH_PORT" -o IdentityFile="$SSH_KEY"

cp -r "${REPO_DIR}"/* "$UPLOAD_DIR"
flatpak build-update-repo "$UPLOAD_DIR" -v --generate-static-deltas --gpg-sign="$FLATPAK_GPG_KEY_ID"
