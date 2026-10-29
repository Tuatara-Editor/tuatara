# tools/containers.sh --- The pinned systems Tuatara is packaged in: sourced by the other scripts.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# The podman images tools/appimage.sh and tools/distro-packages.sh work in,
# each pinned by its digest (tools/check-updates.sh says when a newer one is
# out), and
#
# pinned_container NAME IMAGE SETUP  starts the container NAME, kept from one
# run to the next, first making it from IMAGE and running the shell command
# SETUP in it when there is none, or when the one there was made from another
# image; a container whose SETUP failed is not kept.
ubuntu_image=docker.io/library/ubuntu:24.04@sha256:68ee22004a9109b44973313b02ff7b1de250f1de0f5ecb628d73f579aaff33ac
fedora_image=registry.fedoraproject.org/fedora:44@sha256:12151022b0ec7cb4d70ae14e4a4d319377323265d7198b69dd53ec92713198d5
arch_image=docker.io/library/archlinux:latest@sha256:e43f1fd80e073992c5f173efbbbf65144170bf60aeb7369940087d63eacea8f4

pinned_container() {
  local name="$1" image="$2" setup="$3"
  if podman container exists "$name" &&
     [ "$(podman container inspect -f '{{index .Config.Labels "tuatara.image"}}' "$name")" != "$image" ]; then
    podman rm -f "$name" >/dev/null
  fi
  if ! podman container exists "$name"; then
    podman run -d --name "$name" --label "tuatara.image=$image" "$image" sleep infinity >/dev/null
    if ! podman exec "$name" sh -c "$setup"; then
      podman rm -f "$name" >/dev/null
      echo "$name could not be set up from $image" >&2
      return 1
    fi
  fi
  podman start "$name" >/dev/null
}
