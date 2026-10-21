Tuatara, an editor in Common Lisp after the ideas of Emacs, for the terminal and the desktop. @STATUS@

## What changed

(Written before the release is published.)

## Getting it

For x86-64 Linux. Download what you need and `SHA256SUMS`, and check them first:

```sh
sha256sum -c --ignore-missing SHA256SUMS
```

**AppImage**: one file, nothing installed, for any system with glibc 2.39 or later (Ubuntu 24.04, Debian 13, Fedora 40 and newer):

```sh
chmod +x Tuatara-x86_64.AppImage
./Tuatara-x86_64.AppImage FILE          # in this terminal
./Tuatara-x86_64.AppImage --gui FILE    # in a window of its own
./Tuatara-x86_64.AppImage --install-desktop
```

**Packages**, installed with the system's own package manager, which brings the libraries Tuatara needs:

```sh
sudo dnf install ./tuatara-@VERSION@-*.x86_64.rpm         # Fedora 40 and newer
sudo apt install ./tuatara_@VERSION@*_amd64.deb           # Ubuntu 24.04 and newer
sudo pacman -U tuatara-@VERSION@*-x86_64.pkg.tar.zst      # Arch
```

Then `tuatara FILE` starts it in the terminal and `tuatara --gui FILE` in a window; it is in the desktop's applications too.

Some features use programs of the system, and say so when one is missing (bubblewrap, ImageMagick, poppler, LibreOffice, a LaTeX, gpg, git, enchant). The README lists them.

## Trying it

`M-x tuatara-tutor` teaches the modal keys by editing a text with them. The modal keys (DONKEY) are on by default: Escape for normal, `i` for insert; `(setf *modal-editing* nil)` in `~/.config/tuatara/init.lisp` gives plain Emacs-style keys. `M-x check-health` says what Tuatara finds on your system.

## Telling us

Open an issue with the **bug** label: what you did, what you expected and what happened, and the line `M-x tuatara-version` gives. For a security problem, use the private report in the Security tab instead.

## Sources and licences

Tuatara is GPL-3.0-or-later. `tuatara-sources.tar.zst` holds the sources this release is built from: Tuatara, the Lisp libraries compiled into it, Ghostty's terminal library, and tree-sitter with every grammar at its pinned commit. The licences of SBCL, of those libraries and of every grammar go with every file of the release (`licenses/` and `grammars/licenses/`), and are in `/usr/share/licenses/tuatara/` once a package is installed.

Built on GitHub from the tag `@VERSION@` (commit `@COMMIT@`).
