;;;; lisp/modes/more-languages-build.lisp --- The shells, build files and version control Helix knows, in major modes named as Emacs's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each language is parsed with the grammar and coloured, indented and
;;;; selected by the rules Helix uses for it (syntax/), in a major mode named as
;;;; the one Emacs or its best-known package edits it with. A file type another
;;;; mode already claims stays that mode's.
;;;;
;;;; Written by tools/languages/generate.py: change that, not this file.

(in-package #:tuatara.defaults)

(define-major-mode go-mod-mode prog-mode
  "Go module files (go.mod)."
  :lighter "Go Mod"
  :comment "//"
  :file-names ("go.mod"))

(define-language gomod
  "Go module files (go.mod), parsed with tree-sitter-gomod."
  :grammar "gomod" :modes (go-mod-mode) :indent 4 :queries "gomod" :injection-names "gomod")

(define-major-mode makefile-mode prog-mode
  "Makefiles."
  :lighter "Makefile"
  :comment "#"
  :extensions ("make" "mk" "mak")
  :file-names ("Makefile" "makefile" "GNUmakefile" "OCamlMakefile")
  :interpreters ("make" "gmake"))

(define-language make
  "Makefiles, parsed with tree-sitter-make."
  :grammar "make" :modes (makefile-mode) :indent 4 :queries "make" :injection-names "make|makefile|Makefile|mk")

(define-major-mode cmake-mode prog-mode
  "CMake build files."
  :lighter "CMake"
  :comment "#"
  :extensions ("cmake")
  :file-names ("CMakeLists.txt"))

(define-language cmake
  "CMake build files, parsed with tree-sitter-cmake."
  :grammar "cmake" :modes (cmake-mode) :indent 2 :queries "cmake" :injection-names "cmake" :definitions :textobjects)

(define-major-mode nix-mode prog-mode
  "Nix expressions."
  :lighter "Nix"
  :comment "#"
  :extensions ("nix"))

(define-language nix
  "Nix expressions, parsed with tree-sitter-nix."
  :grammar "nix" :modes (nix-mode) :indent 2 :queries "nix" :injection-names "nix" :definitions :textobjects)

(define-major-mode diff-mode prog-mode
  "Differences between files: patches."
  :lighter "Diff"
  :comment "#"
  :extensions ("diff" "patch" "rej"))

(define-language diff
  "Differences between files: patches, parsed with tree-sitter-diff."
  :grammar "diff" :modes (diff-mode) :indent 2 :queries "diff" :injection-names "diff")

(define-major-mode git-rebase-mode prog-mode
  "Git's list of commits to rebase (git-rebase-todo)."
  :lighter "Git Rebase"
  :comment "#"
  :file-names ("git-rebase-todo"))

(define-language git-rebase
  "Git's list of commits to rebase (git-rebase-todo), parsed with tree-sitter-git-rebase."
  :grammar "git-rebase" :modes (git-rebase-mode) :indent 2 :queries "git-rebase" :injection-names "git-rebase")

(define-major-mode fish-mode prog-mode
  "Fish shell scripts."
  :lighter "Fish"
  :comment "#"
  :extensions ("fish")
  :interpreters ("fish"))

(define-language fish
  "Fish shell scripts, parsed with tree-sitter-fish."
  :grammar "fish" :modes (fish-mode) :indent 4 :queries "fish" :injection-names "fish" :definitions :textobjects)

(define-major-mode awk-mode prog-mode
  "AWK programs."
  :lighter "AWK"
  :comment "#"
  :extensions ("awk" "gawk" "nawk" "mawk"))

(define-language awk
  "AWK programs, parsed with tree-sitter-awk."
  :grammar "awk" :modes (awk-mode) :indent 2 :queries "awk" :injection-names "awk" :definitions :textobjects)

(define-major-mode just-mode prog-mode
  "Justfiles: the recipes of the just command runner."
  :lighter "Just"
  :comment "#"
  :extensions ("just")
  :file-names ("justfile" "Justfile" ".justfile" ".Justfile"))

(define-language just
  "Justfiles: the recipes of the just command runner, parsed with tree-sitter-just."
  :grammar "just" :modes (just-mode) :indent 4 :queries "just" :injection-names "just" :definitions :textobjects)

(define-major-mode powershell-mode prog-mode
  "PowerShell scripts."
  :lighter "PowerShell"
  :comment "#"
  :extensions ("ps1" "psm1" "psd1" "pscc" "psrc")
  :interpreters ("pwsh" "powershell"))

(define-language powershell
  "PowerShell scripts, parsed with tree-sitter-powershell."
  :grammar "powershell" :modes (powershell-mode) :indent 4 :queries "powershell" :injection-names "pwsh|powershell")

(define-major-mode meson-mode prog-mode
  "Meson build files."
  :lighter "Meson"
  :comment "#"
  :file-names ("meson.build" "meson.options" "meson_options.txt"))

(define-language meson
  "Meson build files, parsed with tree-sitter-meson."
  :grammar "meson" :modes (meson-mode) :indent 2 :queries "meson" :injection-names "meson")

(define-major-mode nushell-mode prog-mode
  "Nushell scripts."
  :lighter "Nu"
  :comment "#"
  :extensions ("nu" "nuon")
  :interpreters ("nu"))

(define-language nu
  "Nushell scripts, parsed with tree-sitter-nu."
  :grammar "nu" :modes (nushell-mode) :indent 2 :queries "nu" :injection-names "nu" :definitions :textobjects)

(define-major-mode elvish-mode prog-mode
  "Elvish shell scripts."
  :lighter "Elvish"
  :comment "#"
  :extensions ("elv")
  :interpreters ("elvish"))

(define-language elvish
  "Elvish shell scripts, parsed with tree-sitter-elvish."
  :grammar "elvish" :modes (elvish-mode) :indent 2 :queries "elvish")

(define-major-mode dune-mode prog-mode
  "Dune build files, for OCaml."
  :lighter "Dune"
  :comment (";" ";;")
  :file-names ("dune-project" "dune"))

(define-language dune
  "Dune build files, for OCaml, parsed with tree-sitter-scheme."
  :grammar "scheme" :modes (dune-mode) :indent 1 :queries "dune")

(define-major-mode dts-mode prog-mode
  "Devicetree sources."
  :lighter "DTS"
  :comment "//"
  :extensions ("dts" "dtsi"))

(define-language devicetree
  "Devicetree sources, parsed with tree-sitter-devicetree."
  :grammar "devicetree" :modes (dts-mode) :indent 4 :queries "devicetree" :injection-names "dtsi?|devicetree|fdt")

(define-major-mode kconfig-mode prog-mode
  "Kconfig files: the configuration menus of the Linux kernel."
  :lighter "Kconfig"
  :extensions ("Kconfig")
  :file-names ("Kconfig")
  :patterns ("/Kconfig\\.[^/]*$"))

(define-language kconfig
  "Kconfig files: the configuration menus of the Linux kernel, parsed with tree-sitter-kconfig."
  :grammar "kconfig" :modes (kconfig-mode) :indent 4 :queries "kconfig")

(define-major-mode ld-script-mode prog-mode
  "Linker scripts of GNU ld."
  :lighter "LD-Script"
  :block-comment ("/*" "*/")
  :extensions ("ld"))

(define-language ld
  "Linker scripts of GNU ld, parsed with tree-sitter-ld."
  :grammar "ld" :modes (ld-script-mode) :indent 2 :queries "ld" :injection-names "ld")

(define-major-mode bitbake-mode prog-mode
  "BitBake recipes, as the Yocto Project builds with."
  :lighter "BitBake"
  :comment "#"
  :extensions ("bb" "bbappend" "bbclass")
  :patterns ("/recipe-[^/]*/[^/]*/[^/]*\\.inc$"))

(define-language bitbake
  "BitBake recipes, as the Yocto Project builds with, parsed with tree-sitter-bitbake."
  :grammar "bitbake" :modes (bitbake-mode) :indent 4 :queries "bitbake")

(define-major-mode bazel-starlark-mode prog-mode
  "Starlark: the build files of Bazel and Buck."
  :lighter "Starlark"
  :comment "#"
  :extensions ("bzl" "bazel" "star" "bxl")
  :file-names ("BUILD" "BUCK" "WORKSPACE" "WORKSPACE.bzlmod" "PACKAGE")
  :patterns ("/BUILD\\.[^/]*$"))

(define-language starlark
  "Starlark: the build files of Bazel and Buck, parsed with tree-sitter-python."
  :grammar "python" :modes (bazel-starlark-mode) :indent 4 :queries "starlark" :injection-names "starlark|bzl|bazel|buck" :definitions :textobjects)

(define-major-mode rpm-spec-mode prog-mode
  "RPM spec files: how a package is built."
  :lighter "RPM-Spec"
  :comment "#"
  :extensions ("spec"))

(define-language rpmspec
  "RPM spec files: how a package is built, parsed with tree-sitter-rpmspec."
  :grammar "rpmspec" :modes (rpm-spec-mode) :indent 4 :queries "rpmspec")

(define-language git-commit
  "Git's commit messages, parsed with tree-sitter-gitcommit."
  :grammar "gitcommit" :modes (git-commit-mode) :indent 4 :queries "git-commit" :definitions :textobjects)

(define-major-mode amber-mode prog-mode
  "Amber scripts, which compile to Bash."
  :lighter "Amber"
  :comment "//"
  :extensions ("ab"))

(define-language amber
  "Amber scripts, which compile to Bash, parsed with tree-sitter-amber."
  :grammar "amber" :modes (amber-mode) :indent 4 :queries "amber" :definitions :textobjects)

(define-major-mode cylc-mode prog-mode
  "Cylc workflow definitions."
  :lighter "Cylc"
  :comment "#"
  :extensions ("cylc")
  :file-names ("suite.rc"))

(define-language cylc
  "Cylc workflow definitions, parsed with tree-sitter-cylc."
  :grammar "cylc" :modes (cylc-mode) :indent 4 :queries "cylc" :injection-names "cylc" :definitions :textobjects)

(define-major-mode earthfile-mode prog-mode
  "Earthfiles: builds for Earthly."
  :lighter "Earthfile"
  :comment "#"
  :file-names ("Earthfile"))

(define-language earthfile
  "Earthfiles: builds for Earthly, parsed with tree-sitter-earthfile."
  :grammar "earthfile" :modes (earthfile-mode) :indent 2 :queries "earthfile" :injection-names "earthfile" :definitions :textobjects)

(define-major-mode gn-mode prog-mode
  "GN build files, as Chromium builds with."
  :lighter "GN"
  :comment "#"
  :extensions ("gn" "gni"))

(define-language gn
  "GN build files, as Chromium builds with, parsed with tree-sitter-gn."
  :grammar "gn" :modes (gn-mode) :indent 2 :queries "gn" :injection-names "gn")

(define-major-mode go-work-mode prog-mode
  "Go workspace files (go.work)."
  :lighter "Go Work"
  :comment "//"
  :file-names ("go.work"))

(define-language gowork
  "Go workspace files (go.work), parsed with tree-sitter-gowork."
  :grammar "gowork" :modes (go-work-mode) :indent 4 :queries "gowork" :injection-names "gowork")

(define-major-mode gpr-mode prog-mode
  "GNAT project files, for Ada."
  :lighter "GPR"
  :comment "--"
  :extensions ("gpr"))

(define-language gpr
  "GNAT project files, for Ada, parsed with tree-sitter-gpr."
  :grammar "gpr" :modes (gpr-mode) :indent 3 :queries "gpr" :injection-names "gpr")

(define-major-mode jj-description-mode text-mode
  "The descriptions of Jujutsu changes, as jj asks the editor for them."
  :lighter "JJ-Description"
  :comment "JJ:"
  :extensions ("jjdescription"))

(define-language jjdescription
  "The descriptions of Jujutsu changes, as jj asks the editor for them, parsed with tree-sitter-jjdescription."
  :grammar "jjdescription" :modes (jj-description-mode) :indent 4 :queries "jjdescription")

(define-major-mode snakemake-mode prog-mode
  "Snakemake workflows."
  :lighter "Snakemake"
  :comment "#"
  :extensions ("smk")
  :file-names ("Snakefile"))

(define-language snakemake
  "Snakemake workflows, parsed with tree-sitter-snakemake."
  :grammar "snakemake" :modes (snakemake-mode) :indent 2 :queries "snakemake")

(define-major-mode vhs-mode prog-mode
  "VHS tapes: terminal recordings written as scripts."
  :lighter "VHS"
  :comment "#"
  :extensions ("tape"))

(define-language vhs
  "VHS tapes: terminal recordings written as scripts, parsed with tree-sitter-vhs."
  :grammar "vhs" :modes (vhs-mode) :indent 2 :queries "vhs")

(define-major-mode werk-mode prog-mode
  "Werk build files."
  :lighter "Werk"
  :comment "#"
  :extensions ("werk")
  :file-names ("Werkfile"))

(define-language werk
  "Werk build files, parsed with tree-sitter-werk."
  :grammar "werk" :modes (werk-mode) :indent 2 :queries "werk")
