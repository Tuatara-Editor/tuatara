#!/usr/bin/env python3
"""tools/meaning.py --- Rank text by meaning with a local embedding model.
Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

A development aid for tools/possible-copies.sh; nothing of it goes into the
editor or a release. It reads JSON on standard input and writes JSON:

  {"corpus": [line, ...], "queries": [text, ...], "top": 5}
  -> {"nearest": [[[score, index into corpus], ...], ...]}   one list per query

Each corpus line becomes a vector, once: the vectors are kept in the cache
folder by the hash of the line, so a later run embeds only the lines that
changed. A query is embedded in the form the model was trained on, with a
task sentence before it, and the corpus lines are ranked by cosine likeness
to it. The result is a list to read, never a verdict.

Run it where llama-server and numpy are. Set-up, once:

  toolbox create tuatara-ai
  toolbox run -c tuatara-ai sudo dnf install llama-cpp python3-numpy
  mkdir -p ~/.cache/tuatara-ai/models
  curl -L -o ~/.cache/tuatara-ai/models/Qwen3-Embedding-0.6B-Q8_0.gguf \
    https://huggingface.co/Qwen/Qwen3-Embedding-0.6B-GGUF/resolve/main/Qwen3-Embedding-0.6B-Q8_0.gguf
  sha256sum ~/.cache/tuatara-ai/models/Qwen3-Embedding-0.6B-Q8_0.gguf
  # must be 06507c7b42688469c4e7298b0a1e16deff06caf291cf0a5b278c308249c3e439

Settings, taken from the environment when set:

  TUATARA_AI_MODEL      path of the model file
  TUATARA_AI_CACHE      folder of the cached vectors
  TUATARA_AI_TOOLBOX    name of the toolbox that has llama-server
  TUATARA_AI_THREADS    threads the server uses (default: all processors)
  TUATARA_AI_SERVER     path of llama-server (default: the Vulkan build below
                        when it is there, else llama-server from the path)

A graphics processor makes a first run much quicker. Fedora's llama-cpp is
processor only, so build llama.cpp with Vulkan in the same toolbox:

  toolbox run -c tuatara-ai sudo dnf install cmake gcc-c++ git \
    vulkan-loader-devel vulkan-headers glslc
  cd ~/.local/share/tuatara-ai
  git clone --depth 1 --branch b11819 https://github.com/ggml-org/llama.cpp llama.cpp-b11819
  git -C llama.cpp-b11819 rev-parse HEAD
  # must be 0693dbfb915f33233818373b5f6969396658d02c
  cmake -S llama.cpp-b11819 -B llama.cpp-b11819/build -DGGML_VULKAN=ON -DCMAKE_BUILD_TYPE=Release
  cmake --build llama.cpp-b11819/build --target llama-server -j 24
  llama-server --list-devices   # the card should be listed as Vulkan0

Dependency: ggml-org/llama.cpp, MIT licence, release b11819, commit
0693dbfb915f33233818373b5f6969396658d02c (an old enough release, pinned). The
module uses that build, with every layer on the card (-ngl 99), when it is
there. Check that the card's vectors match the processor's (cosine above
0.999) before trusting it; if not, set TUATARA_AI_SERVER to Fedora's.

Embedding the whole tree (9364 definitions) took 82 minutes on the processor;
a second run only embeds changed definitions.
"""
import hashlib
import json
import os
import shutil
import socket
import subprocess
import sys
import time
import urllib.request

CACHE = os.environ.get("TUATARA_AI_CACHE", os.path.expanduser("~/.cache/tuatara-ai"))
MODEL = os.environ.get("TUATARA_AI_MODEL", os.path.join(os.path.expanduser("~/.cache/tuatara-ai"),
                                                         "models/Qwen3-Embedding-0.6B-Q8_0.gguf"))
TOOLBOX = os.environ.get("TUATARA_AI_TOOLBOX", "tuatara-ai")
THREADS = os.environ.get("TUATARA_AI_THREADS", str(os.cpu_count() or 4))
GPU_SERVER = os.path.expanduser("~/.local/share/tuatara-ai/llama.cpp-b11819/build/bin/llama-server")
SERVER = os.environ.get("TUATARA_AI_SERVER", GPU_SERVER if os.path.isfile(GPU_SERVER) else "llama-server")
TASK = "Given a description of what a piece of code does, find the existing function that does it"
BATCH = 64


def command():
    """The command that runs this file where llama-server and numpy are, as a list, or None with a one-line reason
    in the second place of the pair (COMMAND, REASON): the model file must be there, and llama-server on the path or
    in the toolbox TOOLBOX."""
    if not os.path.isfile(MODEL):
        return None, f"the model file {MODEL} is not there (set-up: see the header of tools/meaning.py)"
    me = [os.path.abspath(__file__)]
    if SERVER == "llama-server" and shutil.which(SERVER):
        return ["python3", "-I"] + me, None
    if shutil.which("toolbox"):
        probe = subprocess.run(["toolbox", "run", "-c", TOOLBOX, "sh", "-c", f'command -v "{SERVER}"'],
                               capture_output=True)
        if probe.returncode == 0:
            settings = [f"{k}={v}" for k, v in os.environ.items() if k.startswith("TUATARA_AI_")]
            return ["toolbox", "run", "-c", TOOLBOX, "env"] + settings + ["python3", "-I"] + me, None
    return None, f"{SERVER} is not on the path and not in a toolbox called {TOOLBOX}"


def nearest(corpus, queries, top):
    """For each of QUERIES, the TOP pairs (score, index into CORPUS) of the corpus lines nearest by meaning, best
    first. Needs llama-server, the model and numpy."""
    import numpy as np
    embedder = Embedder(np)
    try:
        matrix = corpus_vectors(embedder, np, corpus)
        found = []
        for vector in embedder.vectors([f"Instruct: {TASK}\nQuery: {q}" for q in queries]):
            scores = matrix @ vector
            found.append([[float(scores[i]), int(i)] for i in np.argsort(-scores)[:top]])
        return found
    finally:
        embedder.close()


class Embedder:
    """A llama-server running the embedding model on 127.0.0.1, ended when the object is closed."""

    def __init__(self, np):
        self.np = np
        with socket.socket() as s:
            s.bind(("127.0.0.1", 0))
            self.port = s.getsockname()[1]
        self.process = subprocess.Popen(
            [SERVER, "-m", MODEL, "--embeddings", "--pooling", "last", "--host", "127.0.0.1",
             "--port", str(self.port), "-c", "8192", "-b", "2048", "-ub", "2048", "-t", THREADS,
             "--parallel", "4", "--log-disable"]
            + (["-ngl", "99"] if SERVER != "llama-server" else []),
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        deadline = time.time() + 120
        while time.time() < deadline:
            try:
                with urllib.request.urlopen(f"http://127.0.0.1:{self.port}/health", timeout=2) as r:
                    if r.status == 200:
                        return
            except OSError:
                time.sleep(0.3)
        self.close()
        sys.exit("llama-server did not start")

    def vectors(self, texts):
        """Unit vectors for TEXTS, one row each, in order."""
        body = json.dumps({"input": texts}).encode()
        request = urllib.request.Request(f"http://127.0.0.1:{self.port}/v1/embeddings", body,
                                         {"Content-Type": "application/json"})
        with urllib.request.urlopen(request, timeout=600) as r:
            data = sorted(json.load(r)["data"], key=lambda d: d["index"])
        m = self.np.array([d["embedding"] for d in data], dtype=self.np.float32)
        return m / self.np.linalg.norm(m, axis=1, keepdims=True)

    def close(self):
        """End the server."""
        self.process.terminate()
        try:
            self.process.wait(10)
        except subprocess.TimeoutExpired:
            self.process.kill()


def corpus_vectors(embedder, np, lines):
    """A matrix with one unit vector per line of LINES, read from the cache where the line is known and embedded
    (and then cached) where it is not. Progress goes to standard error."""
    os.makedirs(CACHE, exist_ok=True)
    path = os.path.join(CACHE, "definition-vectors.npz")
    known = {}
    if os.path.exists(path):
        saved = np.load(path)
        known = dict(zip(saved["keys"].tolist(), saved["vectors"]))
    keys = [hashlib.sha256(l.encode()).hexdigest()[:24] for l in lines]
    missing = [i for i, k in enumerate(keys) if k not in known]
    for start in range(0, len(missing), BATCH):
        part = missing[start:start + BATCH]
        for i, v in zip(part, embedder.vectors([lines[i] for i in part])):
            known[keys[i]] = v
        print(f"\rembedded {min(start + BATCH, len(missing))}/{len(missing)} new definitions", end="",
              file=sys.stderr, flush=True)
        if (start // BATCH) % 50 == 49:
            save(np, path, known)
    if missing:
        print(file=sys.stderr)
        save(np, path, known)
    return np.array([known[k] for k in keys])


def save(np, path, known):
    """Write the vectors KNOWN, a dict of key to vector, to the cache file PATH, whole or not at all."""
    temporary = path + ".part"
    with open(temporary, "wb") as f:
        np.savez(f, keys=np.array(list(known)), vectors=np.array(list(known.values())))
    os.replace(temporary, path)


def main():
    request = json.load(sys.stdin)
    json.dump({"nearest": nearest(request["corpus"], request["queries"], request.get("top", 5))}, sys.stdout)


if __name__ == "__main__":
    main()
