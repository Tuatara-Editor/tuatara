#!/usr/bin/env bash
# tools/possible-copies.sh --- Show what looks like a copy: a branch's additions, alike bodies, pass-throughs.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Lists the definitions (defun, defmacro, defgeneric, defmethod, defvar,
# defparameter) that the changes from BASE to HEAD add, and under each the
# five entries of test/api/reuse-index.txt nearest to it: those whose
# docstring's first sentence shares meaningful words with the new one, or
# whose name shares parts (split at "-") with the new name; rarer words weigh more, and one the docstring names outright more still.
# The definition itself (same name, same file) is left out. Only the diff, the changed files
# and the index are read; nothing is loaded.
# Two more reports read the whole tree (lisp/, src/ and frontends/):
#   --bodies         the pairs of definitions whose bodies are alike. A body
#                    is read with its local variables numbered by first use
#                    and each literal as its kind, so a copy with other
#                    names and other strings still matches. The score is
#                    the share of 4-token runs the two bodies have in common.
#   --pass-through   the functions whose whole body calls one other function
#                    with the arguments it was given, in the order given;
#                    those the reuse index lists (exported) are named apart.
# With --meaning, each added definition also gets the five definitions of the whole tree nearest to it by
# meaning, internal ones too, ranked by a local embedding model (tools/meaning.py, which says how to set it up);
# the two lists are labelled. Without the model it says so in one line and gives the word list alone.
#   --where TEXT...  the ten definitions nearest by meaning to each description TEXT, for finding where code that does
#                    something already lives.
# Usage: tools/possible-copies.sh [--meaning] BASE [HEAD]
#        tools/possible-copies.sh --where "what the code would do" ...
#        tools/possible-copies.sh --bodies [--packages] [--threshold SCORE]
#        tools/possible-copies.sh --pass-through [--packages]
#   HEAD is a commit; without it the working tree is compared, new files included.
#   --packages adds the package repositories in ../tuatara-packages/*/.
#   SCORE is a percentage, 60 unless given.
set -euo pipefail
cd "$(dirname "$0")/.."
exec python3 -I - "$@" <<'PYTHON'
import importlib.util
import json
import math
import os
import re
import subprocess
import sys

INDEX = "test/api/reuse-index.txt"
DEFINERS = ("defun", "defmacro", "defgeneric", "defmethod", "defvar", "defparameter")
SHOWN = 5
MINIMUM = 12.0
# A term only one entry uses weighs this much; commoner ones weigh less.
RAREST = 10.0
# Words that say nothing about what a definition does: English function words and what docstrings repeat.
STOP = set("""a an the of to in on at by for from with without and or but not no is are was were be been being it its
this that these those as if when while then than so such into onto over under after before up out off all any each
every some one two more most other another which what who whom whose how can may might will would should shall must
do does did done has have had there their they them you your we our i also only just both either neither per via
given gives give use used uses using return returns returned true false nil function functions value values
argument arguments list lists name names string strings symbol t""".split())
STOP_PARTS = {"p", "the", "of", "to", "for", "in", "by", "with", "and", "or", "a"}


def run(*command):
    return subprocess.run(command, check=True, capture_output=True, text=True).stdout


def stem(word):
    """The word with a plural or verb ending taken off, so "opens" and "open" agree."""
    for ending in ("ing", "es", "ed", "s"):
        if word.endswith(ending) and len(word) - len(ending) >= 3:
            return word[:-len(ending)]
    return word


def sentence_words(text):
    """The meaningful words of the first sentence of TEXT, stemmed."""
    first = re.split(r"(?<=[.!?])\s", text.strip(), maxsplit=1)[0]
    return {stem(w) for w in re.findall(r"[a-z]+", first.lower()) if len(w) > 2 and w not in STOP}


def name_parts(name):
    """The parts of a Lisp NAME split at "-", without punctuation or filler."""
    parts = re.split(r"[-/:*+<>=\s]+", re.sub(r"\(internal\)", "", name.lower()))
    return {stem(p) for p in parts if p and p not in STOP_PARTS and len(p) > 1}


def read_index():
    """The entries of the index as dicts of name, words, parts, sentence and file."""
    entries = []
    for line in open(INDEX, encoding="utf-8"):
        found = re.match(r"^(\S+(?: \(internal\))?)  (\S+)  (.*)  (\S+)$", line.rstrip("\n"))
        if not found:
            continue
        qualified, kind, sentence, file = found.groups()
        name = qualified.split(":", 1)[-1]
        entries.append({"name": name.replace(" (internal)", ""), "kind": kind, "file": file,
                        "terms": sentence_words(sentence) | name_parts(name)})
    return list({(e["name"], e["file"]): e for e in entries}.values())


def split_form(text, start):
    """The top-level elements of the form beginning at START in TEXT, as strings; a list is kept whole."""
    elements, depth, i, begin = [], 0, start, None
    while i < len(text):
        c = text[i]
        if c == ";":
            i = text.find("\n", i)
            i = len(text) if i < 0 else i
            continue
        if c == "#" and text[i + 1:i + 2] == "\\":
            i += 3
            while i < len(text) and text[i].isalnum():
                i += 1
            continue
        if c == '"':
            j = i + 1
            while j < len(text) and text[j] != '"':
                j += 2 if text[j] == "\\" else 1
            if depth == 1:
                elements.append(text[i:j + 1])
            i = j + 1
            continue
        if c == "(":
            if depth == 1:
                begin = i
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 1:
                elements.append(text[begin:i + 1])
            if depth == 0:
                break
        elif depth == 1 and not c.isspace():
            j = i
            while j < len(text) and not text[j].isspace() and text[j] not in "()\"":
                j += 1
            elements.append(text[i:j])
            i = j
            continue
        i += 1
    return elements


def docstring(text, start):
    """The docstring of the definition beginning at START in TEXT, or an empty string."""
    elements = split_form(text, start)
    kind = elements[0]
    string = None
    if kind in ("defvar", "defparameter"):
        string = elements[3] if len(elements) > 3 else None
    elif kind == "defgeneric":
        found = re.search(r'\(:documentation\s+("(?:[^"\\]|\\.)*")', "".join(elements[2:]))
        string = found.group(1) if found else None
    else:
        lambda_list = next((n for n, e in enumerate(elements[2:], 2) if e.startswith("(")), None)
        if lambda_list is not None and lambda_list + 1 < len(elements):
            string = elements[lambda_list + 1]
    return string[1:-1].replace('\\"', '"') if string and string.startswith('"') else ""


def added_definitions(base, head):
    """A list of (file, name, docstring) for each definition the changes add in a Lisp file of the tree."""
    command = ["git", "diff", "-U0", "--no-color", base] + ([head] if head else []) + ["--", "*.lisp"]
    diff = run(*command)
    lines = {}
    path = None
    for line in diff.splitlines():
        if line.startswith("+++ "):
            path = line[6:] if line.startswith("+++ b/") else None
        elif path and line.startswith("+("):
            lines.setdefault(path, []).append(line[1:])
    if not head:
        for path in run("git", "ls-files", "--others", "--exclude-standard", "--", "*.lisp").split():
            lines.setdefault(path, []).extend(l for l in open(path, encoding="utf-8").read().splitlines()
                                              if l.startswith("("))
    found = []
    for path, added in lines.items():
        if path.startswith(("test/", "tools/", ".project/")):
            continue
        text = run("git", "show", f"{head}:{path}") if head else open(path, encoding="utf-8").read()
        for line in added:
            match = re.match(r"\((" + "|".join(DEFINERS) + r")\s+(\(setf\s+\S+?\)|[^\s()]+)", line)
            if not match:
                continue
            start = text.find(line)
            doc = docstring(text, start) if start >= 0 else ""
            found.append((path, match.group(2).lower(), doc))
    return found


def score(entry, terms, weight, doc):
    """How alike ENTRY is to a definition whose docstring DOC and name give TERMS: the weights of the terms they
share, and a full weight more when DOC names the entry."""
    named = "-" in entry["name"] and entry["name"] in doc.lower()
    return sum(weight[t] for t in terms & entry["terms"]) + (RAREST if named else 0)


# What the meaning lists rank: every definition of these kinds in the tree, internal ones included.
MEANING_DEFINERS = ("defun", "defmacro", "defgeneric", "defmethod", "define-command")
MEANING_SHOWN = 10


def meaning_module():
    """The module tools/meaning.py, loaded from its file, as the isolated interpreter does not search this folder."""
    spec = importlib.util.spec_from_file_location("meaning", "tools/meaning.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


# The model reads at most this many characters of a definition (about 2000 tokens of Lisp, well inside a slot of
# tools/meaning.py's server, which holds 4096).
MEANING_LONGEST = 6000


def signature(text, start):
    """The lambda list of the definition beginning at START in TEXT, whitespace collapsed, or an empty string."""
    elements = split_form(text, start)
    found = next((e for e in elements[2:] if e.startswith("(")), "")
    return re.sub(r"\s+", " ", found)


def meaning_corpus():
    """A list of (file, name, line) for each definition of the tree, LINE being its name, its lambda list, its whole
    docstring with whitespace collapsed and its file, cut at MEANING_LONGEST characters: the text the model reads."""
    found = []
    pattern = re.compile(r"^\((" + "|".join(MEANING_DEFINERS) + r")\s+(\(setf\s+\S+?\)|[^\s()]+)", re.M)
    for path in source_files(False):
        text = open(path, encoding="utf-8", errors="replace").read()
        for match in pattern.finditer(text):
            doc = re.sub(r"\s+", " ", docstring(text, match.start())).strip()
            name = match.group(2).lower()
            line = f"{name}  {signature(text, match.start())}  {doc}"
            found.append((path, name, f"{line[:MEANING_LONGEST - len(path) - 2]}  {path}"))
    return found


def by_meaning(corpus, queries, top):
    """For each of QUERIES a list of (score, index into CORPUS), TOP long, from tools/meaning.py; or None after a
    line on standard error saying why the model could not be used."""
    meaning = meaning_module()
    run_with, reason = meaning.command()
    if not run_with:
        print(f"meaning: {reason}; the word list alone follows", file=sys.stderr)
        return None
    request = json.dumps({"corpus": [line for _, _, line in corpus], "queries": queries, "top": top})
    done = subprocess.run(run_with, input=request, stdout=subprocess.PIPE, text=True)
    if done.returncode != 0:
        print("meaning: the model could not be run (see above); the word list alone follows", file=sys.stderr)
        return None
    return json.loads(done.stdout)["nearest"]


def report_where(descriptions):
    """Print the definitions nearest by meaning to each of DESCRIPTIONS, with scores."""
    corpus = meaning_corpus()
    found = by_meaning(corpus, descriptions, MEANING_SHOWN)
    if found is None:
        sys.exit(1)
    for text, near in zip(descriptions, found):
        print(f"== {text}")
        for points, i in near:
            print(f"  {points:.3f}  {corpus[i][1]}  {corpus[i][0]}")


# The tree's definitions, read for the reports that cover all of it.
TREE_DEFINERS = ("defun", "defmacro", "defmethod")
FOLDERS = ("lisp", "src", "frontends")
# A body of fewer tokens than this is too short to say anything of its likeness.
SHORTEST_BODY = 40
RUN = 4
# A run that more definitions than this share is boilerplate and starts no pair.
COMMON_RUN = 30
BINDERS = {"let", "let*", "dolist", "dotimes", "do-symbols", "do-external-symbols", "multiple-value-bind",
           "destructuring-bind", "lambda", "with-open-file", "with-input-from-string", "with-output-to-string",
           "handler-case"}
TOKEN = re.compile(r'"(?:[^"\\]|\\.)*"|;[^\n]*|#\\(?:[A-Za-z0-9_-]+|.)|[()]|#?[\'`,@]+|[^\s()"\';`,]+')
NUMBER = re.compile(r"^[-+]?(?:\d+\.?\d*(?:[eEdD][-+]?\d+)?|\d+/\d+|#x[0-9a-fA-F]+|#b[01]+|#o[0-7]+)$")


def source_files(packages):
    """The Lisp files the whole-tree reports read: those of the folders, and with PACKAGES those of the package
    repositories (their own git folders, not the worktrees of their branches), leaving out any folder called test or
    tests. Without ../tuatara-packages/ it says so on standard error and goes on."""
    roots = list(FOLDERS)
    if packages:
        base = "../tuatara-packages"
        if os.path.isdir(base):
            roots += [os.path.join(base, d) for d in sorted(os.listdir(base))
                      if os.path.isdir(os.path.join(base, d, ".git"))]
        else:
            print(f"{base}/ is not there: the package repositories are left out", file=sys.stderr)
    for root in roots:
        for folder, names, files in os.walk(root):
            names[:] = sorted(n for n in names if n not in ("test", "tests", ".git"))
            for name in sorted(files):
                if name.endswith(".lisp"):
                    yield os.path.join(folder, name)


def tree_definitions(packages):
    """A list of (file, name, parameters, body) for each defun, defmacro and defmethod of the tree, where
    PARAMETERS is the text of its lambda list and BODY the list of its body forms as text, docstring left out. The
    forms are read with split_form."""
    found = []
    pattern = re.compile(r"^\((" + "|".join(TREE_DEFINERS) + r")\s+(\(setf\s+\S+?\)|[^\s()]+)", re.M)
    for path in source_files(packages):
        text = open(path, encoding="utf-8", errors="replace").read()
        for match in pattern.finditer(text):
            elements = split_form(text, match.start())
            lambda_list = next((n for n, e in enumerate(elements[2:], 2) if e.startswith("(")), None)
            if lambda_list is None:
                continue
            body = elements[lambda_list + 1:]
            if len(body) > 1 and body[0].startswith('"'):
                body = body[1:]
            found.append((path, match.group(2).lower(), elements[lambda_list], body))
    return found


def tokens_of(text):
    """The tokens of TEXT without comments, a string being one token."""
    return [t for t in TOKEN.findall(text) if not t.startswith(";")]


def local_names(parameters, tokens):
    """The names the code binds itself, as a set: its parameters, and what let, dolist, loop and the like name (not
    what their init forms say)."""
    names = {t.lower() for t in tokens_of(parameters) if t not in "()" and not t.startswith("&")}
    for i, token in enumerate(tokens):
        head = token.lower()
        if i and tokens[i - 1] == "(" and head in BINDERS and i + 1 < len(tokens) and tokens[i + 1] == "(":
            if head in ("let", "let*"):
                # (let ((a 1) b) ...): a name is the first token of a binding, or a token standing alone
                depth, j = 0, i + 1
                while j < len(tokens):
                    t = tokens[j]
                    depth += 1 if t == "(" else -1 if t == ")" else 0
                    if depth <= 0:
                        break
                    if t not in "()" and depth == 1:
                        names.add(t.lower())
                    elif t == "(" and depth == 2 and tokens[j + 1] not in "()":
                        names.add(tokens[j + 1].lower())
                    j += 1
            elif head in ("multiple-value-bind", "destructuring-bind", "lambda"):
                # every name in the first list
                depth, j = 0, i + 1
                while j < len(tokens):
                    t = tokens[j]
                    depth += 1 if t == "(" else -1 if t == ")" else 0
                    if depth <= 0:
                        break
                    if t not in "()":
                        names.add(t.lower())
                    j += 1
            elif head != "handler-case" and i + 2 < len(tokens) and tokens[i + 2] not in "()":
                # (dolist (x list) ...): the first token of the list
                names.add(tokens[i + 2].lower())
        elif head in ("for", "with", "as") and i and tokens[i - 1] != "(" and i + 1 < len(tokens):
            nxt = tokens[i + 1]
            if nxt == "(":
                j = i + 2
                while j < len(tokens) and tokens[j] != ")":
                    names.add(tokens[j].lower())
                    j += 1
            else:
                names.add(nxt.lower())
    return {n for n in names if re.match(r"^[a-z][^:]*$", n) and n not in ("nil", "t")}


def normalised(parameters, body):
    """The tokens of BODY (a list of forms as text) with each local name replaced by v and the number of its first
    use, each string by S, each number by N and each character by C, package prefixes dropped."""
    tokens = tokens_of(" ".join(body))
    names = local_names(parameters, tokens)
    numbered, out = {}, []
    for token in tokens:
        if token.startswith('"'):
            out.append("S")
        elif token.startswith("#\\"):
            out.append("C")
        elif NUMBER.match(token):
            out.append("N")
        else:
            word = token.lower()
            if word in names:
                out.append("v%d" % numbered.setdefault(word, len(numbered)))
            else:
                out.append(word.rsplit(":", 1)[-1] if not word.startswith((":", "#")) else word)
    return out


def runs_of(tokens):
    """The set of the runs of RUN successive tokens."""
    return {tuple(tokens[i:i + RUN]) for i in range(len(tokens) - RUN + 1)}


def alike_bodies(packages, threshold):
    """A list of (score, definition, definition) for the pairs of definitions, each (file, name), whose bodies share
    at least THRESHOLD percent of their runs of tokens, best first. A definition and one of the same name in the same
    file (the methods of a generic function) are not a pair."""
    definitions = []
    for path, name, parameters, body in tree_definitions(packages):
        tokens = normalised(parameters, body)
        if len(tokens) >= SHORTEST_BODY:
            definitions.append((path, name, runs_of(tokens)))
    holders = {}
    for n, (_, _, runs) in enumerate(definitions):
        for run in runs:
            holders.setdefault(run, []).append(n)
    shared = {}
    for numbers in holders.values():
        if 2 <= len(numbers) <= COMMON_RUN:
            for a in range(len(numbers)):
                for b in range(a + 1, len(numbers)):
                    pair = (numbers[a], numbers[b])
                    shared[pair] = shared.get(pair, 0) + 1
    found = []
    for (a, b), common in shared.items():
        (file_a, name_a, runs_a), (file_b, name_b, runs_b) = definitions[a], definitions[b]
        if (file_a, name_a) == (file_b, name_b) or common * 100 < threshold * min(len(runs_a), len(runs_b)):
            continue
        score = 100.0 * len(runs_a & runs_b) / len(runs_a | runs_b)
        if score >= threshold:
            found.append((score, (file_a, name_a), (file_b, name_b)))
    return sorted(found, key=lambda f: (-f[0], f[1], f[2]))


def pass_throughs(packages):
    """A list of (file, name, target) for each function whose one body form calls TARGET with its required and
    optional parameters in the order given, or with apply and its rest parameter after them."""
    found = []
    for path, name, parameters, body in tree_definitions(packages):
        body = [b for b in body if not b.startswith("(declare")]
        if len(body) != 1 or not body[0].startswith("(") or name.startswith("("):
            continue
        parts = [t for t in tokens_of(parameters) if t not in "()"]
        call = [t for t in tokens_of(body[0])]
        if any(t.startswith("&") and t.lower() not in ("&optional", "&rest") for t in parts) or call.count("(") != 1:
            continue
        words = [t.lower() for t in call[1:-1]]
        arguments = [p.lower() for p in parts if p.lower() not in ("&optional", "&rest")]
        if words and words[0] == "apply" and "&rest" in [p.lower() for p in parts]:
            words = words[1:]
            words[0] = words[0].lstrip("#'")
        if words and words[1:] == arguments and words[0] != name and not words[0].startswith(("'", "#")):
            found.append((path, name, words[0]))
    return found


def report_alike(packages, threshold):
    """Print the alike bodies, one pair to a line with the score."""
    pairs = alike_bodies(packages, threshold)
    print(f"{len(pairs)} pairs of definitions with alike bodies (score {threshold:g} or more)")
    for score, (file_a, name_a), (file_b, name_b) in pairs:
        print(f"  {score:5.1f}  {name_a}  {file_a}  {name_b}  {file_b}")


def report_pass_through(packages):
    """Print the pass-through functions: those nothing lists as exported first, then the exported ones."""
    exported = {e["name"] for e in read_index()}
    found = pass_throughs(packages)
    kept = lambda name: name in exported or ":" in name
    internal = [f for f in found if not kept(f[1])]
    api = [f for f in found if kept(f[1])]
    print(f"{len(internal)} functions that only call another with their own arguments")
    for path, name, target in internal:
        print(f"  {name}  {path}  calls {target}")
    print(f"{len(api)} more are exported or documented in the reuse index, an interface kept on purpose")
    for path, name, target in api:
        print(f"  {name}  {path}  calls {target}")


def main(argv):
    if argv[:1] == ["--where"]:
        return report_where(argv[1:]) if argv[1:] else sys.exit("usage: --where DESCRIPTION...")
    options = {a for a in argv if a in ("--bodies", "--pass-through", "--packages", "--meaning")}
    threshold = 60.0
    if "--threshold" in argv:
        at = argv.index("--threshold")
        threshold = float(argv[at + 1])
        argv = argv[:at] + argv[at + 2:]
    argv = [a for a in argv if a not in options]
    if "--bodies" in options:
        return report_alike("--packages" in options, threshold)
    if "--pass-through" in options:
        return report_pass_through("--packages" in options)
    if not 1 <= len(argv) <= 2:
        sys.exit("usage: tools/possible-copies.sh [--meaning] BASE [HEAD] | --bodies [--packages] [--threshold SCORE] "
                 "| --pass-through [--packages] | --where DESCRIPTION...")
    base, head = argv[0], (argv[1] if len(argv) > 1 else None)
    entries = read_index()
    frequency = {}
    for entry in entries:
        for term in entry["terms"]:
            frequency[term] = frequency.get(term, 0) + 1
    scale = RAREST / math.log(len(entries))
    weight = {t: scale * math.log(len(entries) / n) for t, n in frequency.items()}
    added = added_definitions(base, head)
    if "--meaning" in options and not added:
        print(f"no definitions added since {base}")
    corpus = meaning_corpus() if "--meaning" in options and added else []
    meant = by_meaning(corpus, [f"{name}: {doc}" for _, name, doc in added], MEANING_SHOWN) if corpus else None
    for number, (path, name, doc) in enumerate(added):
        terms = sentence_words(doc) | name_parts(name)
        for term in terms:
            weight.setdefault(term, RAREST)
        near = sorted(((score(e, terms, weight, doc), e) for e in entries
                       if (e["file"], e["name"]) != (path, name)),
                      key=lambda pair: (-pair[0], pair[1]["name"]))
        near = [pair for pair in near[:SHOWN] if pair[0] >= MINIMUM]
        close = [(points, corpus[i]) for points, i in meant[number] if corpus[i][:2] != (path, name)][:SHOWN] \
            if meant else []
        if not near and not close:
            continue
        print(f"{path}  {name}")
        if meant:
            print("  by words:")
        for points, entry in near:
            print(f"  {points:5.1f}  {entry['name']}  {entry['file']}")
        if close:
            print("  by meaning:")
            for points, (file, other, _) in close:
                print(f"  {points:5.3f}  {other}  {file}")


main(sys.argv[1:])
PYTHON
