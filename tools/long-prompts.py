#!/usr/bin/env python3
# tools/long-prompts.py --- List the questions asked at a long prompt with no details beside it.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""List the questions whose prompt is built from more than 80 columns of text and that give no :details.

Usage: tools/long-prompts.py [--summary] [--packages] [ROOT...]

Each ROOT is a repository (the current directory when left out); with
--packages the package repositories beside this one (../tuatara-packages/*/)
are looked at as well. Test files are left out. A question is a call of one of
the question functions (QUESTIONS), with or without a package name before it.
Its prompt is its first argument, and the text it is built from is every
string written in that argument (a format control string's line breaks after
a tilde left out, and of branches such as an IF's or a format choice's only
the longest). A prompt
that is a variable is measured by the form the nearest LET-style binding
before the call gives it, and one that is a call of a function defined in the
same file by the form that function returns. A variable in the prompt that
a LET-style binding gives a call of such a function that returns a FORMAT counts
that format control's text as well. A question asked at such
a prompt should keep the prompt to the thing asked and its keys, the rest
given as :details. Prints one line per question as FILE:LINE: NAME COLUMNS,
then the total. With --summary it prints the count for each file instead. The
list is for a reviewer to judge: it always exits with 0.
"""
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from repositories import lisp_files, package_roots

QUESTIONS = {
    "read-answer-key", "y-or-n-p*", "yes-or-no-p*", "read-string", "completing-read", "completing-read-value",
    "read-password", "read-typed-answer", "read-whole-number", "read-decimal-number", "read-fraction",
    "read-several", "read-path", "read-file-path", "read-directory-path", "read-native-path",
    "read-new-native-path", "read-date",
}
LIMIT = 80
TOKEN = re.compile(r'"(?:[^"\\]|\\.)*"|;[^\n]*|#\|.*?\|#|#\\.[^\s()]*|#\(|[()]|[^\s()";]+', re.S)


class Form:
    """A list read from the text: where it starts and what it holds (Forms, strings as str, atoms as Atom)."""

    def __init__(self, start):
        """A list begun at the offset START, empty so far."""
        self.start = start
        self.items = []


class Atom(str):
    """A symbol or number as written."""


def read_forms(text):
    """The top-level lists of TEXT, strings and comments read as Lisp reads them."""
    top = Form(0)
    stack = [top]
    for match in TOKEN.finditer(text):
        token = match.group()
        if token in ("(", "#("):
            form = Form(match.start())
            stack[-1].items.append(form)
            stack.append(form)
        elif token == ")":
            if len(stack) > 1:
                stack.pop()
        elif token.startswith(";") or token.startswith("#|"):
            continue
        elif token.startswith('"'):
            stack[-1].items.append(re.sub(r"\\(.)", r"\1", token[1:-1], flags=re.S))
        else:
            stack[-1].items.append(Atom(token))
    return [item for item in top.items if isinstance(item, Form)]


BRANCHES = {"if": 2, "or": 1, "cond": 1, "case": 2, "ecase": 2, "typecase": 2, "etypecase": 2}
CHOICE = re.compile(r"~[:@]?\[((?:(?!~[:@]?\[).)*?)~\]", re.S)


def longest_choices(control):
    """The format CONTROL string with each of its choices (~[...~;...~]) cut to its longest clause."""
    while True:
        cut = CHOICE.sub(lambda match: max(re.split(r"~:?;", match.group(1)), key=len), control)
        if cut == control:
            return control
        control = cut


def text_columns(item):
    """How many columns of text ITEM is built from: the length of each string in it, a tilde and the line break and
    spaces after it counting for nothing. Of the branches of an IF, OR, COND or CASE, and of the clauses of a
    format choice, only the longest counts, as only one of them is the prompt; of an AND only the last."""
    if isinstance(item, Form):
        head = name_of(item.items[0]) if item.items and isinstance(item.items[0], Atom) else ""
        if head == "and":
            return text_columns(item.items[-1]) if len(item.items) > 1 else 0
        if head in BRANCHES:
            return max((text_columns(inner) for inner in item.items[BRANCHES[head]:]), default=0)
        return sum(text_columns(inner) for inner in item.items)
    if isinstance(item, Atom) or not isinstance(item, str):
        return 0
    return len(longest_choices(re.sub(r"~\n\s*", "", item)))


def name_of(atom):
    """ATOM's symbol name in lower case, without a package name before it."""
    return atom.lower().rsplit(":", 1)[-1].lstrip("#'")


def binding_before(forms, symbol, position):
    """The form the nearest (SYMBOL FORM) binding before POSITION in FORMS gives, or None."""
    found = None
    for form in forms:
        if form.start >= position:
            break
        items = form.items
        if len(items) == 2 and isinstance(items[0], Atom) and name_of(items[0]) == symbol:
            found = items[1]
    return found


def all_forms(form):
    """FORM and every list inside it, in the order they start."""
    yield form
    for item in form.items:
        if isinstance(item, Form):
            yield from all_forms(item)


WRAPPERS = {"let", "let*", "flet", "labels", "progn", "when", "unless", "multiple-value-bind", "destructuring-bind"}


def returned(form):
    """The form whose value FORM returns, through the LETs and the like around it."""
    while (isinstance(form, Form) and len(form.items) > 1 and isinstance(form.items[0], Atom)
           and name_of(form.items[0]) in WRAPPERS):
        form = form.items[-1]
    return form


def defined_bodies(tops):
    """What the functions TOPS, top-level forms, define return, by name: the last form of each body, through the
    LETs and the like around it (RETURNED)."""
    bodies = {}
    for top in tops:
        items = top.items
        if len(items) > 3 and isinstance(items[0], Atom) and items[0].lower() == "defun" and isinstance(items[1], Atom):
            body = items[3:]
            if len(body) > 1 and isinstance(body[0], str) and not isinstance(body[0], Atom):
                body = body[1:]
            bodies[name_of(items[1])] = returned(body[-1])
    return bodies


def bound_call_columns(prompt, forms, bodies, position):
    """The columns of text a question's PROMPT gets from its variables: for each one that the nearest LET-style
    binding before POSITION in FORMS gives a call of a function in BODIES that returns a FORMAT, that control string."""
    total = 0
    for inner in all_forms(prompt):
        for item in inner.items:
            if isinstance(item, Atom) and not item.startswith(":"):
                bound = binding_before(forms, name_of(item), position)
                if isinstance(bound, Form) and bound.items and isinstance(bound.items[0], Atom):
                    made = bodies.get(name_of(bound.items[0]))
                    if isinstance(made, Form) and len(made.items) > 2 and name_of(made.items[0]) == "format":
                        total += text_columns(made.items[2])
    return total


def long_prompts(text):
    """The (line number, question name, columns) of each question in TEXT asked at a long prompt with no details."""
    found = []
    tops = read_forms(text)
    bodies = defined_bodies(tops)
    for top in tops:
        forms = list(all_forms(top))
        for form in forms:
            items = form.items
            if len(items) < 2 or not isinstance(items[0], Atom) or items[0].startswith("#'"):
                continue
            name = name_of(items[0])
            if name not in QUESTIONS or any(isinstance(i, Atom) and i.lower() == ":details" for i in items[2:]):
                continue
            prompt = items[1]
            if isinstance(prompt, Atom) and not prompt.startswith(":"):
                prompt = binding_before(forms, name_of(prompt), form.start)
            if isinstance(prompt, Form) and prompt.items and isinstance(prompt.items[0], Atom):
                prompt = bodies.get(name_of(prompt.items[0]), prompt)
            columns = text_columns(prompt) if prompt is not None else 0
            if isinstance(prompt, Form):
                columns += bound_call_columns(prompt, forms, bodies, form.start)
            if columns > LIMIT:
                found.append((text.count("\n", 0, form.start) + 1, name, columns))
    return found


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    summary = "--summary" in argv
    roots = [Path(a) for a in argv if not a.startswith("--")]
    if "--packages" in argv:
        roots = (roots or [Path(".")]) + package_roots()
    roots = roots or [Path(".")]
    total = 0
    for root in roots:
        for name in lisp_files(root):
            try:
                text = (root / name).read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            found = long_prompts(text)
            total += len(found)
            if summary and found:
                print(f"{len(found):6d} {root.resolve().name}/{name}")
            elif not summary:
                for number, question, columns in found:
                    print(f"{root.resolve().name}/{name}:{number}: {question} {columns}")
    print(f"{total} questions with a prompt of more than {LIMIT} columns of text and no details")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
