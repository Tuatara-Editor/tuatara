#!/usr/bin/env python3
# tools/long-prompts.py --- List the questions asked at a long prompt with no details beside it.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""List the questions whose prompt is long or says more than what is asked, and that give no :details.

Usage: tools/long-prompts.py [--summary] [--packages] [ROOT...]

Each ROOT is a repository (the current directory when left out); with
--packages the package repositories beside this one (../tuatara-packages/*/)
are looked at as well. Test files are left out. A question function is one
the editor defines (this checkout's own sources are always read for them)
with a required parameter called PROMPT or QUESTION, and a question is a call
of one, with or without a package name before it, that argument being its
prompt. The prompt's length is the text it is built from: every string
written in it (a format control string's line breaks after a tilde left out,
and of branches such as an IF's or a format choice's only the longest), each
argument a format control writes counting ARGUMENT_WIDTH columns. A prompt
that is a variable is measured by the form the nearest LET-style binding
before the call gives it, and one that is a call of a function the editor or
the file defines by the form that function returns. A variable in the prompt
that a LET-style binding gives a call of such a function that returns a
FORMAT counts that format control's text as well. A prompt is listed when its
text is more than LIMIT columns, or whatever its length when it formats a path
(path), a list (list; so is a call of LIST-QUESTION inside a format), more than keys after its question mark
(after ?) or a clause ending in a semicolon before it (before ?).
Such a question should keep the prompt to the thing asked and its keys, the
rest given as :details. Prints one line per question as
REPOSITORY/FILE:LINE: NAME COLUMNS and what else it does, with where the
prompt is made when that is in another file, then the total. With --summary
it prints the count for each file instead. The list is for a reviewer to
judge: it always exits with 0.
"""
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lisp_text import Atom, Form, read_forms
from repositories import lisp_files, report, report_roots

LIMIT = 80
ARGUMENT_WIDTH = 10
# a directive of a format control that writes one argument (~a, ~:d, ~10s ...), and any directive
ARGUMENT = re.compile(r"~(?:[0-9,#vV]|'.)*[:@]*[aAsSdDrRbBoOxXfFeEgGcC]")
DIRECTIVE = re.compile(r"~(?:[0-9,#vV]|'.)*[:@]*.", re.S)
PATH = re.compile(r"(^|-)(path|file|directory|filename|namestring)s?$")
# after the last question mark keys are written in words of three letters at most: a longer word, or an argument
# (CONTROL_TEXT's x), says more
AFTER_QUESTION = re.compile(r"\?[^?]*[A-Za-z]{4}[^?]*$")
# a clause ending in a semicolon before the question mark is an explanation: it belongs in the details
BEFORE_QUESTION = re.compile(r";[^?;]*\?")


BRANCHES = {"if": 2, "or": 1, "cond": 1, "case": 2, "ecase": 2, "typecase": 2, "etypecase": 2}
CHOICE = re.compile(r"~[:@]?\[((?:(?!~[:@]?\[).)*?)~\]", re.S)


def longest_choices(control):
    """The format CONTROL string with each of its choices (~[...~;...~]) cut to its longest clause."""
    while True:
        cut = CHOICE.sub(lambda match: max(re.split(r"~:?;", match.group(1)), key=len), control)
        if cut == control:
            return control
        control = cut


def control_text(control):
    """The text the format CONTROL string writes: its line breaks after a tilde left out, each choice cut to its
    longest clause, each argument it writes ARGUMENT_WIDTH columns of x and its other directives nothing."""
    control = longest_choices(re.sub(r"~\n\s*", "", control)).replace("~~", "\0")
    return DIRECTIVE.sub("", ARGUMENT.sub("x" * ARGUMENT_WIDTH, control)).replace("\0", "~")


def name_of(atom):
    """ATOM's symbol name in lower case, without a package name before it."""
    return atom.lower().rsplit(":", 1)[-1].lstrip("#'")


def head_of(form):
    """The name FORM, a list, starts with, or an empty string."""
    return name_of(form.items[0]) if form.items and isinstance(form.items[0], Atom) else ""


def path_like(item):
    """Whether ITEM, an argument of a format, is a path: a variable named for one or a call that makes a name."""
    if isinstance(item, Atom):
        return bool(PATH.search(name_of(item)))
    return isinstance(item, Form) and "namestring" in head_of(item)


def text_reasons(text):
    """The reasons, among after ? and before ?, that the TEXT of a prompt says more than what is asked."""
    return ({"after ?"} if AFTER_QUESTION.search(text) else set()) | (
        {"before ?"} if BEFORE_QUESTION.search(text) else set())


def measure(item):
    """The (columns, reasons) of the prompt ITEM. COLUMNS is the length of each string in it, a format control's as
    CONTROL_TEXT gives it; of the branches of an IF, OR, COND or CASE, and of the clauses of a format choice, only
    the longest counts, as only one of them is the prompt; of an AND only the last. REASONS is the set of what
    lists it whatever its length: path, list, after ?, before ?."""
    if isinstance(item, Form):
        head = head_of(item)
        if head == "list-question":
            return 0, {"list"}
        if head == "and":
            return measure(item.items[-1]) if len(item.items) > 1 else (0, set())
        if head in BRANCHES:
            parts = [measure(inner) for inner in item.items[BRANCHES[head]:]] or [(0, set())]
            return max(columns for columns, _ in parts), set().union(*(reasons for _, reasons in parts))
        control = item.items[2] if head == "format" and len(item.items) > 2 else None
        if isinstance(control, str) and not isinstance(control, Atom):
            text = control_text(control)
            reasons = {"list"} if "~{" in control else set()
            if any(path_like(argument) for argument in item.items[3:]):
                reasons.add("path")
            reasons |= text_reasons(text)
            reasons.update(*(measure(argument)[1] for argument in item.items[3:] if isinstance(argument, Form)))
            return len(text), reasons
        parts = [measure(inner) for inner in item.items]
        return sum(columns for columns, _ in parts), set().union(set(), *(reasons for _, reasons in parts))
    if isinstance(item, Atom) or not isinstance(item, str):
        return 0, set()
    text = longest_choices(re.sub(r"~\n\s*", "", item))
    return len(text), text_reasons(text)


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


def definitions(tops):
    """The functions TOPS, top-level forms, define: a list of (name, required parameters, what it returns, offset).
    What it returns is the last form of its body, through the LETs and the like around it (RETURNED)."""
    found = []
    for top in tops:
        items = top.items
        if (len(items) > 3 and isinstance(items[0], Atom) and items[0].lower() == "defun"
                and isinstance(items[1], Atom) and isinstance(items[2], Form)):
            body = items[3:]
            if len(body) > 1 and isinstance(body[0], str) and not isinstance(body[0], Atom):
                body = body[1:]
            required = []
            for parameter in items[2].items:
                if isinstance(parameter, Atom) and parameter.startswith("&"):
                    break
                required.append(name_of(parameter) if isinstance(parameter, Atom) else "")
            found.append((name_of(items[1]), required, returned(body[-1]), top.start))
    return found


class Sources:
    """What the files read define, for telling a question and where its prompt is made: the question functions,
    by name, with the place of their prompt among the arguments, and what each function returns."""

    def __init__(self):
        """Nothing read yet."""
        self.questions = {}
        self.made = {}
        self.forms = {}

    def read(self, path, label, text):
        """Take in the file at PATH, shown as LABEL, of TEXT."""
        tops = read_forms(text)
        self.forms[path] = tops
        for name, required, made, start in definitions(tops):
            for place, parameter in enumerate(required, 1):
                if parameter in ("prompt", "question"):
                    self.questions[name] = place
                    break
            self.made.setdefault(name, []).append((made, f"{label}:{text.count(chr(10), 0, start) + 1}", path))

    def made_by(self, name, path):
        """What the function NAME returns and where it is defined, for a call in the file at PATH: its definition
        in that file, else its one definition elsewhere; (None, None) when there is neither."""
        found = self.made.get(name, [])
        mine = [(made, where) for made, where, at in found if at == path]
        if mine:
            return mine[-1][0], None
        return (found[0][0], found[0][1]) if len(found) == 1 else (None, None)


def bound_call_columns(prompt, forms, sources, path, position):
    """The columns of text a question's PROMPT gets from its variables: for each one that the nearest LET-style
    binding before POSITION in FORMS gives a call of a function SOURCES knows that returns a FORMAT, that format's."""
    total = 0
    for inner in all_forms(prompt):
        for item in inner.items:
            if isinstance(item, Atom) and not item.startswith(":"):
                bound = binding_before(forms, name_of(item), position)
                if isinstance(bound, Form) and head_of(bound):
                    made, _ = sources.made_by(head_of(bound), path)
                    if isinstance(made, Form) and head_of(made) == "format":
                        total += measure(made)[0]
    return total


def long_prompts(sources, path, text):
    """The rows for each question in TEXT, the file at PATH, asked at a long prompt with no details, as
    LINE: NAME COLUMNS, what else it does, and where the prompt is made when that is in another file."""
    found = []
    for top in sources.forms.get(path) or read_forms(text):
        forms = list(all_forms(top))
        for form in forms:
            items = form.items
            if len(items) < 2 or not isinstance(items[0], Atom) or items[0].startswith("#'"):
                continue
            name = name_of(items[0])
            place = sources.questions.get(name)
            if (place is None or len(items) <= place
                    or any(isinstance(i, Atom) and i.lower() == ":details" for i in items[place + 1:])):
                continue
            prompt, elsewhere = items[place], None
            if isinstance(prompt, Atom) and not prompt.startswith(":"):
                prompt = binding_before(forms, name_of(prompt), form.start)
            if isinstance(prompt, Form) and head_of(prompt):
                made, where = sources.made_by(head_of(prompt), path)
                if made is not None:
                    prompt, elsewhere = made, where
            columns, reasons = measure(prompt) if prompt is not None else (0, set())
            if isinstance(prompt, Form):
                columns += bound_call_columns(prompt, forms, sources, path, form.start)
            if columns > LIMIT or reasons:
                row = f"{text.count(chr(10), 0, form.start) + 1}: {name} {columns}"
                row += "".join(f", {reason}" for reason in sorted(reasons))
                found.append(row + (f" (made at {elsewhere})" if elsewhere else ""))
    return found


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    sources = Sources()
    roots = {Path(__file__).resolve().parent.parent, *(root.resolve() for root in report_roots(argv))}
    for root in sorted(roots):
        for name in lisp_files(root):
            try:
                sources.read(root / name, f"{root.name}/{name}", (root / name).read_text(encoding="utf-8"))
            except (OSError, UnicodeDecodeError):
                continue
    return report(argv, lisp_files, lambda path, text: long_prompts(sources, path.resolve(), text),
                  "questions with a long prompt or more than keys after its question mark, and no details")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
