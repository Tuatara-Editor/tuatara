# tools/lisp_text.py --- Read Lisp source text as the Lisp reader splits it, for the scripts of tools/.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Lisp text split into strings, comments, characters, parentheses and atoms, the one way the tools read it."""
import re

# what is not code: a string, a line comment, a block comment (not nested), a character such as #\" or #\Space
LEXEME = r'"(?:[^"\\]|\\.)*"|;[^\n]*|#\|.*?\|#|#\\.[^\s()";]*'
NOT_CODE = re.compile(LEXEME, re.S)
TOKEN = re.compile(LEXEME + r'|#\(|[()]|[^\s()";]+', re.S)
QUOTES = re.compile(r"#?['`,@]+")


def is_comment(token):
    """Whether TOKEN, one of TOKENS', is a comment."""
    return token.startswith((";", "#|"))


def tokens(text, start=0):
    """The (offset, token) pairs of TEXT from START on: each string, comment and character whole, an opening
    parenthesis (#( for a vector), a closing one, and each atom as written, quote marks before it included."""
    for match in TOKEN.finditer(text, start):
        yield match.start(), match.group()


def code_only(text):
    """TEXT with every string, comment and character blanked (line ends kept, so line numbers stay), leaving the
    code alone."""
    return NOT_CODE.sub(lambda match: re.sub(r"[^\n]", " ", match.group()), text)


def split_form(text, start):
    """The top-level elements of the form beginning at START in TEXT, as strings, comments left out; a list is kept
    whole."""
    elements, depth, begin = [], 0, None
    for offset, token in tokens(text, start):
        if is_comment(token):
            continue
        if token in ("(", "#("):
            if depth == 1:
                begin = offset
            depth += 1
        elif token == ")":
            depth -= 1
            if depth == 1:
                elements.append(text[begin:offset + 1])
            if depth == 0:
                break
        elif depth == 1:
            elements.append(token)
    return elements


class Form:
    """A list read from the text: where it starts and what it holds (Forms, strings as str, atoms as Atom)."""

    def __init__(self, start):
        """A list begun at the offset START, empty so far."""
        self.start = start
        self.items = []


class Atom(str):
    """A symbol or number as written."""


def read_forms(text):
    """The top-level lists of TEXT as Forms, strings and comments read as Lisp reads them."""
    top = Form(0)
    stack = [top]
    for offset, token in tokens(text):
        if token in ("(", "#("):
            form = Form(offset)
            stack[-1].items.append(form)
            stack.append(form)
        elif token == ")":
            if len(stack) > 1:
                stack.pop()
        elif is_comment(token):
            continue
        elif token.startswith('"'):
            stack[-1].items.append(re.sub(r"\\(.)", r"\1", token[1:-1], flags=re.S))
        else:
            stack[-1].items.append(Atom(token))
    return [item for item in top.items if isinstance(item, Form)]
