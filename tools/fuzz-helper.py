# tools/fuzz-helper.py --- Send the syntax helper requests made to be wrong.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/fuzz-helper.sh: python3 -I tools/fuzz-helper.py HELPER GRAMMARS ROUNDS SEED LOG
# Starts HELPER (syntax/helper.c, built with the sanitizers) on the grammars of
# GRAMMARS, as the editor starts it, and sends it ROUNDS rounds of requests of
# every kind it takes, and some it does not: each made from the parsers,
# trees and queries it holds, then often with its numbers, counts and lengths
# made wrong, cut short or replaced by random octets, all from SEED. The text
# a parse asks for is given from texts made to be hard on a parser. Fails
# (exit 1) when the helper ends, or a reply does not answer the request it
# follows; a connection the helper closes on a request it cannot read is
# opened again. What the helper writes, its sanitizers' reports among it,
# goes to LOG; the last requests sent, each its op and body in hexadecimal,
# to LOG.last when the helper ends.
import array, collections, random, socket, struct, subprocess, sys

helper, grammars, rounds, seed, log = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4]), sys.argv[5]
rnd = random.Random(seed)

OP_GRAMMAR, OP_PARSER, OP_PARSER_DELETE, OP_PARSE, OP_CANCEL = 1, 2, 3, 4, 5
OP_COPY, OP_TREE_DELETE, OP_QUERY, OP_QUERY_DELETE, OP_DISABLE, OP_MATCHES = 7, 8, 9, 10, 11, 12
OP_ANCESTRY, OP_SELECT, OP_CHILDREN, OP_HAS_ERROR, OP_RESET, OP_MEMORY, OP_CONNECT = 13, 14, 15, 16, 17, 18, 19
OP_PARSE_RANGES, OP_MATCHES_MANY, OP_CHILDREN_MANY, OP_EDITS, OP_READ = 20, 21, 22, 23, 64
KNOWN = [OP_GRAMMAR, OP_PARSER, OP_PARSER_DELETE, OP_PARSE, OP_COPY, OP_TREE_DELETE, OP_QUERY, OP_QUERY_DELETE,
         OP_DISABLE, OP_MATCHES, OP_ANCESTRY, OP_SELECT, OP_CHILDREN, OP_HAS_ERROR, OP_RESET, OP_MEMORY,
         OP_PARSE_RANGES, OP_MATCHES_MANY, OP_CHILDREN_MANY, OP_EDITS]

# languages, with rules for them that capture much and test text
LANGUAGES = {
    'json': b'(pair key: (_) @key) (string) @string (number) @number',
    'rust': b'(identifier) @name ((identifier) @c (#match? @c "^[A-Z]")) (function_item name: (_) @f)',
    'commonlisp': b'(sym_lit) @symbol (str_lit) @string ((sym_lit) @k (#any-of? @k "defun" "let"))',
    'bash': b'(word) @word (heredoc_body) @here (string) @string',
    'python': b'(identifier) @name (string) @string ((identifier) @self (#eq? @self "self"))',
}
TEXTS = [
    b'{"a": [1, 2, {"b": null}], "c": "x\\u00e9"}\n' * 40,
    b'fn main() { let x = vec![1, 2]; println!("{}", x[0]); }\n' * 30,
    b'(defun f (x) "doc" (let ((y 1)) (+ x y)))\n' * 30,
    b'cat <<EOF\nline $x\n  EOF\nEOF\necho "a $(b)"\n' * 20,
    b'class A:\n    def f(self):\n        return """doc\n"""\n' * 20,
    b'[' * 2000 + b']' * 1990,
    b'"' * 500 + b'\\' * 300,
    bytes(rnd.randrange(256) for _ in range(3000)),
    b'\xff\xfe\x00' * 200,
    b'',
]

u = lambda *xs: struct.pack('<%dI' % len(xs), *[x & 0xffffffff for x in xs])
padded = lambda b: b + b'\0' * (-len(b) % 4)


class Ended(Exception):
    """The helper closed a connection."""


first, theirs = socket.socketpair()
proc = subprocess.Popen([helper, grammars], stdin=theirs, stderr=open(log, 'ab'))
theirs.close()


def connect():
    """A new connection to the helper, handed over the first."""
    mine, other = socket.socketpair()
    first.sendmsg([u(0, 0, OP_CONNECT)], [(socket.SOL_SOCKET, socket.SCM_RIGHTS, array.array('i', [other.fileno()]))])
    other.close()
    mine.settimeout(120)
    return mine


conn = connect()
text = TEXTS[0]
serial = [1]
sent = collections.deque(maxlen=40)
stats = {'answered': 0, 'refused': 0, 'closed': 0}


def received(n):
    data = b''
    while len(data) < n:
        chunk = conn.recv(n - len(data))
        if not chunk:
            raise Ended()
        data += chunk
    return data


def request(op, body):
    """Send a request, giving the text a parse asks for, and return its status and reply."""
    serial[0] += 1
    number = serial[0]
    sent.append((op, body))
    conn.sendall(u(len(body), number, op) + body)
    while True:
        size, answering, status = struct.unpack('<III', received(12))
        data = received(size)
        if status == OP_READ and answering & 0x80000000:
            parser, octet = struct.unpack('<II', data[:8]) if len(data) >= 8 else (0, 0)
            chunk = text[octet:octet + rnd.choice([1, 7, 4096])] if octet < len(text) else b''
            conn.sendall(u(len(chunk), answering, OP_READ) + chunk)
            continue
        if answering != number:
            print('reply %d to request %d (op %d)' % (answering, number, op))
            sys.exit(1)
        stats['refused' if status else 'answered'] += 1
        return status, data


parsers, trees, queries = {}, [], {}


def word():
    return rnd.choice([0, 1, 2, rnd.randrange(64), rnd.randrange(len(text) + 10), 0xffffffff, 0x7fffffff,
                       rnd.randrange(1 << 32), rnd.choice(trees) if trees else 0])


def a(seq, default=0):
    return rnd.choice(seq) if seq else default


def made_request():
    """A request of some op, made from what the helper holds, its body perhaps spoilt."""
    global text
    name = rnd.choice(list(LANGUAGES))
    tree, query = a(trees), a(list(queries.values()))
    parser = parsers.get(name, 0)
    n = len(text)
    op = rnd.choice(KNOWN + [0, 6, 24, 63, 65, rnd.randrange(1 << 32)])
    if op in (OP_GRAMMAR, OP_PARSER):
        body = rnd.choice([name.encode(), b'../x', b'a' * 80, b'', b'rust\0'])
    elif op in (OP_PARSER_DELETE, OP_RESET):
        body = u(a([parser, word()]))
    elif op == OP_PARSE:
        text = rnd.choice(TEXTS)
        body = u(parser, a([tree, 0, word()]), rnd.choice([0, 1, 2000])) + struct.pack('<Q', rnd.choice([0, 1 << 20, 1 << 40]))
    elif op in (OP_COPY, OP_TREE_DELETE, OP_HAS_ERROR):
        body = u(a([tree, word()]))
    elif op == OP_QUERY:
        source = rnd.choice([LANGUAGES[name], b'(((', b'(identifier) @x (#match? @x "[")', bytes(rnd.randrange(256) for _ in range(40))])
        body = u(rnd.randrange(1, 40), len(name)) + padded(name.encode()) + source
    elif op == OP_QUERY_DELETE:
        body = u(a([query, word()]))
    elif op == OP_DISABLE:
        body = u(query, word())
    elif op == OP_MATCHES:
        body = u(query, tree, word(), word())
    elif op in (OP_ANCESTRY,):
        body = u(tree, word(), word())
    elif op == OP_SELECT:
        body = u(tree, word(), word(), rnd.randrange(6))
    elif op == OP_CHILDREN:
        body = u(tree, 0, n, word(), rnd.randrange(3), word(), word())
    elif op == OP_MEMORY:
        body = b''
    elif op == OP_MATCHES_MANY:
        k = rnd.choice([0, 1, 3, 64, 65, 1 << 31])
        body = u(k) + b''.join(u(query, a([tree, word()]), word(), word()) for _ in range(min(k, 70)))
    elif op == OP_CHILDREN_MANY:
        k = rnd.choice([0, 1, 5, 4096, 1 << 31])
        body = u(k) + b''.join(u(tree, 0, n, word(), 1, word(), word()) for _ in range(min(k, 20)))
    elif op == OP_PARSE_RANGES:
        k = rnd.choice([1, 2, 3, 257])
        parts = []
        for _ in range(min(k, 4)):
            if trees and rnd.random() < 0.3:
                parts.append(u(tree))
            else:
                count = rnd.choice([0, 1, 2, 5000])
                ranges = []
                at = 0
                for _ in range(min(count, 3)):
                    s = at + rnd.randrange(20); e = s + rnd.randrange(60); at = e
                    ranges.append(u(s, e, 0, s, 0, e) if rnd.random() < 0.8 else u(*[word() for _ in range(6)]))
                parts.append(u(0, len(name)) + padded(name.encode()) + u(count) + b''.join(ranges)
                             + u(rnd.choice([0, 500])) + struct.pack('<Q', rnd.choice([0, 1 << 30])))
            asked = rnd.choice([0, 1, 2, 65])
            parts.append(u(asked) + b''.join(u(query, word(), word()) for _ in range(min(asked, 3))))
        given = text[:rnd.randrange(len(text) + 1)]
        body = u(k) + b''.join(parts) + u(rnd.choice([0, word()]), rnd.choice([len(given), word()])) + given
    elif op == OP_EDITS:
        k = rnd.choice([0, 1, 2, 50, rnd.randrange(1 << 32)])
        records = []
        for _ in range(min(k, 60)):
            if rnd.random() < 0.6:
                s = rnd.randrange(n + 1); oe = rnd.randrange(s, n + 1); ne = s + rnd.randrange(20)
                records.append(u(s, oe, ne, 0, s, 0, oe, 0, ne))
            else:
                records.append(u(*[word() for _ in range(9)]))
        body = u(a([tree, word()]), rnd.choice([0, 1, 2]), k) + b''.join(records)
    else:
        body = bytes(rnd.randrange(256) for _ in range(rnd.randrange(64)))
    # spoilt: cut short, a number changed, or more after it
    roll = rnd.random()
    if roll < 0.15:
        body = body[:rnd.randrange(len(body) + 1)]
    elif roll < 0.3 and len(body) >= 4:
        at = rnd.randrange(len(body) // 4) * 4
        body = body[:at] + u(word()) + body[at + 4:]
    elif roll < 0.35:
        body += bytes(rnd.randrange(256) for _ in range(rnd.randrange(100)))
    return op, body


def take(op, status, data):
    """Keep what a reply made: a parser, a tree, a query."""
    if status != 0:
        return
    if op == OP_PARSER and len(data) == 4:
        parsers[rnd.choice(list(LANGUAGES))] = struct.unpack('<I', data)[0]
    elif op in (OP_PARSE, OP_COPY, OP_EDITS) and len(data) >= 4:
        number = struct.unpack('<I', data[:4])[0]
        if number:
            trees.append(number)


# well-made first: a parser and rules for each language
for name, source in LANGUAGES.items():
    status, data = request(OP_PARSER, name.encode())
    if status == 0:
        parsers[name] = struct.unpack('<I', data)[0]
    number = len(queries) + 1
    status, data = request(OP_QUERY, u(number, len(name)) + padded(name.encode()) + source)
    if status == 0 and data[:4] == u(0):
        queries[name] = number

for _ in range(rounds):
    try:
        op, body = made_request()
        take(op, *request(op, body))
        if rnd.random() < 0.05 and parsers:
            # a parse stopped over the first connection, which has no reply
            first.sendall(u(4, 0, OP_CANCEL) + u(rnd.choice(list(parsers.values()) + [0, 0xffffffff])))
        while len(trees) > 40:
            request(OP_TREE_DELETE, u(trees.pop(0)))
    except (Ended, ConnectionError, socket.timeout):
        if proc.poll() is not None:
            break
        stats['closed'] += 1
        conn.close()
        conn = connect()

alive = proc.poll() is None
if not alive:
    with open(log + '.last', 'w') as last:
        for op, body in sent:
            last.write('%d %s\n' % (op, body.hex()))
print('rounds %d, seed %d: %d answered, %d refused, %d connections closed by the helper; helper %s'
      % (rounds, seed, stats['answered'], stats['refused'], stats['closed'],
         'alive' if alive else 'ENDED (exit %s)' % proc.returncode))
first.close()
conn.close()
try:
    proc.wait(timeout=60)
except subprocess.TimeoutExpired:
    proc.kill()
sys.exit(0 if alive else 1)
