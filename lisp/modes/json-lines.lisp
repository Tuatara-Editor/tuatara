;;;; lisp/modes/json-lines.lisp --- JSON Lines: a JSON value on each line, a record each.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(define-major-mode jsonl-mode json-mode
  "JSON Lines (NDJSON): a JSON value on each line, as logs and exports write
their records, coloured as JSON. Each line is a record, so search filters
\(\\[search-filter-regex]) narrow the file by hiding the lines they leave out."
  :lighter "JSON Lines"
  :extensions ("jsonl" "ndjson"))
