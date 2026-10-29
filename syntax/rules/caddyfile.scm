; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for caddyfile: runtime/queries/caddyfile/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice.

(comment) @comment
[
  (environment_variable)
  (placeholder)
] @constant

[
  (network_address)
  (ip_address_or_cidr)
] @string.special.url

(path) @string.special.path

[
  (snippet_name)
  (named_route_identifier)
  (site_address)
] @keyword

(directive (directive_name) @variable.other.member)

; declaration of a named matcher
(named_matcher (matcher_identifier (matcher_name)) @function.macro)

; reference to a named matcher
(matcher (matcher_identifier (matcher_name)) @function.macro)

; directive within a named matcher declaration
(matcher_directive (matcher_directive_name) @function.method)

; any other matcher (wildcard and path)
(matcher) @function.macro

[
  (interpreted_string_literal)
  (raw_string_literal)
  (heredoc)
  (cel_expression)
] @string
(escape_sequence) @constant.character.escape

[
  (duration_literal)
  (int_literal)
  (status_code_fallback)
] @constant.numeric

[
  "{"
  "}"
] @punctuation.bracket

(global_options
  (directive) @keyword.directive)

(directive
  name: (directive_name)
  (argument) @type)

; matches directive arguments that looks like an absolute path
; e.g.
; log {
;     output file /var/log/caddy.log
; }
(directive
  (argument) @string.special.path
  (#match? @string.special.path "^/"))

((argument) @constant.builtin.boolean
  (#any-of? @constant.builtin.boolean "on" "off"))

((argument) @type.enum.variant
  (#any-of? @type.enum.variant "tcp" "udp" "ipv4" "ipv6"))
