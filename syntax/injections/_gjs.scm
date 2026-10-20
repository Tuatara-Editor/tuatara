; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in _gjs: runtime/queries/_gjs/injections.scm of
; Helix (https://github.com/YardQuit/helix_editor) at commit 9bcdf4d79d14,
; unchanged below this notice.

; PARSE GLIMMER TEMPLATES
(call_expression
  function: [
    (identifier) @injection.language
    (member_expression
      property: (property_identifier) @injection.language)
  ]
  arguments: (template_string) @injection.content)

; e.g.: <template><SomeComponent @arg={{double @value}} /></template>
((glimmer_template) @injection.content
 (#set! injection.language "glimmer")
 (#set! injection.include-children))

; Parse Ember/Glimmer/Handlebars/HTMLBars/etc. template literals
; e.g.: await render(hbs`<SomeComponent />`)
((call_expression
   function: ((identifier) @_name
              (#eq? @_name "hbs"))
   arguments: (template_string) @injection.content)
 (#set! injection.language "glimmer"))
