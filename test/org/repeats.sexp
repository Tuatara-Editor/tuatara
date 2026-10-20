;; Org tasks marked done with C-c C-t, and what Org 9.7.11 (Emacs 30.2) makes of them at 2026-10-01 18:00, with org-log-done time, org-log-into-drawer t: (NAME LINE TEXT RESULT), LINE the headline's.
("plus" 0 "* TODO Water plants
SCHEDULED: <2026-09-28 Mon +1w>
" "* TODO Water plants
SCHEDULED: <2026-10-05 Mon +1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("catchup" 0 "* TODO Water plants
SCHEDULED: <2026-09-01 Tue ++1w>
" "* TODO Water plants
SCHEDULED: <2026-10-06 Tue ++1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("restart" 0 "* TODO Water plants
SCHEDULED: <2026-09-01 Tue 09:00 .+2d>
" "* TODO Water plants
SCHEDULED: <2026-10-03 Sat 09:00 .+2d>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("deadline-warn" 0 "* TODO Pay
DEADLINE: <2026-09-30 Wed +1m -3d>
" "* TODO Pay
DEADLINE: <2026-10-30 Fri +1m -3d>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("month-end" 0 "* TODO Rent
SCHEDULED: <2026-01-31 Sat +1m>
" "* TODO Rent
SCHEDULED: <2026-03-03 Tue +1m>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("year" 0 "* TODO Birthday
SCHEDULED: <2024-02-29 Thu +1y>
" "* TODO Birthday
SCHEDULED: <2025-03-01 Sat +1y>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("both" 0 "* TODO Report
DEADLINE: <2026-10-02 Fri +1w> SCHEDULED: <2026-09-30 Wed +1w>
Body <2026-09-29 Tue +1w>
" "* TODO Report
DEADLINE: <2026-10-09 Fri +1w> SCHEDULED: <2026-10-07 Wed +1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
Body <2026-10-06 Tue +1w>
")
("hours" 0 "* TODO Pill
SCHEDULED: <2026-10-01 Thu 08:00 +8h>
" "* TODO Pill
SCHEDULED: <2026-10-01 Thu 16:00 +8h>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
("time-range" 0 "* TODO Meet
<2026-09-24 Thu 10:00-11:30 +1w>
" "* TODO Meet
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
<2026-10-01 Thu 10:00-11:30 +1w>
")
("custom-seq" 1 "#+TODO: NEXT WAIT | DONE CANCELLED
* WAIT Thing
SCHEDULED: <2026-09-28 Mon +1d>
" "#+TODO: NEXT WAIT | DONE CANCELLED
* NEXT Thing
SCHEDULED: <2026-09-29 Tue +1d>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"WAIT\"       [2026-10-01 Thu 18:00]
:END:
")
("existing-props" 0 "* TODO Water
SCHEDULED: <2026-09-28 Mon +1w>
:PROPERTIES:
:ID: abc
:LAST_REPEAT: [2026-09-21 Mon 10:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-09-21 Mon 10:00]
:END:
Body
" "* TODO Water
SCHEDULED: <2026-10-05 Mon +1w>
:PROPERTIES:
:ID: abc
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
- State \"DONE\"       from \"TODO\"       [2026-09-21 Mon 10:00]
:END:
Body
")
("clock-logbook" 0 "* TODO Water
SCHEDULED: <2026-09-28 Mon +1w>
:LOGBOOK:
CLOCK: [2026-09-30 Wed 10:00]--[2026-09-30 Wed 11:00] =>  1:00
:END:
" "* TODO Water
SCHEDULED: <2026-10-05 Mon +1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
CLOCK: [2026-09-30 Wed 10:00]--[2026-09-30 Wed 11:00] =>  1:00
:END:
")
("subtree" 0 "* TODO Parent
SCHEDULED: <2026-09-28 Mon +1w>
** TODO Child
SCHEDULED: <2026-09-28 Mon +1w>
" "* TODO Parent
SCHEDULED: <2026-10-05 Mon +1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
** TODO Child
SCHEDULED: <2026-09-28 Mon +1w>
")
("no-repeat" 0 "* TODO Once
SCHEDULED: <2026-09-28 Mon>
" "* DONE Once
CLOSED: [2026-10-01 Thu 18:00] SCHEDULED: <2026-09-28 Mon>
")
("no-repeat-body" 0 "* TODO Once
Body
" "* DONE Once
CLOSED: [2026-10-01 Thu 18:00]
Body
")
("last-line" 0 "* TODO Water
SCHEDULED: <2026-09-28 Mon +1w>" "* TODO Water
SCHEDULED: <2026-10-05 Mon +1w>
:PROPERTIES:
:LAST_REPEAT: [2026-10-01 Thu 18:00]
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]
:END:
")
