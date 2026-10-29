/* lisp/terminal/shim.c --- What the terminal library takes in a way Lisp cannot pass.
 * Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
 *
 * The library's formatter takes its options, and a mouse event its position,
 * as structures passed by value, which SBCL's foreign calls cannot make;
 * these functions make them in C.
 * Built by tools/terminal-library.sh into libtuatara-terminal.so beside the
 * library, loaded after it. */
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <ghostty/vt.h>

/* The whole of TERMINAL's screen, its scrollback first, a line for each row
 * (soft-wrapped rows not joined), the spaces at their ends left out: with
 * STYLED its colours and looks as SGR escape sequences, else plain text.
 * Into *OUT, *LENGTH octets the library allocated (free with
 * tuatara_terminal_text_free). Returns the library's result. */
int tuatara_terminal_text(GhosttyTerminal terminal, int styled, uint8_t **out, size_t *length) {
  GhosttyFormatterTerminalOptions options = {0};
  options.size = sizeof options;
  options.emit = styled ? GHOSTTY_FORMATTER_FORMAT_VT : GHOSTTY_FORMATTER_FORMAT_PLAIN;
  options.unwrap = false;
  options.trim = true;
  options.extra.size = sizeof options.extra;
  options.extra.screen.size = sizeof options.extra.screen;
  options.selection = NULL;
  GhosttyFormatter formatter = NULL;
  GhosttyResult result = ghostty_formatter_terminal_new(NULL, &formatter, terminal, options);
  if (result != GHOSTTY_SUCCESS) return result;
  result = ghostty_formatter_format_alloc(formatter, NULL, out, length);
  ghostty_formatter_free(formatter);
  return result;
}

/* Free what tuatara_terminal_text made. */
void tuatara_terminal_text_free(uint8_t *text, size_t length) {
  ghostty_free(NULL, text, length);
}

/* What the mouse did at cell COLUMN, ROW of a screen COLUMNS by ROWS, as the
 * program asks to hear of it (its mouse tracking and format, as ENCODER was
 * last set from the terminal with ghostty_mouse_encoder_setopt_from_terminal):
 * ACTION a GhosttyMouseAction, BUTTON a GhosttyMouseButton (0 for none, a
 * motion), MODS GhosttyMods, PRESSED true while a button is held. The
 * library takes positions in pixels: cells are given as 10 by 20 pixels, the
 * middle of the cell meant. Into OUT, at most SIZE octets, *LENGTH of them,
 * none when the program does not track the mouse; ENCODER and EVENT are the
 * caller's, reused. Returns the library's result. */
int tuatara_terminal_mouse(GhosttyMouseEncoder encoder, GhosttyMouseEvent event,
                           int action, int button, int mods, int column, int row, int columns, int rows,
                           int pressed, char *out, size_t size, size_t *length) {
  GhosttyMouseEncoderSize screen = {0};
  screen.size = sizeof screen;
  screen.cell_width = 10;
  screen.cell_height = 20;
  screen.screen_width = (uint32_t)columns * 10;
  screen.screen_height = (uint32_t)rows * 20;
  bool held = pressed != 0;
  ghostty_mouse_encoder_setopt(encoder, GHOSTTY_MOUSE_ENCODER_OPT_SIZE, &screen);
  ghostty_mouse_encoder_setopt(encoder, GHOSTTY_MOUSE_ENCODER_OPT_ANY_BUTTON_PRESSED, &held);
  ghostty_mouse_event_set_action(event, (GhosttyMouseAction)action);
  if (button) ghostty_mouse_event_set_button(event, (GhosttyMouseButton)button);
  else ghostty_mouse_event_clear_button(event);
  ghostty_mouse_event_set_mods(event, (GhosttyMods)mods);
  GhosttyMousePosition position = { column * 10.0f + 5.0f, row * 20.0f + 10.0f };
  ghostty_mouse_event_set_position(event, position);
  *length = 0;
  return ghostty_mouse_encoder_encode(encoder, event, out, size, length);
}

/* The address of the link (OSC 8) on the cell at COLUMN, ROW of TERMINAL:
 * of its screen as it shows (SCREEN false), or counted from the first row of
 * its scrollback (SCREEN true). Into OUT, at most SIZE octets, *LENGTH of
 * them, none when the cell has no link. Returns the library's result:
 * GHOSTTY_OUT_OF_SPACE with the size needed in *LENGTH when OUT is too
 * small. */
int tuatara_terminal_link(GhosttyTerminal terminal, int screen, int column, int row,
                          uint8_t *out, size_t size, size_t *length) {
  GhosttyPoint point = {0};
  point.tag = screen ? GHOSTTY_POINT_TAG_SCREEN : GHOSTTY_POINT_TAG_VIEWPORT;
  point.value.coordinate.x = (uint16_t)column;
  point.value.coordinate.y = (uint32_t)row;
  GhosttyGridRef ref = {0};
  ref.size = sizeof ref;
  *length = 0;
  GhosttyResult result = ghostty_terminal_grid_ref(terminal, point, &ref);
  if (result != GHOSTTY_SUCCESS) return result;
  return ghostty_grid_ref_hyperlink_uri(&ref, out, size, length);
}
