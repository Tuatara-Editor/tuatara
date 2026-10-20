/* lisp/terminal/layout.c --- What the terminal library's C headers say, for Lisp.
 * Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
 *
 * Built and run by tools/terminal-library.sh against the library's own headers;
 * it prints, as a Lisp property list read by lisp/terminal/emulator.lisp, the
 * values of the enumerations and the sizes and offsets of the structures the
 * emulator uses, so that no number is copied by hand from the headers. */
#include <stdio.h>
#include <stddef.h>
#include <ghostty/vt.h>

#define V(name, value) printf(" :%s %ld\n", name, (long)(value))
#define OFF(name, type, field) printf(" :%s %ld\n", name, (long)offsetof(type, field))

int main(void) {
  printf("(\n");
  V("success", GHOSTTY_SUCCESS);
  V("no-value", GHOSTTY_NO_VALUE);
  V("opt-userdata", GHOSTTY_TERMINAL_OPT_USERDATA);
  V("opt-write-pty", GHOSTTY_TERMINAL_OPT_WRITE_PTY);
  V("opt-scrollback-max-bytes", GHOSTTY_TERMINAL_OPT_SCROLLBACK_MAX_BYTES);
  V("opt-scrollback-max-lines", GHOSTTY_TERMINAL_OPT_SCROLLBACK_MAX_LINES);
  V("data-title", GHOSTTY_TERMINAL_DATA_TITLE);
  V("data-pwd", GHOSTTY_TERMINAL_DATA_PWD);
  V("data-scrollback-rows", GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS);
  V("data-total-rows", GHOSTTY_TERMINAL_DATA_TOTAL_ROWS);
  V("data-mode", GHOSTTY_TERMINAL_DATA_MODE);
  V("mode-config-size", sizeof(GhosttyTerminalModeConfig));
  OFF("mode-config-mode", GhosttyTerminalModeConfig, mode);
  OFF("mode-config-value", GhosttyTerminalModeConfig, value);
  V("mode-bracketed-paste", GHOSTTY_MODE_BRACKETED_PASTE);
  V("out-of-space", GHOSTTY_OUT_OF_SPACE);
  V("mouse-press", GHOSTTY_MOUSE_ACTION_PRESS);
  V("mouse-release", GHOSTTY_MOUSE_ACTION_RELEASE);
  V("mouse-motion", GHOSTTY_MOUSE_ACTION_MOTION);
  V("mouse-left", GHOSTTY_MOUSE_BUTTON_LEFT);
  V("mouse-middle", GHOSTTY_MOUSE_BUTTON_MIDDLE);
  V("mouse-wheel-up", GHOSTTY_MOUSE_BUTTON_FOUR);
  V("mouse-wheel-down", GHOSTTY_MOUSE_BUTTON_FIVE);
  V("data-active-screen", GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN);
  V("screen-alternate", GHOSTTY_TERMINAL_SCREEN_ALTERNATE);
  V("mode-alternate-scroll", GHOSTTY_MODE_ALT_SCROLL);
  V("opt-kitty-storage-limit", GHOSTTY_TERMINAL_OPT_KITTY_IMAGE_STORAGE_LIMIT);
  V("data-kitty-storage-limit", GHOSTTY_TERMINAL_DATA_KITTY_IMAGE_STORAGE_LIMIT);
  V("opt-kitty-medium-file", GHOSTTY_TERMINAL_OPT_KITTY_IMAGE_MEDIUM_FILE);
  V("opt-kitty-medium-temp-file", GHOSTTY_TERMINAL_OPT_KITTY_IMAGE_MEDIUM_TEMP_FILE);
  V("opt-kitty-medium-shared-mem", GHOSTTY_TERMINAL_OPT_KITTY_IMAGE_MEDIUM_SHARED_MEM);
  V("data-kitty-graphics", GHOSTTY_TERMINAL_DATA_KITTY_GRAPHICS);
  V("graphics-placement-iterator", GHOSTTY_KITTY_GRAPHICS_DATA_PLACEMENT_ITERATOR);
  V("graphics-generation", GHOSTTY_KITTY_GRAPHICS_DATA_GENERATION);
  V("placement-image-id", GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_IMAGE_ID);
  V("placement-z", GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_Z);
  V("placement-virtual", GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_IS_VIRTUAL);
  V("placement-columns", GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_COLUMNS);
  V("placement-rows", GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_ROWS);
  V("image-width", GHOSTTY_KITTY_IMAGE_DATA_WIDTH);
  V("image-height", GHOSTTY_KITTY_IMAGE_DATA_HEIGHT);
  V("image-format", GHOSTTY_KITTY_IMAGE_DATA_FORMAT);
  V("image-data-pointer", GHOSTTY_KITTY_IMAGE_DATA_DATA_PTR);
  V("image-data-length", GHOSTTY_KITTY_IMAGE_DATA_DATA_LEN);
  V("image-generation", GHOSTTY_KITTY_IMAGE_DATA_GENERATION);
  V("image-format-rgb", GHOSTTY_KITTY_IMAGE_FORMAT_RGB);
  V("image-format-rgba", GHOSTTY_KITTY_IMAGE_FORMAT_RGBA);
  V("image-format-gray-alpha", GHOSTTY_KITTY_IMAGE_FORMAT_GRAY_ALPHA);
  V("image-format-gray", GHOSTTY_KITTY_IMAGE_FORMAT_GRAY);
  V("render-info-size", sizeof(GhosttyKittyGraphicsPlacementRenderInfo));
  OFF("render-info-columns", GhosttyKittyGraphicsPlacementRenderInfo, grid_cols);
  OFF("render-info-rows", GhosttyKittyGraphicsPlacementRenderInfo, grid_rows);
  OFF("render-info-column", GhosttyKittyGraphicsPlacementRenderInfo, viewport_col);
  OFF("render-info-row", GhosttyKittyGraphicsPlacementRenderInfo, viewport_row);
  OFF("render-info-visible", GhosttyKittyGraphicsPlacementRenderInfo, viewport_visible);
  OFF("render-info-source-x", GhosttyKittyGraphicsPlacementRenderInfo, source_x);
  OFF("render-info-source-y", GhosttyKittyGraphicsPlacementRenderInfo, source_y);
  OFF("render-info-source-width", GhosttyKittyGraphicsPlacementRenderInfo, source_width);
  OFF("render-info-source-height", GhosttyKittyGraphicsPlacementRenderInfo, source_height);
  V("sys-decode-png", GHOSTTY_SYS_OPT_DECODE_PNG);
  OFF("sys-image-width", GhosttySysImage, width);
  OFF("sys-image-height", GhosttySysImage, height);
  OFF("sys-image-data", GhosttySysImage, data);
  OFF("sys-image-length", GhosttySysImage, data_len);
  V("compress-incremental", GHOSTTY_TERMINAL_COMPRESSION_MODE_INCREMENTAL);
  V("compress-pending", GHOSTTY_TERMINAL_COMPRESSION_RESULT_PENDING);
  V("compress-complete", GHOSTTY_TERMINAL_COMPRESSION_RESULT_COMPLETE);
  V("data-memory-usage", GHOSTTY_TERMINAL_DATA_MEMORY_USAGE);
  V("memory-size", sizeof(GhosttyTerminalMemoryUsage));
  OFF("memory-compression-supported", GhosttyTerminalMemoryUsage, compression_supported);
  OFF("memory-primary-resident", GhosttyTerminalMemoryUsage, primary_resident_bytes);
  OFF("memory-primary-compressed-pages", GhosttyTerminalMemoryUsage, primary_compressed_pages);
  OFF("memory-primary-pages", GhosttyTerminalMemoryUsage, primary_pages);
  OFF("memory-alternate-resident", GhosttyTerminalMemoryUsage, alternate_resident_bytes);
  V("rs-dirty", GHOSTTY_RENDER_STATE_DATA_DIRTY);
  V("rs-row-iterator", GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR);
  V("rs-cursor", GHOSTTY_RENDER_STATE_DATA_CURSOR);
  V("rs-colors", GHOSTTY_RENDER_STATE_DATA_COLORS);
  V("row-cells", GHOSTTY_RENDER_STATE_ROW_DATA_CELLS);
  V("cell-style", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE);
  V("cell-graphemes-len", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_LEN);
  V("cell-graphemes-buf", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_BUF);
  V("cell-bg-color", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_BG_COLOR);
  V("cell-fg-color", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_FG_COLOR);
  V("cell-has-styling", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_HAS_STYLING);
  V("cell-raw", GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW);
  V("raw-wide", GHOSTTY_CELL_DATA_WIDE);
  V("raw-has-link", GHOSTTY_CELL_DATA_HAS_HYPERLINK);
  V("row-raw", GHOSTTY_RENDER_STATE_ROW_DATA_RAW);
  V("raw-row-link", GHOSTTY_ROW_DATA_HYPERLINK);
  V("wide-spacer-tail", GHOSTTY_CELL_WIDE_SPACER_TAIL);
  V("wide-spacer-head", GHOSTTY_CELL_WIDE_SPACER_HEAD);
  V("string-size", sizeof(GhosttyString));
  OFF("string-pointer", GhosttyString, ptr);
  OFF("string-length", GhosttyString, len);
  V("cursor-bar", GHOSTTY_RENDER_STATE_CURSOR_VISUAL_STYLE_BAR);
  V("cursor-block", GHOSTTY_RENDER_STATE_CURSOR_VISUAL_STYLE_BLOCK);
  V("cursor-underline", GHOSTTY_RENDER_STATE_CURSOR_VISUAL_STYLE_UNDERLINE);
  V("cursor-hollow", GHOSTTY_RENDER_STATE_CURSOR_VISUAL_STYLE_BLOCK_HOLLOW);
  V("style-size", sizeof(GhosttyStyle));
  OFF("style-bold", GhosttyStyle, bold);
  OFF("style-italic", GhosttyStyle, italic);
  OFF("style-faint", GhosttyStyle, faint);
  OFF("style-inverse", GhosttyStyle, inverse);
  OFF("style-invisible", GhosttyStyle, invisible);
  OFF("style-strikethrough", GhosttyStyle, strikethrough);
  OFF("style-underline", GhosttyStyle, underline);
  V("cursor-size", sizeof(GhosttyRenderStateCursor));
  OFF("cursor-has-value", GhosttyRenderStateCursor, viewport_has_value);
  OFF("cursor-x", GhosttyRenderStateCursor, viewport_x);
  OFF("cursor-y", GhosttyRenderStateCursor, viewport_y);
  OFF("cursor-visible", GhosttyRenderStateCursor, visible);
  OFF("cursor-style", GhosttyRenderStateCursor, visual_style);
  V("colors-size", sizeof(GhosttyRenderStateColors));
  OFF("colors-background", GhosttyRenderStateColors, background);
  OFF("colors-foreground", GhosttyRenderStateColors, foreground);
  V("rgb-size", sizeof(GhosttyColorRgb));
  OFF("style-fg", GhosttyStyle, fg_color);
  OFF("style-bg", GhosttyStyle, bg_color);
  OFF("color-tag", GhosttyStyleColor, tag);
  OFF("color-value", GhosttyStyleColor, value);
  V("color-palette", GHOSTTY_STYLE_COLOR_PALETTE);
  V("color-rgb", GHOSTTY_STYLE_COLOR_RGB);
  V("key-press", GHOSTTY_KEY_ACTION_PRESS);
  V("mods-shift", GHOSTTY_MODS_SHIFT);
  V("mods-ctrl", GHOSTTY_MODS_CTRL);
  V("mods-alt", GHOSTTY_MODS_ALT);
  /* the physical keys, by the names Tuatara gives keys: a character, or a key's keyword */
  printf(" :keys (\n");
  {
    static const struct { const char *name; long value; } keys[] = {
#define K(name, key) { name, key }
      K("a", GHOSTTY_KEY_A), K("b", GHOSTTY_KEY_B), K("c", GHOSTTY_KEY_C), K("d", GHOSTTY_KEY_D),
      K("e", GHOSTTY_KEY_E), K("f", GHOSTTY_KEY_F), K("g", GHOSTTY_KEY_G), K("h", GHOSTTY_KEY_H),
      K("i", GHOSTTY_KEY_I), K("j", GHOSTTY_KEY_J), K("k", GHOSTTY_KEY_K), K("l", GHOSTTY_KEY_L),
      K("m", GHOSTTY_KEY_M), K("n", GHOSTTY_KEY_N), K("o", GHOSTTY_KEY_O), K("p", GHOSTTY_KEY_P),
      K("q", GHOSTTY_KEY_Q), K("r", GHOSTTY_KEY_R), K("s", GHOSTTY_KEY_S), K("t", GHOSTTY_KEY_T),
      K("u", GHOSTTY_KEY_U), K("v", GHOSTTY_KEY_V), K("w", GHOSTTY_KEY_W), K("x", GHOSTTY_KEY_X),
      K("y", GHOSTTY_KEY_Y), K("z", GHOSTTY_KEY_Z),
      K("0", GHOSTTY_KEY_DIGIT_0), K("1", GHOSTTY_KEY_DIGIT_1), K("2", GHOSTTY_KEY_DIGIT_2), K("3", GHOSTTY_KEY_DIGIT_3),
      K("4", GHOSTTY_KEY_DIGIT_4), K("5", GHOSTTY_KEY_DIGIT_5), K("6", GHOSTTY_KEY_DIGIT_6), K("7", GHOSTTY_KEY_DIGIT_7),
      K("8", GHOSTTY_KEY_DIGIT_8), K("9", GHOSTTY_KEY_DIGIT_9),
      K("`", GHOSTTY_KEY_BACKQUOTE), K("\\", GHOSTTY_KEY_BACKSLASH), K("[", GHOSTTY_KEY_BRACKET_LEFT),
      K("]", GHOSTTY_KEY_BRACKET_RIGHT), K(",", GHOSTTY_KEY_COMMA), K("=", GHOSTTY_KEY_EQUAL), K("-", GHOSTTY_KEY_MINUS),
      K(".", GHOSTTY_KEY_PERIOD), K("'", GHOSTTY_KEY_QUOTE), K(";", GHOSTTY_KEY_SEMICOLON), K("/", GHOSTTY_KEY_SLASH),
      K(" ", GHOSTTY_KEY_SPACE),
      K(":backspace", GHOSTTY_KEY_BACKSPACE), K(":enter", GHOSTTY_KEY_ENTER), K(":tab", GHOSTTY_KEY_TAB),
      K(":escape", GHOSTTY_KEY_ESCAPE), K(":delete", GHOSTTY_KEY_DELETE), K(":insert", GHOSTTY_KEY_INSERT),
      K(":home", GHOSTTY_KEY_HOME), K(":end", GHOSTTY_KEY_END), K(":page-up", GHOSTTY_KEY_PAGE_UP),
      K(":page-down", GHOSTTY_KEY_PAGE_DOWN), K(":up", GHOSTTY_KEY_ARROW_UP), K(":down", GHOSTTY_KEY_ARROW_DOWN),
      K(":left", GHOSTTY_KEY_ARROW_LEFT), K(":right", GHOSTTY_KEY_ARROW_RIGHT),
      K(":f1", GHOSTTY_KEY_F1), K(":f2", GHOSTTY_KEY_F2), K(":f3", GHOSTTY_KEY_F3), K(":f4", GHOSTTY_KEY_F4),
      K(":f5", GHOSTTY_KEY_F5), K(":f6", GHOSTTY_KEY_F6), K(":f7", GHOSTTY_KEY_F7), K(":f8", GHOSTTY_KEY_F8),
      K(":f9", GHOSTTY_KEY_F9), K(":f10", GHOSTTY_KEY_F10), K(":f11", GHOSTTY_KEY_F11), K(":f12", GHOSTTY_KEY_F12),
#undef K
    };
    for (size_t i = 0; i < sizeof keys / sizeof keys[0]; i++) {
      putchar(' '); putchar(' '); putchar('(');
      putchar('"');
      for (const char *c = keys[i].name; *c; c++) { if (*c == '"' || *c == '\\') putchar('\\'); putchar(*c); }
      printf("\" . %ld)\n", keys[i].value);
    }
  }
  printf(" )\n");
  printf(")\n");
  return 0;
}
