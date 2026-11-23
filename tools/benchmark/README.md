# Large files in four editors

This benchmark compares Tuatara with GNU Emacs, Neovim and Helix on large
files. Anyone can run it again and get the same files. It measures:

- how long each editor takes to open a file;
- how long each key takes while typing;
- how long each page down and the jump to the end of the file take;
- how long a search over the whole file takes;
- how much memory each editor uses;
- how often an editor freezes, meaning a key takes over a second;
- how a replacement in every line of a gigantic file goes: typing while it
  runs, then saving and undoing it (the replace run, below).

The files are made up from a fixed seed by `make-data.py`: a CSV table of
2 GiB, a gigantic CSV table of 11 GiB (two times, the second as seconds, a
sentence and a token in each row), a log of 1 GiB, an Org note of 10 MiB
and a JSON file of 20 MiB on one line. The gigantic table is not far below
the 16 GiB of memory each editor is given: an editor that reads a whole file
into memory may not open it, and the report says so. The same sizes and seed
give the same bytes on any machine, and `manifest.json` beside the files
lists their SHA-256 sums. The files take about 15 GiB of disk.

Visual Studio Code is left out. It has no terminal mode, so it cannot be
driven the same way.

## Running it

You need Emacs, Neovim (`nvim`), Helix (`hx`), Python 3, `taskset`,
`systemd-run` with a user session, and a build of Tuatara
(`tools/build-image.sh`). Run it on an idle machine:

    tools/benchmark/run.sh

The full run takes three hours or more. The report is written to
`~/.cache/tuatara-benchmark/report.md`, and the generated files are kept
there for the next run. `BENCH_WORK` picks another directory, which must not
be a temporary directory: Tuatara opens files there read-only.

`tools/benchmark/run.sh --trial` uses small files, fewer keys and two
repetitions, to check that everything works. Options after it go to the
harness, for example `--editors emacs,tuatara` or `--files csv`.
`BENCH_TUATARA` measures another build of Tuatara. `BENCH_EMACS_VLF`, set to
a directory holding vlf.el, gives Emacs the vlf package.

The processors and memory come from `reference-env.sh`. They stand in for a
laptop of 16 GB with an Intel Core Ultra X7 358H. The default processor list
fits an i9-13900KF. On another machine, set `TUATARA_REFERENCE_CPUS` and
`TUATARA_REFERENCE_SPARE` to the nearest layout it allows.

## The fairness rules

- Every editor runs in its terminal mode in the same 120 by 40
  pseudo-terminal, driven by the same harness, on the same files.
- Every editor runs in a systemd user scope whose memory is capped at 16 GB,
  pinned by `taskset` to the same processors. The harness itself runs on
  other processors.
- Each editor gets an empty home of its own, made afresh for every run, so
  nobody's own settings are read.
- Each editor gets its stock setup for large files. For Emacs that means
  so-long turned on, and no question before opening a large file. Neovim,
  Helix and Tuatara run stock.
- Emacs starts without the files a distribution adds to it, because
  Fedora's bring warnings that take half the screen.
- Where so-long makes the buffer read-only, stock Emacs cannot type into the
  file, and the report says so. A second Emacs row, "made writable", runs
  C-x C-q first and times the typing.
- Before each timed run, the editor is started once on a small file, not
  timed. The benchmark file is read once, so that every editor opens it from
  the page cache.
- Each key gets two times. "Shown" runs from sending the key until the
  editor's output has settled, with no output for 100 ms (not counted). This
  is what a person waits for, and freezes are counted on it. "Idle" runs on
  until the editor's processes have also used no processor time, and none
  of their threads has waited on the disk, for 100 ms. This is the work
  the key cost, including work done in the background after the screen
  settled.
- Before each key, the harness waits for 400 ms with no output. A redraw
  that an editor delays on purpose therefore does not land in the next key.
  An editor that collects garbage when typing pauses (Tuatara does after
  0.3 s) may do so during this wait, which a person typing steadily would not
  give it time for.
- A search is typed one key every 150 ms, as a person types. Only the final
  RET is timed: until the match is on the screen (shown), and until the
  editor is idle.
- Memory is the largest resident set of the editor's processes together,
  read from /proc.
- If one step takes over 120 s, the run stops and is reported as timed out.
  An editor that crashes or refuses is reported, never left out.
- The editors take turns in a different order in each repetition.

## The replace run

The tables above time small edits. The replace run times one large change
in the gigantic table, and what a person does while it runs:

    tools/benchmark/run.sh --replace

Each editor opens a fresh copy of the file and replaces fox with cat in all
of it, about 103 million times, with its own keys. At once, without waiting,
it makes a new buffer and types "the quick brown fox jumps over the lazy
dog" into it. Then it goes back to the file, saves it and undoes the
replacement. The harness types each step's keys one every 150 ms, as a
person types.

The report gives each editor's typing while the replacement runs, each key
timed until it shows, with any freezes, and the time to switch to the new
buffer and back. It gives the time until the
replacement is finished, from its last key until the file's lines show cat
and the editor has stopped using the processor and the disk. It gives the
time to save, after which the whole file is read to check that no fox is
left, and the time to undo, until the lines show fox again.

The terminal is 192 by 54, the size a full-screen terminal has on a screen
of 1920 by 1080. Each editor runs once, and each step is given an hour. An
editor still busy after that, or one that runs out of its 16 GB, is
reported so. The report gives the memory charged against the cap, and the
resident memory, which also counts the pages of a file an editor maps from
disk. The copy needs about 11 GiB of free disk. The report goes to
`replace-report.md`, and the results to `replace-results.jsonl`.

    tools/benchmark/run.sh --replace --film

also records each editor's screen as it runs and makes `replace-film.mp4`,
the four editors side by side, each from its own start, so that the film
shows what each would have shown in the same seconds. The first 90 seconds
play at their true speed, the rest 30 times as fast, as the clock at the top
says. `film.py` makes it; it needs Pillow and ffmpeg, which the benchmark
itself does not.

## Reading the report

The report starts with the machine, the editors' versions and setups, the
files and these rules. Then come two tables for each file: one of shown
times, with memory and freezes, and one of idle times. Times are in
milliseconds. For typing and page down, a table cell shows three figures:
the median, the 95th percentile and the worst key over all repetitions.
Memory is the largest seen in any repetition. Freezes are counted over all
repetitions. Under each table are the runs that did not go as planned, and
why.

The last section lists every case where Tuatara is slower or larger than
another editor, however slightly. Differences of a millisecond or less are
within what the harness can tell apart.

## The files here

- `run.sh` makes the files, runs the harness and writes the report.
- `reference-env.sh` holds the processors and memory cap, which the speed
  tests share.
- `make-data.py` writes the benchmark files.
- `harness.py` runs the editors in a pseudo-terminal and times them.
- `screen.py` is the small terminal screen the harness reads output into.
- `keys.py` lists each editor, how it is started, and the same edits in its
  keys.
- `emacs-init.el` holds Emacs's settings.
- `report.py` turns the results into the Markdown report.
- `film.py` turns the recordings of a replace run into a film.
