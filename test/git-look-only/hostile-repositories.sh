#!/bin/sh
# hostile-repositories.sh REPOS MARKERS: make in REPOS repositories whose settings,
# the files they take in, attributes, hooks and odd files try to run a program that
# makes a file in MARKERS, or to show the analyst something other than what they hold.
# For test/git-look-only.lisp: none of those files may appear, none of the repositories change.
set -e
R=$1; M=$2
mkdir -p "$R" "$M"
G="git -c user.name=Test -c user.email=test@example.invalid -c author.name=Test -c author.email=test@example.invalid -c committer.name=Test -c committer.email=test@example.invalid -c init.defaultBranch=main -c core.hooksPath=/dev/null -c commit.gpgsign=false"
base() { mkdir -p "$R/$1"; cd "$R/$1"; $G init -q $2; printf 'one\ntwo\n' > a.txt; printf 'x\n' > b.txt
         $G add .; $G commit -qm first; printf 'one\nthree\n' > a.txt; $G commit -qam second; printf 'one\nfour\n' > a.txt; }
drivers() { cd "$R/$1"; name=$(printf "$2")
  printf '* filter=%s diff=%s\n' "$name" "$name" > .git/info/attributes
  printf '[filter "%s"]\n\tclean = touch %s/%s-clean; cat\n\tsmudge = touch %s/%s-smudge; cat\n\tprocess = touch %s/%s-process\n[diff "%s"]\n\ttextconv = touch %s/%s-textconv; cat\n\tcommand = touch %s/%s-command\n' \
    "$name" "$M" "$3" "$M" "$3" "$M" "$3" "$name" "$M" "$3" "$M" "$3" >> .git/config; }
base ascii; drivers ascii 'evil' ascii
base bytes; drivers bytes '\377\376' bytes
base latin; drivers latin 'caf\351' latin
base empty; drivers empty '' empty
base caseful; cd "$R/caseful"; printf '* filter=X\n' > .git/info/attributes; printf '[Filter "X"]\n\tClean = touch %s/caseful-clean; cat\n' "$M" >> .git/config
base incl; cd "$R/incl"; printf '* filter=inc diff=inc\n' > .git/info/attributes
  printf '[filter "inc"]\n\tclean = touch %s/incl-clean; cat\n[diff "inc"]\n\ttextconv = touch %s/incl-textconv; cat\n' "$M" "$M" > .git/evil.cfg
  printf '[includeIf "onbranch:main"]\n\tpath = evil.cfg\n' >> .git/config
base tree; cd "$R/tree"; printf '* filter=t diff=t\n' > .gitattributes; printf '[filter "t"]\n\tclean = touch %s/tree-clean; cat\n[diff "t"]\n\ttextconv = touch %s/tree-textconv; cat\n' "$M" "$M" >> .git/config
base fsmon; cd "$R/fsmon"; printf '[core]\n\tfsmonitor = touch %s/fsmon\n\tpager = touch %s/pager\n[pager]\n\tstatus = touch %s/pager-status\n[diff]\n\texternal = touch %s/external\n' "$M" "$M" "$M" "$M" >> .git/config
base hooks; cd "$R/hooks"; for h in post-index-change reference-transaction pre-auto-gc post-checkout; do
  printf '#!/bin/sh\ntouch %s/hook-%s\n' "$M" "$h" > .git/hooks/$h; chmod +x .git/hooks/$h; done
base split; cd "$R/split"; $G update-index --split-index; printf 'one\nfive\n' > b.txt; $G add b.txt; printf 'one\nsix\n' > b.txt
base grafts; cd "$R/grafts"; $G commit -qam third; $G rev-parse HEAD > .git/info/grafts
base mailmap; cd "$R/mailmap"; printf 'Forged Name <forged@example.invalid> <test@example.invalid>\n' > .mailmap; $G add .mailmap; $G commit -qm mailmap
base replaced; cd "$R/replaced"; first=$($G rev-parse HEAD~1); second=$($G rev-parse HEAD); $G replace "$second" "$first"
base lazy; cd "$R/lazy"; blob=$($G rev-parse HEAD:b.txt); rm -f .git/objects/$(echo $blob | cut -c1-2)/$(echo $blob | cut -c3-)
  printf '[remote "origin"]\n\turl = ext::sh -c touch%% %s/lazy-ext\n\tpromisor = true\n[extensions]\n\tpartialClone = origin\n[protocol "ext"]\n\tallow = always\n' "$M" >> .git/config
  $G config core.repositoryformatversion 1
base gc; cd "$R/gc"; printf '[gc]\n\tauto = 1\n\tautoDetach = false\n[maintenance]\n\tauto = true\n[core]\n\tsplitIndex = true\n\tuntrackedCache = true\n[index]\n\tversion = 4\n' >> .git/config
  for i in 1 2 3 4 5 6 7 8 9 10; do echo $i | $G hash-object -w --stdin >/dev/null; done
base idxlink; cd "$R/idxlink"; mv .git/index "$R/outside-index"; ln -s "$R/outside-index" .git/index
base idxfifo; cd "$R/idxfifo"; rm .git/index; mkfifo .git/index
base cfgfifo; cd "$R/cfgfifo"; mkfifo .git/inc; printf '[include]\n\tpath = inc\n' >> .git/config
base reft --ref-format=reftable; cd "$R/reft"; $G tag v1; $G stash -q; printf 'one\nfive\n' > a.txt
base sha --object-format=sha256; cd "$R/sha"; $G tag v1
base unknown; cd "$R/unknown"; $G config core.repositoryformatversion 1; $G config extensions.someday yes
base nest; cd "$R/nest"; printf '* filter=evil\n' > .git/info/attributes; printf '[filter "evil"]\n\tclean = touch %s/nest-clean; cat\n' "$M" >> .git/config
  mkdir -p X/.git; printf 'x\n' > X/a.txt
base holder; cd "$R/holder"; $G init -q --bare inner.git; printf '[core]\n\tfsmonitor = touch %s/bare-fsmon\n' "$M" >> inner.git/config
