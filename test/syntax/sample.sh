#!/usr/bin/env bash
# A sample of shell script for the colouring tests.
set -euo pipefail

readonly NAME="sample"
declare -a items=(one two "three four")
local_count=0
export PATH="$HOME/bin:$PATH"

greet() {
    local who="${1:-world}"
    echo "Hello, $who ($NAME)"
    printf '%s\n' "raw 'single' text"
    return 0
}

function count_items {
    for item in "${items[@]}"; do
        local_count=$((local_count + 1))
        echo "$item" >> /dev/null 2>&1
    done
    echo "$local_count items, last status $?"
}

case "${2:-}" in
    start|run)
        greet "$USER"
        ;;
    stop)
        exit 1
        ;;
    *)
        echo "usage: $0 start|stop" >&2
        ;;
esac

if [[ -d "$HOME" && ! -f /nonexistent ]]; then
    cd "$HOME" || exit
elif [ "$local_count" -gt 3 ]; then
    echo "many"
else
    true
fi

while read -r line; do
    echo "line: $line"
done < <(printf 'a\nb\n')

today=$(date +%Y-%m-%d)
files=`ls | wc -l`
cat <<END
Today is $today, with $files files.
END

until [ "$local_count" -ge 5 ]; do
    local_count=$((local_count + 2))
done

trap 'echo bye' EXIT
count_items | tee /dev/null
test -n "$today" && echo ok || echo no
