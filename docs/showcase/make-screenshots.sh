#!/bin/sh
# Reproduce docs/showcase/agents.png: a scratch hc core (short socket dir under
# /tmp, its own HOME) with fake agents on fake labels, shown by herdr-core-agents
# in a terminal Emacs, captured with vhs.  Needs hc, emacs, vhs (and its ttyd and
# ffmpeg).  Usage: docs/showcase/make-screenshots.sh [OUTPUT.png]
set -eu
here=$(cd "$(dirname "$0")/../.." && pwd)
out=${1:-$here/docs/showcase/agents.png}
root=$(mktemp -d /tmp/hcshow.XXXXXX)
mkdir -p "$root/run" "$root/cwd"; chmod 700 "$root/run"
env_sh="$root/env.sh"
cat > "$env_sh" <<EOF
export HOME=$root XDG_RUNTIME_DIR=$root/run XDG_STATE_HOME=$root/state
export XDG_CONFIG_HOME=$root/cfg XDG_DATA_HOME=$root/data TERM=xterm-256color
unset HC_SESSION HC_TERMINAL_ID
EOF
# shellcheck disable=SC1090
. "$env_sh"
trap 'hc server stop >/dev/null 2>&1 || true; rm -rf "$root"' EXIT
hc server start --json >/dev/null
agent() { # name space tab app state [kind]
  hc run --bg --json --name "$1" --cwd "$root/cwd" \
    --label "herdr.space=$2" --label "herdr.tab=$3" --label "session.title=$2 · $3" \
    -- sh -c "printf '\\033]7501;state=$5${6:+:kind=$6}:app=$4\\033\\\\'; sleep 600" >/dev/null
}
agent juniper-build juniper "build-7 - Fix the cache" claude blocked permission
agent juniper-docs juniper "docs - Write the guide" claude working
agent tidepool-lead tidepool "tidepool-lead" codex idle
agent tidepool-api tidepool "api - Add pagination" claude done
agent quillwork-run quillwork "named-run-x" codex blocked question
sleep 2
cat > "$root/emacs.sh" <<EOF
#!/bin/sh
. $env_sh
exec emacs -nw -Q -L $here -l herdr-core.el --eval '(progn (herdr-core-agents) (delete-other-windows))'
EOF
chmod +x "$root/emacs.sh"
cat > "$root/show.tape" <<EOF
Output "$root/show.gif"
Set Shell "bash"
Set FontSize 18
Set Width 1500
Set Height 420
Set Padding 12
Type "$root/emacs.sh"
Enter
Sleep 5s
Screenshot "$out"
Sleep 500ms
Ctrl+x
Ctrl+c
EOF
vhs "$root/show.tape" >/dev/null
echo "wrote $out"
