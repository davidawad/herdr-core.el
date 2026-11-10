# Showcase

`agents.png`: `herdr-core-agents` in a terminal Emacs against a scratch hc core
with five fake agents on fake labels: the vuistack-table list grouped by space
(a group row per space with its agent count and a glyph count of the agents
needing attention), each group sorted by attention (blocked first, then done,
working, idle).

Reproduce (needs `hc`, `emacs`, `vhs` with its `ttyd` and `ffmpeg`, and the
deps `make deps` fetches):

    docs/showcase/make-screenshots.sh docs/showcase/agents.png

The script starts its own core under `/tmp` (own HOME and socket directory),
touches nothing else, and removes it afterwards.
