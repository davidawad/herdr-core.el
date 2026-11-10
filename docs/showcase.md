# Showcase

`agents.png`: `herdr-core-agents` in a terminal Emacs against a scratch hc core
with five fake agents on fake labels (blocked first, then done, working, idle).

Reproduce (needs `hc`, `emacs`, `vhs`):

    docs/showcase/make-screenshots.sh docs/showcase/agents.png

The script starts its own core under `/tmp` (own HOME and socket directory),
touches nothing else, and removes it afterwards.
