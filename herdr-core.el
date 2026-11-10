;;; herdr-core.el --- herdr-core (hc) agents in Emacs/Ghostel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ngoc Nguyen, David Awad

;; Author: Ngoc Nguyen, David Awad <me@davidaw.ad>
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1") (transient "0.4") (vui "1.4") (vuistack-table "0.2"))
;; Keywords: terminals, tools
;; URL: https://gitlab.com/davidawad/herdr-core.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs client for herdr-core (hc): every agent on every machine
;; the core reaches (this host, ssh machines, pod machines), with its
;; OSC 7501 status.  It talks to hc only (`hc events/ls/get/read/send/
;; run/attach/wait/...  --json'; `hc schema' is the OpenRPC spec).
;; Derived from emacs-herdr by Ngoc Nguyen
;; (https://github.com/baongoc124/emacs-herdr, e2ea729, 2026-10-12,
;; GPL-3.0-or-later), ported to talk to hc instead of the herdr TUI.
;;
;; Seeing agents
;;   `herdr-core-agents'          live list (a vuistack-table), grouped and sorted as you like
;;   `herdr-core-mode'            event stream, mode-line count, notifications
;;   `herdr-core-menu'            transient menu of everything below
;; Switching
;;   `herdr-core-switch-agent'    attention-sorted picker
;;   `herdr-core-next-agent', `herdr-core-previous-agent', `herdr-core-last-agent'
;;   `herdr-core-goto-blocked', `herdr-core-next-attention'
;;   `herdr-core-switch-machine', `herdr-core-switch-space', `herdr-core-switch-tab'
;; Attaching
;;   `herdr-core-attach-agent', `herdr-core-observe-agent', `herdr-core-attach-terminal'
;;   `herdr-core-attach-several', `herdr-core-attach-group'  side by side
;;   `herdr-core-read-agent'      screen and history, links/files/images/diffs rendered
;; Talking
;;   `herdr-core-send-region', `herdr-core-submit-region', `herdr-core-send-keys',
;;   `herdr-core-interrupt-agent', `herdr-core-put-file', `herdr-core-wait-agent',
;;   `herdr-core-rename-agent', `herdr-core-kill-agent'
;; Starting
;;   `herdr-core-new-agent'       through `herdr-core-launcher' (hc run)
;;
;; State comes pushed from `hc events --json' (a snapshot line, then one
;; line per change; `missed' is a fresh snapshot); `herdr-core-refresh-policy'
;; can poll `hc ls' instead, or while the stream is down.  Buffers carry
;; the session's one name: the `session.title' label,
;; `*hc: juniper · build-7 - Fix it*', else the hc address.  Every behavior
;; is a defcustom or hook, in one customize group per area under
;; `herdr-core'; README.org lists them all.

;;; Code:

(require 'ansi-color)
(require 'button)
(require 'format-spec)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'vc-git)
(require 'vui)
(require 'vuistack-table)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-mode "ghostel" ())
(declare-function term-exec "term" (buffer name command startfile switches))
(declare-function term-mode "term" ())
(declare-function term-char-mode "term" ())
(declare-function notifications-notify "notifications" (&rest params))
(declare-function diff-mode "diff-mode" ())
(defvar ghostel-keymap-exceptions)
(defvar ghostel--process)
(defvar herdr-core-mode)
(defvar herdr-core-command-map)

;;; Customization groups

(defgroup herdr-core nil
  "herdr-core agents in Emacs."
  :group 'tools
  :prefix "herdr-core-")

(defgroup herdr-core-backend nil
  "How herdr-core runs hc."
  :group 'herdr-core)

(defgroup herdr-core-events nil
  "How herdr-core learns about changes: the hc event stream, or polling."
  :group 'herdr-core)

(defgroup herdr-core-agents nil
  "Which terminals are agents, and what they are called."
  :group 'herdr-core)

(defgroup herdr-core-list nil
  "The agent list buffer: columns, sorting, grouping."
  :group 'herdr-core)

(defgroup herdr-core-picker nil
  "Picking and switching agents."
  :group 'herdr-core)

(defgroup herdr-core-display nil
  "Attach buffers, display actions and window layouts."
  :group 'herdr-core)

(defgroup herdr-core-render nil
  "Rendering what agents produce in read buffers."
  :group 'herdr-core)

(defgroup herdr-core-launcher nil
  "Starting new agents."
  :group 'herdr-core)

(defgroup herdr-core-notify nil
  "Notifications and the mode line."
  :group 'herdr-core)

(defgroup herdr-core-keys nil
  "Keys and the transient menu."
  :group 'herdr-core)

(defgroup herdr-core-faces nil
  "Faces of herdr-core."
  :group 'herdr-core
  :group 'faces)

;;; Backend

(defcustom herdr-core-executable "hc"
  "The herdr-core CLI."
  :type 'string
  :group 'herdr-core-backend)

(defcustom herdr-core-executable-args nil
  "Arguments put before every hc command.
With `herdr-core-executable' \"ssh\" and this (\"box\" \"hc\"), every
call goes to box's hc."
  :type '(repeat string)
  :group 'herdr-core-backend)

(defcustom herdr-core-process-environment nil
  "Extra VAR=VALUE entries in the environment of every hc process."
  :type '(repeat string)
  :group 'herdr-core-backend)

(defcustom herdr-core-start-core 'ask
  "What to do when the hc core is not running and a command needs it."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Start it" always)
                 (const :tag "Fail" never))
  :group 'herdr-core-backend)

;;; Events

(defcustom herdr-core-refresh-policy 'events
  "How the agent state is kept current.
`events': follow `hc events --json' (pushed), restarting it when it ends.
`events+poll': the same, and poll `hc ls' while the stream is down.
`poll': only poll `hc ls --json' every `herdr-core-poll-interval' seconds."
  :type '(choice (const :tag "hc events (push)" events)
                 (const :tag "hc events, poll while it is down" events+poll)
                 (const :tag "Poll hc ls" poll))
  :group 'herdr-core-events)

(defcustom herdr-core-poll-interval 10
  "Seconds between polls of `hc ls' (see `herdr-core-refresh-policy')."
  :type 'number
  :group 'herdr-core-events)

(defcustom herdr-core-poll-details t
  "Non-nil completes each polled terminal with `hc get' (blocked kind, labels).
Nil polls only `hc ls', one call however many terminals there are."
  :type 'boolean
  :group 'herdr-core-events)

(defcustom herdr-core-reconnect-delay 5
  "Seconds before a dropped event stream is started again."
  :type 'number
  :group 'herdr-core-events)

(defcustom herdr-core-refresh-delay 0.2
  "Seconds to gather a burst of events into one redraw."
  :type 'number
  :group 'herdr-core-events)

(defcustom herdr-core-snapshot-timeout 2
  "Seconds a picker waits for the stream's snapshot before asking `hc ls'."
  :type 'number
  :group 'herdr-core-events)

(defcustom herdr-core-event-functions nil
  "Functions called with each parsed message of the event stream, after it applies."
  :type 'hook
  :group 'herdr-core-events)

(defcustom herdr-core-refresh-hook nil
  "Hook run after the list buffer, buffer names and mode line are redrawn."
  :type 'hook
  :group 'herdr-core-events)

;;; Agents

(defcustom herdr-core-labelled-only nil
  "Non-nil shows only terminals with a `herdr-core-space-label' (the Herdr app's)."
  :type 'boolean
  :group 'herdr-core-agents)

(defcustom herdr-core-show-finished nil
  "Non-nil also shows finished terminals."
  :type 'boolean
  :group 'herdr-core-agents)

(defcustom herdr-core-hidden-machines nil
  "Machines whose terminals are never shown (\"local\" is this host)."
  :type '(repeat string)
  :group 'herdr-core-agents)

(defcustom herdr-core-filter-functions nil
  "Predicates on an agent alist; an agent is shown only when all return non-nil."
  :type 'hook
  :group 'herdr-core-agents)

(defcustom herdr-core-title-label "session.title"
  "The label holding a session's one name."
  :type 'string
  :group 'herdr-core-agents)

(defcustom herdr-core-space-label "herdr.space"
  "The label naming a terminal's space (the Herdr app's sidebar)."
  :type 'string
  :group 'herdr-core-agents)

(defcustom herdr-core-tab-label "herdr.tab"
  "The label naming a terminal's tab within its space."
  :type 'string
  :group 'herdr-core-agents)

(defcustom herdr-core-name-sources '(session tab name address)
  "Where an agent's display name comes from, the first non-empty one.
Each is an agent key (session, tab, name, address, kind, space) or a
label name string."
  :type '(repeat (choice symbol string))
  :group 'herdr-core-agents)

(defcustom herdr-core-name-function #'herdr-core-default-name
  "Function of an agent alist returning its display name."
  :type 'function
  :group 'herdr-core-agents)

(defcustom herdr-core-buffer-name-function #'herdr-core-default-buffer-name
  "Function of (KIND TITLE) returning a buffer's base name.
KIND is nil (attach), \"observe\" or \"read\"; TITLE is `herdr-core-title'."
  :type 'function
  :group 'herdr-core-agents)

(defcustom herdr-core-status-glyphs
  '(("blocked" . "●") ("error" . "✗") ("done" . "✓") ("working" . "◐")
    ("idle" . "·") ("unknown" . "?"))
  "Glyph per status, in header lines and the glyph column."
  :type '(alist :key-type string :value-type string)
  :group 'herdr-core-agents)

(defcustom herdr-core-status-priority
  '(("blocked" . 0) ("error" . 1) ("done" . 2) ("working" . 3) ("idle" . 4) ("unknown" . 5))
  "Attention order of statuses: lower comes first."
  :type '(alist :key-type string :value-type integer)
  :group 'herdr-core-agents)

(defcustom herdr-core-attention-statuses '("blocked" "error" "done")
  "Statuses `herdr-core-next-attention' stops at."
  :type '(repeat string)
  :group 'herdr-core-agents)

(defcustom herdr-core-pod-machine-regexp "-a[0-9]+\\'"
  "Machine names matching this are pods (a pod's machine name such as pod-x-a1)."
  :type 'regexp
  :group 'herdr-core-agents)

(defcustom herdr-core-machine-kind-function #'herdr-core-machine-kind
  "Function of (NAME RECORD) returning a machine's kind string.
RECORD is its `hc machine ls' entry or nil; the default gives local,
pod, command (a --via machine) or ssh."
  :type 'function
  :group 'herdr-core-agents)

;;; List

(defcustom herdr-core-list-columns
  '(("Status" 9 herdr-core-column-status)
    ("Session" 44 herdr-core-column-name)
    ("Kind" 8 herdr-core-column-kind)
    ("Machine" 16 herdr-core-column-machine)
    ("Space" 14 herdr-core-column-space)
    ("Address" 0 herdr-core-column-address))
  "Columns of the agent list table: (TITLE WIDTH FUNCTION [PROPS]).
FUNCTION gets the agent alist and returns the cell's string.  Built in,
each `herdr-core-column-' and: status, glyph, name, kind, machine,
space, tab, cwd, message, age, address.  WIDTH is the column's least
width; it grows to its widest cell, up to `vuistack-table-max-auto-width'.
PROPS, a plist of `vuistack-table' column keys (:hidden t,
:sort-fn, :aggregation, :action ...), goes into the column spec."
  :type '(repeat (choice (list (string :tag "Title") (natnum :tag "Width") (function :tag "Cell"))
                         (list (string :tag "Title") (natnum :tag "Width") (function :tag "Cell")
                               (plist :tag "vuistack-table column keys"))))
  :group 'herdr-core-list)

(defcustom herdr-core-sort-predicate #'herdr-core-sort-by-attention
  "Predicate of two agents ordering every list, picker and cycle.
In the list table, attention is the default multi-sort (attention, then
most recent change) and most recent the change column, sorted descending;
any other predicate orders the rows the table starts from."
  :type '(choice (const :tag "Attention, then most recent" herdr-core-sort-by-attention)
                 (const :tag "Most recent change" herdr-core-sort-by-recent)
                 (const :tag "Name" herdr-core-sort-by-name)
                 (const :tag "Machine, then attention" herdr-core-sort-by-machine)
                 (const :tag "Address" herdr-core-sort-by-address)
                 function)
  :group 'herdr-core-list)

(defcustom herdr-core-group-by nil
  "What the agent list (and picker) groups by.
nil: no groups; `machine', `space', `tab', `kind', `status'; (label KEY):
that label; or a function of an agent returning its group name.  In the
list it is the table's grouping, on a group column of its own."
  :type '(choice (const :tag "No groups" nil)
                 (const machine) (const space) (const tab) (const kind) (const status)
                 (list :tag "Label" (const label) string)
                 function)
  :group 'herdr-core-list)

(defcustom herdr-core-group-order 'first
  "Order of groups: `first' by their first agent (attention), `name' by name."
  :type '(choice (const :tag "By first agent" first) (const :tag "By name" name))
  :group 'herdr-core-list)

(defcustom herdr-core-group-header-function #'herdr-core-group-header
  "Function of (GROUP AGENTS) returning the text of a group's row.
The table adds the count of agents after it, and the status columns
count the agents needing attention."
  :type 'function
  :group 'herdr-core-list)

(defcustom herdr-core-list-page-size nil
  "Agents per page of the list table, or nil for one page."
  :type '(choice (const :tag "One page" nil) natnum)
  :group 'herdr-core-list)

;;; Picker

(defcustom herdr-core-candidate-function #'herdr-core-format-candidate
  "Function of an agent returning its picker candidate string."
  :type 'function
  :group 'herdr-core-picker)

(defcustom herdr-core-picker-group-by 'list
  "Completion groups in the picker: `list' follows `herdr-core-group-by',
nil groups nothing, else a value as in `herdr-core-group-by'."
  :type '(choice (const :tag "As the list" list)
                 (const :tag "No groups" nil)
                 (const machine) (const space) (const tab) (const kind) (const status)
                 function)
  :group 'herdr-core-picker)

(defcustom herdr-core-completing-read-function 'completing-read
  "The `completing-read' herdr-core uses."
  :type 'function
  :group 'herdr-core-picker)

(defcustom herdr-core-cycle-scope 'all
  "What `herdr-core-next-agent' and `herdr-core-previous-agent' cycle through:
`all' agents, or those on the current agent's `machine', `space' or `tab'."
  :type '(choice (const all) (const machine) (const space) (const tab))
  :group 'herdr-core-picker)

;;; Display

(defcustom herdr-core-terminal-function #'herdr-core-terminal
  "Function of (NAME PROGRAM ARGS &optional ACTION) that runs PROGRAM in a
terminal buffer NAME, displays it with `display-buffer' ACTION and
returns the buffer."
  :type 'function
  :group 'herdr-core-display)

(defcustom herdr-core-display-actions
  '((attach . ((display-buffer-same-window)))
    (observe . ((display-buffer-same-window)))
    (switch . ((display-buffer-same-window)))
    (launch . ((display-buffer-same-window)))
    (picker . ((display-buffer-same-window)))
    (list . ((display-buffer-same-window)))
    (read . nil)
    (show . ((display-buffer-reuse-window display-buffer-use-some-window
              display-buffer-pop-up-window)
             (inhibit-same-window . t))))
  "`display-buffer' action per context.
attach/observe: a picked agent; switch: next/previous/blocked; launch:
a new agent; picker: *herdr-core*; list: the agent list; read: read
buffers; show: a notification click (never the selected window)."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'herdr-core-display)

(defcustom herdr-core-layout 'columns
  "How `herdr-core-attach-several' arranges agents side by side:
`columns', `rows', `grid', or a function of a list of agents that
shows them (and may call `herdr-core-attach-in-window')."
  :type '(choice (const columns) (const rows) (const grid) function)
  :group 'herdr-core-display)

(defcustom herdr-core-layout-max 4
  "Most agents shown side by side."
  :type 'natnum
  :group 'herdr-core-display)

(defcustom herdr-core-layout-delete-other-windows t
  "Non-nil makes a side-by-side layout fill the frame; nil splits the selected window."
  :type 'boolean
  :group 'herdr-core-display)

(defcustom herdr-core-attach-history 0
  "Scrollback lines `hc attach' shows first (its --history)."
  :type 'natnum
  :group 'herdr-core-display)

(defcustom herdr-core-attach-header-line t
  "Show a clickable strip of all agents in the header line of attach buffers.
Agents are sorted by attention (blocked first); clicking one switches to it."
  :type 'boolean
  :group 'herdr-core-display)

(defcustom herdr-core-header-label-width 16
  "Maximum characters per agent label in the attach buffer header line."
  :type 'natnum
  :group 'herdr-core-display)

(defcustom herdr-core-header-label-function #'herdr-core-header-label
  "Function of an agent returning its header-line label."
  :type 'function
  :group 'herdr-core-display)

(defcustom herdr-core-attach-hook nil
  "Hook run in each new attach buffer."
  :type 'hook
  :group 'herdr-core-display)

(defcustom herdr-core-read-lines 500
  "History lines `herdr-core-read-agent' shows above the screen."
  :type 'natnum
  :group 'herdr-core-display)

(defcustom herdr-core-wait-timeout nil
  "Seconds `herdr-core-wait-agent' waits before hc checks in (its --timeout);
nil is hc's own default (config `wait_timeout', else 180)."
  :type '(choice (const :tag "hc's default" nil) natnum)
  :group 'herdr-core-display)

(defcustom herdr-core-read-follow t
  "Non-nil reads an agent again when its turn or status changes."
  :type 'boolean
  :group 'herdr-core-display)

(defcustom herdr-core-read-hook nil
  "Hook run in a read buffer after it is filled and rendered."
  :type 'hook
  :group 'herdr-core-display)

;;; Render

(defcustom herdr-core-renderers
  '(herdr-core-render-ansi herdr-core-render-diffs herdr-core-render-links
    herdr-core-render-images herdr-core-render-files)
  "Functions of (BEG END AGENT) that decorate a read buffer's text, in order.
Built in: `herdr-core-render-ansi', `herdr-core-render-diffs',
`herdr-core-render-links', `herdr-core-render-images', `herdr-core-render-files'."
  :type 'hook
  :group 'herdr-core-render)

(defcustom herdr-core-url-regexp "\\bhttps?://[^][[:space:]<>\"'`()]+[^][[:space:]<>\"'`().,;:!?]"
  "URLs `herdr-core-render-links' makes into buttons."
  :type 'regexp
  :group 'herdr-core-render)

(defcustom herdr-core-browse-url-function #'browse-url
  "Function of a URL that opens it."
  :type 'function
  :group 'herdr-core-render)

(defcustom herdr-core-file-regexp
  (concat "\\(?:^\\|[[:space:]\"'`(]\\)\\("
          "\\(?:~\\|\\.\\{1,2\\}\\)?/[[:alnum:]._~+@-][[:alnum:]._/~+@-]*" ; /abs, ~/x, ./x
          "\\|[[:alnum:]_-][[:alnum:]._-]*/[[:alnum:]._/-]*\\.[[:alnum:]]+" ; dir/file.ext
          "\\|[[:alnum:]_-][[:alnum:]_-]*\\(?:\\.[[:alnum:]_-]+\\)*\\.[[:alpha:]][[:alnum:]]*" ; file.ext
          "\\)\\(?::\\([0-9]+\\)\\)?")
  "File paths (group 1, optional :LINE in group 2) `herdr-core-render-files' links.
On this host only existing files are linked; elsewhere only absolute paths."
  :type 'regexp
  :group 'herdr-core-render)

(defcustom herdr-core-image-regexp "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|webp\\)\\'"
  "File names `herdr-core-render-images' shows as images."
  :type 'regexp
  :group 'herdr-core-render)

(defcustom herdr-core-image-max-width 480
  "Widest an inline image is shown, in pixels."
  :type 'natnum
  :group 'herdr-core-render)

(defcustom herdr-core-render-remote-images 'click
  "Images on other machines: `click' fetches one when its button is pressed,
`inline' fetches each as the buffer renders (`hc exec --on M base64'), nil never."
  :type '(choice (const click) (const inline) (const :tag "Never" nil))
  :group 'herdr-core-render)

(defcustom herdr-core-find-file-function #'find-file-other-window
  "Function of a local file name that opens it."
  :type 'function
  :group 'herdr-core-render)

;;; Launcher

(defconst herdr-core-hc-run-launcher-args
  '("run" "--bg" "--keep" "--outlive-parent" "--no-parent" "--json"
    "--name" "%n" "--cwd" "%d" herdr-core-launch-on-args
    "--label" "herdr.space=%s" "--label" "herdr.tab=%t" "--label" "session.title=%s · %t"
    "--" "%k")
  "`herdr-core-launcher-args' for plain hc (`herdr-core-launcher' \"hc\").")

(defcustom herdr-core-launcher "hc"
  "The program `herdr-core-new-agent' runs: hc itself (`hc run ...'), or a
launcher script of your own.  Its arguments are `herdr-core-launcher-args'."
  :type 'string
  :group 'herdr-core-launcher)

(defcustom herdr-core-launcher-args herdr-core-hc-run-launcher-args
  "Arguments of `herdr-core-launcher'.
In strings, %n is the agent's name, %d its directory, %k its kind
(harness), %m its machine, %s its space (the project), %t its tab
(the name) and %% a percent sign.  A symbol is a function of the spec
alist ((?n . NAME) ...) returning a string, a list of strings or nil;
`herdr-core-launch-on-args' gives --on MACHINE off this host.  The
default, `herdr-core-hc-run-launcher-args', is plain `hc run'; a
launcher script taking `run --name N --cwd D -- KIND' would use
\='(\"run\" \"--name\" \"%n\" \"--cwd\" \"%d\" \"--\" \"%k\")."
  :type '(repeat (choice string function))
  :group 'herdr-core-launcher)

(defcustom herdr-core-launch-function #'herdr-core-launch-command
  "Function of (SPEC CALLBACK) that starts an agent.
SPEC is an alist (name dir kind machine space tab); CALLBACK is called
with the new terminal's address, or nil when it failed."
  :type 'function
  :group 'herdr-core-launcher)

(defcustom herdr-core-default-agent-kind "claude"
  "Default harness for `herdr-core-new-agent'."
  :type 'string
  :group 'herdr-core-launcher)

(defcustom herdr-core-agent-kinds '("claude" "codex" "pi" "opencode" "gemini")
  "Harnesses `herdr-core-new-agent' offers."
  :type '(repeat string)
  :group 'herdr-core-launcher)

(defcustom herdr-core-launch-directory-function #'herdr-core-project-directory
  "Function of no arguments returning the directory a new agent starts in."
  :type 'function
  :group 'herdr-core-launcher)

(defcustom herdr-core-agent-name-function #'herdr-core-default-agent-name
  "Function of (DIRECTORY KIND) returning a new agent's base name."
  :type 'function
  :group 'herdr-core-launcher)

(defcustom herdr-core-attach-after-launch t
  "Non-nil attaches to a new agent once it started."
  :type 'boolean
  :group 'herdr-core-launcher)

(defcustom herdr-core-after-launch-functions nil
  "Functions called with a new agent's address and the launcher's output."
  :type 'hook
  :group 'herdr-core-launcher)

;;; Notify

(defcustom herdr-core-notify-statuses '("blocked" "done" "error")
  "Statuses an agent entering which `herdr-core-notify' reports."
  :type '(repeat string)
  :group 'herdr-core-notify)

(defcustom herdr-core-status-change-functions '(herdr-core-notify)
  "Functions run when an agent's status changes in the event stream.
Each is called with the agent alist, the old status and the new status.
An agent seen for the first time is not a change."
  :type 'hook
  :options '(herdr-core-notify)
  :group 'herdr-core-notify)

(defcustom herdr-core-notify-function #'herdr-core-system-notify
  "Function of (TITLE BODY ADDRESS) showing a notification.
The default is a desktop notification (terminal-notifier, osascript or
D-Bus), clicking which shows the agent; `message' is the quiet choice."
  :type '(choice (const :tag "Desktop" herdr-core-system-notify)
                 (const :tag "Echo area" herdr-core-message-notify)
                 function)
  :group 'herdr-core-notify)

(defcustom herdr-core-notify-when-visible nil
  "Non-nil notifies even while the agent's buffer is selected in a focused frame."
  :type 'boolean
  :group 'herdr-core-notify)

(defcustom herdr-core-mode-line-statuses '("blocked")
  "Statuses the mode-line segment counts."
  :type '(repeat string)
  :group 'herdr-core-notify)

(defcustom herdr-core-mode-line-format " ● %d "
  "Format of the mode-line segment, given the count; nothing shows at zero."
  :type 'string
  :group 'herdr-core-notify)

(defcustom herdr-core-mode-line-function #'herdr-core-mode-line-segment
  "Function of no arguments returning the mode-line segment string."
  :type 'function
  :group 'herdr-core-notify)

;;; Faces

(defface herdr-core-status-blocked '((t :inherit warning))
  "Blocked agents (waiting for a person)." :group 'herdr-core-faces)
(defface herdr-core-status-error '((t :inherit warning))
  "Agents in error." :group 'herdr-core-faces)
(defface herdr-core-status-done '((t :inherit success))
  "Agents done with their turn." :group 'herdr-core-faces)
(defface herdr-core-status-working '((t :inherit default))
  "Working agents." :group 'herdr-core-faces)
(defface herdr-core-status-idle '((t :inherit shadow))
  "Idle agents." :group 'herdr-core-faces)
(defface herdr-core-status-unknown '((t :inherit shadow))
  "Terminals that report no status." :group 'herdr-core-faces)
(defface herdr-core-group-header '((t :inherit bold :extend t))
  "Group header lines in the agent list." :group 'herdr-core-faces)
(defface herdr-core-mode-line '((t :inherit warning))
  "The mode-line segment." :group 'herdr-core-faces)
(defface herdr-core-header-current '((t :inverse-video t))
  "The current agent in attach-buffer header lines." :group 'herdr-core-faces)
(defface herdr-core-link '((t :inherit link))
  "Links and files in read buffers." :group 'herdr-core-faces)

;;; Keys

(defun herdr-core--ghostel-pass (key bind)
  "Add KEY to `ghostel-keymap-exceptions' (remove it when BIND is nil)."
  (when (boundp 'ghostel-keymap-exceptions)
    (if bind
        (unless (member key ghostel-keymap-exceptions)
          (setopt ghostel-keymap-exceptions (append ghostel-keymap-exceptions (list key))))
      (when (member key ghostel-keymap-exceptions)
        (setopt ghostel-keymap-exceptions (remove key ghostel-keymap-exceptions))))))

(defun herdr-core--bind-global (key def bind)
  "Bind KEY to DEF globally and let it through Ghostel; unbind when BIND is nil."
  (when key
    (if bind
        (keymap-global-set key def)
      (when (equal (keymap-global-lookup key) def)
        (keymap-global-unset key)))
    (herdr-core--ghostel-pass key bind)))

(defun herdr-core--bind-menu-key (key bind)
  "Bind KEY to `herdr-core-menu' globally and in Ghostel, or unbind when BIND is nil."
  (herdr-core--bind-global key #'herdr-core-menu bind))

(defcustom herdr-core-menu-repeat-command #'herdr-core-agents
  "Command run by pressing `herdr-core-menu-key' again inside the menu.
`herdr-core-agents' opens the agent list; `herdr-core-toggle' opens or hides *herdr-core*."
  :type '(choice (const :tag "Open agent list" herdr-core-agents)
                 (const :tag "Open / hide *herdr-core*" herdr-core-toggle)
                 function)
  :group 'herdr-core-keys)

(defcustom herdr-core-menu-key nil
  "Global key for `herdr-core-menu', a `key-valid-p' string such as \"C-9\".
Bound as soon as it is set (independently of `herdr-core-mode'), and also
added to `ghostel-keymap-exceptions' so it reaches Emacs from terminal
buffers.  Pressing it again inside the menu runs `herdr-core-menu-repeat-command'."
  :type '(choice (const :tag "None" nil) (string :tag "Key"))
  :set (lambda (sym val)
         (herdr-core--bind-menu-key (and (boundp sym) (symbol-value sym)) nil)
         (set-default sym val)
         (herdr-core--bind-menu-key val t))
  :group 'herdr-core-keys)

(defcustom herdr-core-keymap-prefix nil
  "Global prefix key for `herdr-core-command-map', such as \"C-c h\".
Bound as soon as it is set and let through Ghostel."
  :type '(choice (const :tag "None" nil) (string :tag "Key"))
  :set (lambda (sym val)
         (when (boundp 'herdr-core-command-map)
           (herdr-core--bind-global (and (boundp sym) (symbol-value sym)) herdr-core-command-map nil))
         (set-default sym val)
         (when (boundp 'herdr-core-command-map)
           (herdr-core--bind-global val herdr-core-command-map t)))
  :group 'herdr-core-keys)

(defcustom herdr-core-menu-suffixes
  '(("l" "Agent list" herdr-core-agents)
    ("o" "Open *herdr-core* (hc picker)" herdr-core)
    ("q" "Hide *herdr-core*" herdr-core-hide)
    ("s" "Switch agent" herdr-core-switch-agent)
    ("a" "Attach agent in buffer" herdr-core-attach-agent)
    ("t" "Attach any terminal" herdr-core-attach-terminal)
    ("A" "Attach several side by side" herdr-core-attach-several)
    ("G" "Attach a group side by side" herdr-core-attach-group)
    ("w" "Watch agent" herdr-core-observe-agent)
    ("v" "Read agent screen" herdr-core-read-agent)
    ("SPC" "Last agent buffer" herdr-core-last-agent)
    ("]" "Next agent" herdr-core-next-agent)
    ("[" "Previous agent" herdr-core-previous-agent)
    ("." "Next needing attention" herdr-core-next-attention)
    ("M" "Switch machine" herdr-core-switch-machine)
    ("S" "Switch space" herdr-core-switch-space)
    ("T" "Switch tab" herdr-core-switch-tab)
    ("n" "New agent for project" herdr-core-new-agent)
    ("b" herdr-core--menu-blocked-description herdr-core-goto-blocked)
    ("r" "Send region" herdr-core-send-region)
    ("R" "Send region, even to a blocked agent" herdr-core-submit-region)
    ("k" "Send keys" herdr-core-send-keys)
    ("i" "Interrupt agent" herdr-core-interrupt-agent)
    ("p" "Put a file beside an agent" herdr-core-put-file)
    ("W" "Notify when an agent stops" herdr-core-wait-agent)
    ("e" "Rename agent (session title)" herdr-core-rename-agent)
    ("m" herdr-core--menu-mode-description herdr-core-mode))
  "Entries of `herdr-core-menu': (KEY DESCRIPTION COMMAND).
DESCRIPTION is a string or a function returning one."
  :type '(repeat (list (string :tag "Key") (choice string function) (function :tag "Command")))
  :group 'herdr-core-keys)

;;; Internal state

(defconst herdr-core--buffer-name "*herdr-core*")
(defconst herdr-core--agents-buffer-name "*herdr-core-agents*")

(defvar herdr-core--terminals (make-hash-table :test 'equal)
  "hc terminal records by address (`t3', `orion/t1'), as hc reports them.")

(defvar herdr-core--snapshot-seen nil
  "Non-nil once the state is a whole snapshot (the stream's, or a poll).")

(defvar herdr-core--process nil "The `hc events' process.")
(defvar herdr-core--pending "" "A partial line of the event stream.")
(defvar herdr-core--refresh-timer nil)
(defvar herdr-core--reconnect-timer nil)
(defvar herdr-core--poll-timer nil)
(defvar herdr-core--error-notified nil)
(defvar herdr-core--machines nil "`hc machine ls' entries, or nil before the first.")

(defvar-local herdr-core--agents nil
  "Agents currently shown in the list buffer, sorted.")

(defvar-local herdr-core--attached-address nil
  "The hc address this buffer shows, or nil.")
(put 'herdr-core--attached-address 'permanent-local t)

(defvar-local herdr-core--observing nil
  "Non-nil when this attach buffer only watches.")
(put 'herdr-core--observing 'permanent-local t)

(defvar-local herdr-core--read-stamp nil
  "The (status turn status_ms) a read buffer was last filled at.")

(defvar-local herdr-core--read-follow nil
  "Non-nil when this read buffer reads again on changes.")

;;; hc records -> agents

(defun herdr-core--status-record (record)
  "RECORD's 7501 status as an alist, or nil.  ls gives a word, events an object."
  (let ((s (alist-get 'status record)))
    (cond ((stringp s) `((state . ,s)))
          ((consp s) s))))

(defun herdr-core--label (record key)
  "RECORD's label KEY (a string), or nil."
  (cdr (assoc-string key (alist-get 'labels record))))

(defun herdr-core--machine (address)
  "The machine of ADDRESS, or \"local\"."
  (if (string-match "\\`\\([^/]+\\)/" address) (match-string 1 address) "local"))

(defun herdr-core--agent (record)
  "RECORD, an hc terminal, as an agent alist."
  (let* ((status (herdr-core--status-record record))
         (address (alist-get 'address record))
         (argv (alist-get 'argv record)))
    `((address . ,address)
      (status . ,(or (alist-get 'state status) "unknown"))
      (blocked-kind . ,(alist-get 'kind status))
      (msg . ,(alist-get 'msg status))
      (name . ,(alist-get 'name record))
      (kind . ,(or (alist-get 'app status) (alist-get 'agent record)
                   (and argv (file-name-nondirectory (car argv)))))
      (session . ,(herdr-core--label record herdr-core-title-label))
      (space . ,(herdr-core--label record herdr-core-space-label))
      (tab . ,(herdr-core--label record herdr-core-tab-label))
      (labels . ,(alist-get 'labels record))
      (cwd . ,(alist-get 'cwd record))
      (state . ,(alist-get 'state record))
      (turn . ,(alist-get 'turn record))
      (machine . ,(herdr-core--machine address))
      (seq . ,(or (alist-get 'status_ms record) (alist-get 'created_ms record) 0)))))

(defun herdr-core--visible-p (record)
  "Non-nil when RECORD is a terminal herdr-core shows."
  (and (or (equal (alist-get 'state record) "live") herdr-core-show-finished)
       (or (not herdr-core-labelled-only) (herdr-core--label record herdr-core-space-label))
       (not (member (herdr-core--machine (alist-get 'address record)) herdr-core-hidden-machines))))

(defun herdr-core--priority (agent)
  "AGENT's attention priority."
  (or (cdr (assoc (alist-get 'status agent) herdr-core-status-priority)) 9))

(defun herdr-core-sort-by-attention (a b)
  "Non-nil when A comes before B: attention, then most recent change, then address."
  (let ((pa (herdr-core--priority a)) (pb (herdr-core--priority b)))
    (cond ((/= pa pb) (< pa pb))
          ((/= (alist-get 'seq a) (alist-get 'seq b))
           (> (alist-get 'seq a) (alist-get 'seq b)))
          (t (string< (alist-get 'address a) (alist-get 'address b))))))

(defun herdr-core-sort-by-recent (a b)
  "Non-nil when A changed more recently than B."
  (if (/= (alist-get 'seq a) (alist-get 'seq b))
      (> (alist-get 'seq a) (alist-get 'seq b))
    (string< (alist-get 'address a) (alist-get 'address b))))

(defun herdr-core-sort-by-name (a b)
  "Non-nil when A's display name sorts before B's."
  (string-collate-lessp (herdr-core--agent-display-name a) (herdr-core--agent-display-name b)
                        nil t))

(defun herdr-core-sort-by-machine (a b)
  "Non-nil when A's machine sorts before B's (this host first), then attention."
  (let ((ma (alist-get 'machine a)) (mb (alist-get 'machine b)))
    (cond ((equal ma mb) (herdr-core-sort-by-attention a b))
          ((equal ma "local") t)
          ((equal mb "local") nil)
          (t (string< ma mb)))))

(defun herdr-core-sort-by-address (a b)
  "Non-nil when A's address sorts before B's."
  (string< (alist-get 'address a) (alist-get 'address b)))

(defun herdr-core--sort-agents (agents)
  "Sort AGENTS by `herdr-core-sort-predicate'."
  (sort agents herdr-core-sort-predicate))

(defun herdr-core-list ()
  "The visible agents, sorted by `herdr-core-sort-predicate'."
  (let (agents)
    (maphash (lambda (_ r)
               (when (herdr-core--visible-p r)
                 (let ((agent (herdr-core--agent r)))
                   (when (run-hook-with-args-until-failure 'herdr-core-filter-functions agent)
                     (push agent agents)))))
             herdr-core--terminals)
    (herdr-core--sort-agents agents)))

(defun herdr-core-count (statuses)
  "How many visible agents have one of STATUSES."
  (seq-count (lambda (a) (member (alist-get 'status a) statuses)) (herdr-core-list)))

(defun herdr-core-blocked-count ()
  "How many visible agents are blocked (waiting for a person)."
  (herdr-core-count '("blocked")))

(defun herdr-core--find-agent (address)
  "The agent at ADDRESS, or nil."
  (when-let* ((record (and address (gethash address herdr-core--terminals))))
    (herdr-core--agent record)))

;;; Groups

(defun herdr-core-group-of (agent &optional by)
  "AGENT's group name under BY (default `herdr-core-group-by'), or nil."
  (let ((by (if by by herdr-core-group-by)))
    (pcase by
      ('nil nil)
      ((or 'machine 'space 'tab 'kind 'status)
       (or (alist-get by agent) (format "(no %s)" by)))
      (`(label ,key) (or (cdr (assoc-string key (alist-get 'labels agent))) (format "(no %s)" key)))
      ((pred functionp) (funcall by agent)))))

(defun herdr-core-groups (agents &optional by)
  "AGENTS in groups under BY: a list of (GROUP . AGENTS), in `herdr-core-group-order'."
  (let (groups)
    (dolist (a agents)
      (let* ((g (herdr-core-group-of a by))
             (cell (assoc g groups)))
        (if cell (setcdr cell (cons a (cdr cell)))
          (push (list g a) groups))))
    (setq groups (mapcar (lambda (c) (cons (car c) (nreverse (cdr c)))) (nreverse groups)))
    (if (eq herdr-core-group-order 'name)
        (sort groups (lambda (x y) (string< (or (car x) "") (or (car y) ""))))
      groups)))

(defun herdr-core--count-phrase (agents)
  "\"N agents, M blocked\" for AGENTS."
  (let ((n (length agents))
        (b (seq-count (lambda (a) (equal (alist-get 'status a) "blocked")) agents)))
    (concat (format "%d agent%s" n (if (= n 1) "" "s"))
            (if (> b 0) (format ", %d blocked" b) ""))))

(defun herdr-core-group-header (group _agents)
  "Default group row text: GROUP, and its machine kind under machine grouping."
  (if (eq herdr-core-group-by 'machine)
      (format "%s · %s" group (herdr-core--machine-kind-of group))
    (format "%s" group)))

;;; The event stream

(defun herdr-core--statuses ()
  "Address -> status of every visible terminal."
  (mapcar (lambda (a) (cons (alist-get 'address a) (alist-get 'status a))) (herdr-core-list)))

(defun herdr-core--reset (terminals)
  "Replace every record with TERMINALS (a snapshot)."
  (clrhash herdr-core--terminals)
  (dolist (r terminals)
    (puthash (alist-get 'address r) r herdr-core--terminals))
  (setq herdr-core--snapshot-seen t))

(defun herdr-core--update (address fn)
  "Set ADDRESS's record to (FN RECORD) when there is one."
  (when-let* ((record (gethash address herdr-core--terminals)))
    (puthash address (funcall fn (copy-alist record)) herdr-core--terminals)))

(defun herdr-core--merge (record data &optional skip)
  "RECORD with DATA's keys set, but not those in SKIP."
  (dolist (cell data record)
    (unless (memq (car cell) skip)
      (setf (alist-get (car cell) record) (cdr cell)))))

(defvar herdr-core--machine-names-cache nil
  "Alist of machine name -> `hc machine ls' entry.")

(defun herdr-core--apply-event (params)
  "Apply one hc event's PARAMS (type, terminal, data)."
  (let ((type (alist-get 'type params))
        (address (alist-get 'terminal params))
        (data (alist-get 'data params)))
    (when address
      (pcase type
        ("created" (puthash address data herdr-core--terminals)
         (unless (or (equal (herdr-core--machine address) "local")
                     (assoc (herdr-core--machine address) herdr-core--machine-names-cache))
           (herdr-core--refresh-machines)))
        ("removed" (remhash address herdr-core--terminals))
        ("status" (herdr-core--update address (lambda (r) (herdr-core--merge r data '(address state)))))
        ("finished" (herdr-core--update address (lambda (r) (setf (alist-get 'state r) "finished")
                                                  (herdr-core--merge r data '(address state)))))
        ;; The foreground process (its own name, argv, cwd, pid) is the record's
        ;; `foreground', not the terminal's: merged flat, its name replaced the
        ;; terminal's --name.
        ("foreground" (herdr-core--update address (lambda (r) (setf (alist-get 'foreground r) data) r)))
        ;; turn (its state is the turn's, not the terminal's), labels, keep, ...
        (_ (herdr-core--update address (lambda (r) (herdr-core--merge r data '(state address)))))))))

(defun herdr-core--run-status-changes (before)
  "Run `herdr-core-status-change-functions' for agents whose status left BEFORE's."
  (when before
    (dolist (agent (herdr-core-list))
      (when-let* ((was (assoc (alist-get 'address agent) before)))
        (unless (equal (cdr was) (alist-get 'status agent))
          (run-hook-with-args 'herdr-core-status-change-functions
                              agent (cdr was) (alist-get 'status agent)))))))

(defun herdr-core-apply (message)
  "Apply one parsed line MESSAGE of `hc events --json'.
Runs `herdr-core-status-change-functions' for each agent whose status changed."
  (let ((method (alist-get 'method message))
        (params (alist-get 'params message))
        (before (and herdr-core--snapshot-seen (herdr-core--statuses))))
    (cond
     ((and (null method) (assq 'terminals message))
      (herdr-core--reset (alist-get 'terminals message))
      (herdr-core--stop-poll))
     ((equal method "missed") (herdr-core--reset (alist-get 'terminals params)))
     ((equal method "event") (herdr-core--apply-event params))
     ;; A machine left (its terminals' removed events follow).
     ((assq 'removed message) (herdr-core--refresh-machines)))
    (herdr-core--run-status-changes before)
    (run-hook-with-args 'herdr-core-event-functions message)))

(defun herdr-core--parse (line)
  "LINE as an alist, or nil when it is not a JSON object."
  (condition-case nil
      (let ((v (json-parse-string line :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil)))
        (and (consp v) v))
    (error nil)))

(defun herdr-core--filter (_process output)
  "Apply each whole line of OUTPUT, keeping a partial line for later."
  (let ((lines (split-string (concat herdr-core--pending output) "\n")))
    (setq herdr-core--pending (car (last lines)))
    (dolist (line (butlast lines))
      (when-let* ((message (herdr-core--parse line)))
        (herdr-core-apply message))))
  (when herdr-core--error-notified
    (setq herdr-core--error-notified nil)
    (message "herdr-core: hc reachable again"))
  (herdr-core--schedule-refresh))

(defun herdr-core--wanted-p ()
  "Non-nil while something shows the state: the mode or the list buffer."
  (or (bound-and-true-p herdr-core-mode) (get-buffer herdr-core--agents-buffer-name)))

(defun herdr-core--sentinel (process _event)
  "Start the stream again later when PROCESS ends and it is still wanted."
  (when (and (eq process herdr-core--process) (not (process-live-p process)))
    (setq herdr-core--process nil herdr-core--snapshot-seen nil)
    (unless herdr-core--error-notified
      (setq herdr-core--error-notified t)
      (message "herdr-core: hc events ended, will keep trying"))
    (when (and (eq herdr-core-refresh-policy 'events+poll) (herdr-core--wanted-p))
      (herdr-core--start-poll))
    (when (and (herdr-core--wanted-p) (not herdr-core--reconnect-timer))
      (setq herdr-core--reconnect-timer
            (run-at-time herdr-core-reconnect-delay nil
                         (lambda ()
                           (setq herdr-core--reconnect-timer nil)
                           (when (herdr-core--wanted-p) (herdr-core-start))))))))

(defun herdr-core--command (&rest args)
  "The hc command line for ARGS."
  (append (list herdr-core-executable) herdr-core-executable-args args))

(defun herdr-core--environment ()
  "`process-environment' for hc processes."
  (append herdr-core-process-environment process-environment))

(defun herdr-core-start ()
  "Start following hc (the stream, or polling), unless it is already followed."
  (if (eq herdr-core-refresh-policy 'poll)
      (herdr-core--start-poll)
    (unless (process-live-p herdr-core--process)
      (setq herdr-core--pending "")
      (let ((process-environment (herdr-core--environment)))
        (setq herdr-core--process
              (make-process
               :name "herdr-core-events"
               :command (herdr-core--command "events" "--json")
               :connection-type 'pipe
               :coding 'utf-8
               :noquery t
               :stderr (get-buffer-create " *herdr-core-events-stderr*")
               :filter #'herdr-core--filter
               :sentinel #'herdr-core--sentinel)))))
  (unless herdr-core--machines (herdr-core--refresh-machines)))

(defun herdr-core-stop ()
  "Stop following hc."
  (when herdr-core--reconnect-timer
    (cancel-timer herdr-core--reconnect-timer)
    (setq herdr-core--reconnect-timer nil))
  (herdr-core--stop-poll)
  (let ((process herdr-core--process))
    (setq herdr-core--process nil herdr-core--snapshot-seen nil)
    (when (process-live-p process) (delete-process process))))

(defun herdr-core-reconnect ()
  "Start the event stream again (a fresh snapshot)."
  (interactive)
  (herdr-core-stop)
  (herdr-core-start))

;;; Polling

(defun herdr-core--stop-poll ()
  "Stop polling."
  (when herdr-core--poll-timer
    (cancel-timer herdr-core--poll-timer)
    (setq herdr-core--poll-timer nil)))

(defun herdr-core--start-poll ()
  "Poll `hc ls' now and every `herdr-core-poll-interval' seconds."
  (unless herdr-core--poll-timer
    (setq herdr-core--poll-timer
          (run-at-time 0 herdr-core-poll-interval #'herdr-core-poll))))

(defun herdr-core-poll ()
  "Replace the state with `hc ls' (and `hc get') once; nil when hc is down."
  (interactive)
  (when-let* ((terminals (herdr-core--fetch-ls t)))
    (let ((before (and herdr-core--snapshot-seen (herdr-core--statuses))))
      (herdr-core--reset (if (eq terminals 'empty) nil terminals))
      (herdr-core--run-status-changes before)
      (herdr-core--schedule-refresh)
      t)))

;;; Calling hc

(defun herdr-core--call-sync (&rest args)
  "Run hc with ARGS; a cons (EXIT-CODE . OUTPUT), EXIT-CODE nil when hc cannot run."
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (process-environment (herdr-core--environment))
           (command (apply #'herdr-core--command args))
           (code (condition-case nil
                     (apply #'call-process (car command) nil (list t nil) nil (cdr command))
                  (error nil))))
      (cons code (buffer-string)))))

(defun herdr-core--call-json-sync (&rest args)
  "Run hc with ARGS, return its parsed JSON object or nil."
  (let ((got (apply #'herdr-core--call-sync args)))
    (and (eql (car got) 0) (herdr-core--parse (string-trim (cdr got))))))

(defun herdr-core--run (program args on-done)
  "Run PROGRAM with ARGS; call ON-DONE with its exit code and output."
  (let ((buffer (generate-new-buffer " *herdr-core-run*"))
        (process-environment (herdr-core--environment)))
    (make-process
     :name "herdr-core-run"
     :buffer buffer
     :command (cons program args)
     :connection-type 'pipe
     :coding 'utf-8
     :noquery t
     :sentinel (lambda (process _event)
                 (unless (process-live-p process)
                   (let ((out (with-current-buffer buffer (buffer-string))))
                     (kill-buffer buffer)
                     (funcall on-done (process-exit-status process) out)))))))

(defun herdr-core--hc (args on-done)
  "Run hc with ARGS asynchronously; call ON-DONE with its exit code and output."
  (let ((command (apply #'herdr-core--command args)))
    (herdr-core--run (car command) (cdr command) on-done)))

(defun herdr-core--start-server ()
  "Start this host's hc core and wait until it answers."
  (message "herdr-core: starting hc core...")
  (herdr-core--call-sync "server" "start" "--json")
  (or (herdr-core--call-json-sync "ls" "--json")
      (user-error "herdr-core: hc core did not come up")))

(defun herdr-core--fetch-ls (&optional quiet)
  "The terminals of `hc ls --json', each completed by `hc get --json'.
Offers to start the hc core when it is down (`herdr-core-start-core'); with
QUIET, returns nil instead, and `empty' for no terminals."
  (let ((ls (or (herdr-core--call-json-sync "ls" "--json")
                (cond (quiet nil)
                      ((or (eq herdr-core-start-core 'always)
                           (and (eq herdr-core-start-core 'ask)
                                (y-or-n-p "The hc core is not running.  Start it? ")))
                       (herdr-core--start-server))
                      (t (user-error "hc core not running"))))))
    (when ls
      (or (mapcar (lambda (summary)
                    (or (and (or (not quiet) herdr-core-poll-details)
                             (herdr-core--call-json-sync "get" "--json" (alist-get 'address summary)))
                        summary))
                  (alist-get 'terminals ls))
          (and quiet 'empty)))))

(defun herdr-core--fetch-agents-sync ()
  "The visible agents, sorted: from the event stream when it is up (waiting
briefly for its snapshot), else from `hc ls'."
  (herdr-core-start)
  (let ((deadline (+ (float-time) herdr-core-snapshot-timeout)))
    (while (and (not herdr-core--snapshot-seen) (process-live-p herdr-core--process)
                (< (float-time) deadline))
      (accept-process-output herdr-core--process 0.05)))
  (unless herdr-core--snapshot-seen
    (herdr-core--reset (herdr-core--fetch-ls))
    ;; Not the stream's snapshot: the stream still resets it when it arrives.
    (unless (eq herdr-core-refresh-policy 'poll)
      (setq herdr-core--snapshot-seen nil)))
  (herdr-core-list))

;;; Machines

(defun herdr-core--set-machines (machines)
  "Remember MACHINES, `hc machine ls' entries."
  (setq herdr-core--machines (or machines 'none)
        herdr-core--machine-names-cache
        (mapcar (lambda (m) (cons (alist-get 'name m) m)) machines)))

(defun herdr-core--refresh-machines ()
  "Ask `hc machine ls' again, in the background."
  (unless (eq herdr-core--machines 'pending)
    (let ((was herdr-core--machines))
      (setq herdr-core--machines 'pending)
      (condition-case nil
          (herdr-core--hc '("machine" "ls" "--json")
                          (lambda (code out)
                            (if (zerop code)
                                (herdr-core--set-machines
                                 (alist-get 'machines (herdr-core--parse (string-trim out))))
                              (setq herdr-core--machines (if (eq was 'pending) nil was)))))
        (error (setq herdr-core--machines nil))))))

(defun herdr-core-machine-kind (name record)
  "The kind of machine NAME (its `hc machine ls' RECORD or nil):
local, pod (`herdr-core-pod-machine-regexp'), command (--via) or ssh."
  (cond ((equal name "local") "local")
        ((string-match-p herdr-core-pod-machine-regexp name) "pod")
        ((and record (alist-get 'via record)) "command")
        (t "ssh")))

(defun herdr-core--machine-kind-of (name)
  "The kind of machine NAME."
  (funcall herdr-core-machine-kind-function name (cdr (assoc name herdr-core--machine-names-cache))))

(defun herdr-core-machines ()
  "Every machine: this host, those `hc machine ls' lists, those agents are on.
Each an alist (name kind state agents)."
  (let* ((agents (herdr-core-list))
         (names (delete-dups (append '("local") (mapcar #'car herdr-core--machine-names-cache)
                                     (mapcar (lambda (a) (alist-get 'machine a)) agents)))))
    (mapcar (lambda (name)
              (let ((record (cdr (assoc name herdr-core--machine-names-cache))))
                `((name . ,name)
                  (kind . ,(herdr-core--machine-kind-of name))
                  (state . ,(if (equal name "local") "connected"
                              (or (alist-get 'state record) "connected")))
                  (agents . ,(seq-filter (lambda (a) (equal (alist-get 'machine a) name)) agents)))))
            (seq-remove (lambda (n) (member n herdr-core-hidden-machines)) names))))

;;; Names

(defun herdr-core--project-name (dir)
  "DIR's project.el name, else its directory name."
  (or (let ((default-directory dir))
        (when-let* ((proj (project-current nil)))
          (project-name proj)))
      (file-name-nondirectory (directory-file-name dir))))

(defun herdr-core-default-name (agent)
  "AGENT's first non-empty `herdr-core-name-sources'."
  (seq-find (lambda (s) (and (stringp s) (not (string-empty-p s))))
            (append (mapcar (lambda (src)
                              (if (stringp src)
                                  (cdr (assoc-string src (alist-get 'labels agent)))
                                (alist-get src agent)))
                            herdr-core-name-sources)
                    (list (alist-get 'address agent)))))

(defun herdr-core--agent-display-name (agent)
  "AGENT's display name (`herdr-core-name-function')."
  (funcall herdr-core-name-function agent))

(defun herdr-core-title (address)
  "The session name of the terminal at ADDRESS: its `herdr-core-title-label'
\(the session-naming layer's projection), else ADDRESS."
  (or (when-let* ((record (gethash address herdr-core--terminals)))
        (herdr-core--label record herdr-core-title-label))
      address))

(defun herdr-core-default-buffer-name (kind title)
  "`*hc: TITLE*' (KIND nil), `*hc observe: TITLE*' or `*hc read: TITLE*'."
  (format "*hc%s: %s*" (if kind (concat " " kind) "") title))

(defun herdr-core--base-buffer-name (address kind)
  "The base buffer name for ADDRESS and KIND."
  (funcall herdr-core-buffer-name-function kind (herdr-core-title address)))

(defun herdr-core-buffer-name (address &optional kind)
  "The buffer name for ADDRESS (KIND: nil, \"observe\" or \"read\").
`*hc: TITLE*', as the session-naming layer's emacs_buffer projection; a
name another address's buffer holds gets a <N> suffix."
  (let* ((name (herdr-core--base-buffer-name address kind))
         (taken (lambda (b) (and b (not (member (buffer-local-value 'herdr-core--attached-address b)
                                                (list nil address)))))))
    (if (not (funcall taken (get-buffer name)))
        name
      (let ((n 2) candidate)
        (while (funcall taken (get-buffer (setq candidate (format "%s<%d>" name n))))
          (setq n (1+ n)))
        candidate))))

;;; Picking

(defun herdr-core-format-candidate (agent)
  "Format AGENT as \"[status] session (kind) · machine · cwd · address\"."
  (let ((kind (alist-get 'kind agent))
        (blocked (alist-get 'blocked-kind agent))
        (cwd (alist-get 'cwd agent)))
    (string-join
     (delq nil (list (format "[%s%s] %s%s" (alist-get 'status agent)
                             (if blocked (concat ":" blocked) "")
                             (herdr-core--agent-display-name agent)
                             (if kind (format " (%s)" kind) ""))
                     (alist-get 'machine agent)
                     (and cwd (abbreviate-file-name cwd))
                     (alist-get 'address agent)))
     " · ")))

(defun herdr-core--format-candidate (agent)
  "AGENT's picker candidate (`herdr-core-candidate-function')."
  (funcall herdr-core-candidate-function agent))

(defun herdr-core--picker-group-by ()
  "What the picker groups by."
  (if (eq herdr-core-picker-group-by 'list) herdr-core-group-by herdr-core-picker-group-by))

(defun herdr-core--completing-read (prompt candidates &optional group-fn require-match)
  "Read one of CANDIDATES (an alist keyed by string) with PROMPT, in their order.
GROUP-FN is a function of a candidate string returning its group.  With
REQUIRE-MATCH, only a candidate."
  (funcall herdr-core-completing-read-function prompt
           (lambda (string pred action)
             (if (eq action 'metadata)
                 `(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity)
                            ,@(and group-fn
                                   `((group-function
                                      . ,(lambda (cand transform)
                                           (if transform cand (funcall group-fn cand)))))))
               (complete-with-action action candidates string pred)))
           nil require-match))

(defun herdr-core--pick-from (agents prompt)
  "Pick one of AGENTS with PROMPT; its alist."
  (unless agents
    (user-error "No hc agents"))
  (let* ((by (herdr-core--picker-group-by))
         (candidates (mapcar (lambda (a) (cons (herdr-core--format-candidate a) a))
                             (if by (apply #'append (mapcar #'cdr (herdr-core-groups agents by)))
                               agents)))
         (group-fn (and by (lambda (c) (herdr-core-group-of (cdr (assoc c candidates)) by)))))
    (cdr (assoc (herdr-core--completing-read prompt candidates group-fn t) candidates))))

(defun herdr-core--pick-agent (&optional prompt)
  "Pick an agent via `completing-read', return its alist."
  (herdr-core--pick-from (herdr-core--fetch-agents-sync) (or prompt "Agent: ")))

;;; Terminal buffers

(defun herdr-core--buffer-process (buffer)
  "BUFFER's terminal process: Ghostel's, else the buffer's own."
  (or (and (boundp 'ghostel--process) (buffer-local-value 'ghostel--process buffer))
      (get-buffer-process buffer)))

(defun herdr-core--buffer-live-p (buffer)
  "Non-nil when BUFFER is live with a live terminal process."
  (and (buffer-live-p buffer) (process-live-p (herdr-core--buffer-process buffer))))

(defun herdr-core--display-action (context)
  "The `display-buffer' action for CONTEXT (`herdr-core-display-actions')."
  (alist-get context herdr-core-display-actions))

(defun herdr-core-terminal (name program args &optional action)
  "Run PROGRAM with ARGS in a new terminal buffer NAME (Ghostel, else term).
Displays it with `display-buffer' ACTION (same window by default) first,
so the terminal starts at its window's size."
  (let ((buffer (generate-new-buffer name))
        (action (or action '((display-buffer-same-window)))))
    (if (require 'ghostel nil t)
        (progn
          (with-current-buffer buffer
            (ghostel-mode)
            ;; A side-by-side popup halves the PTY width.
            (setq-local split-width-threshold nil))
          (pop-to-buffer buffer action)
          (ghostel-exec buffer program args `((kind . herdr) (command . ,(cons program args)))))
      (require 'term)
      (pop-to-buffer buffer action)
      (with-current-buffer buffer
        (term-mode)
        (term-exec buffer name program nil args)
        (term-char-mode)))
    buffer))

;;;###autoload
(defun herdr-core ()
  "Pop to *herdr-core*: hc's own terminal picker, started if needed."
  (interactive)
  (let ((buffer (get-buffer herdr-core--buffer-name)))
    (if (herdr-core--buffer-live-p buffer)
        (pop-to-buffer buffer (herdr-core--display-action 'picker))
      (when buffer (kill-buffer buffer))
      (let ((command (herdr-core--command)))
        (funcall herdr-core-terminal-function herdr-core--buffer-name (car command) (cdr command)
                 (herdr-core--display-action 'picker))))))

;;;###autoload
(defun herdr-core-hide ()
  "Hide the *herdr-core* window, restoring what it displaced."
  (interactive)
  (if-let* ((win (get-buffer-window herdr-core--buffer-name)))
      (quit-restore-window win 'bury)
    (user-error "*herdr-core* is not displayed")))

;;;###autoload
(defun herdr-core-toggle ()
  "Pop to *herdr-core*, or hide it when its window is already selected."
  (interactive)
  (if (eq (current-buffer) (get-buffer herdr-core--buffer-name))
      (herdr-core-hide)
    (herdr-core)))

;;; Attach buffer header line

(defun herdr-core--status-face (status)
  "Face for an agent STATUS string."
  (let ((face (intern (format "herdr-core-status-%s" status))))
    (if (facep face) face 'herdr-core-status-unknown)))

(defun herdr-core--glyph (status)
  "STATUS's glyph."
  (or (cdr (assoc status herdr-core-status-glyphs)) "?"))

(defun herdr-core-header-label (agent)
  "Short label for AGENT: status glyph and its truncated herdr.tab (what it
is doing; titles share their space prefix), else its display name."
  (format " %s %s "
          (herdr-core--glyph (alist-get 'status agent))
          (truncate-string-to-width (or (alist-get 'tab agent) (herdr-core--agent-display-name agent))
                                    herdr-core-header-label-width nil nil "…")))

(defun herdr-core--header-click-command (agent)
  "Command switching to AGENT in the clicked window."
  (lambda (event)
    (interactive "e")
    (select-window (posn-window (event-start event)))
    (herdr-core--attach-agent agent)))

(defun herdr-core--header-segment (agent current)
  "Clickable header-line segment for AGENT; the CURRENT address is highlighted."
  (let ((map (make-sparse-keymap))
        (status (alist-get 'status agent)))
    (define-key map [header-line mouse-1] (herdr-core--header-click-command agent))
    (propertize (funcall herdr-core-header-label-function agent)
                'face (if (equal (alist-get 'address agent) current)
                          `(herdr-core-header-current ,(herdr-core--status-face status))
                        (herdr-core--status-face status))
                ;; A fresh list per segment: adjacent text sharing an `eq'
                ;; mouse-face highlights as one block.
                'mouse-face (list 'highlight)
                'help-echo (herdr-core--format-candidate agent)
                'pointer 'hand
                'local-map map)))

(defun herdr-core--header-overflow (dropped)
  "Header-line \"+N\" marker for DROPPED hidden agents.
Clicking it opens the agent list."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (select-window (posn-window (event-start event)))
                  (herdr-core-agents)))
    (propertize (format " +%d" dropped)
                'face 'shadow
                'mouse-face (list 'highlight)
                'help-echo (format "%d more agents; click for the agent list" dropped)
                'pointer 'hand
                'local-map map)))

(defun herdr-core--header-line ()
  "Header-line string: as many agents as fit the window, then a \"+N\" marker."
  (let ((width (window-width)) (row "") (dropped 0))
    (dolist (agent (herdr-core-list))
      (let ((seg (herdr-core--header-segment agent herdr-core--attached-address)))
        (cond
         ((string-empty-p row) (setq row seg))
         ;; Keep 4 columns free for the overflow marker.
         ((<= (+ (string-width row) 1 (string-width seg) 4) width)
          (setq row (concat row " " seg)))
         (t (setq dropped (1+ dropped))))))
    (if (> dropped 0)
        (concat row (herdr-core--header-overflow dropped))
      row)))

(defvar herdr-core--header-line-format '(:eval (herdr-core--header-line))
  "Value installed as `header-line-format' in attach buffers.")

;;; Attach (one buffer per agent)

(defun herdr-core--read-buffer-p (buf)
  "Non-nil when BUF is a read buffer."
  (eq (buffer-local-value 'major-mode buf) 'herdr-core-read-mode))

(defun herdr-core--find-attach-buffer (address &optional observe)
  "Live buffer attached to ADDRESS (watching only with OBSERVE), or nil.
Dead ones are killed."
  (seq-find (lambda (buf)
              (when (and (equal (buffer-local-value 'herdr-core--attached-address buf) address)
                         (eq (buffer-local-value 'herdr-core--observing buf) (and observe t))
                         (not (herdr-core--read-buffer-p buf)))
                (or (herdr-core--buffer-live-p buf)
                    (progn (let ((kill-buffer-query-functions nil)) (kill-buffer buf))
                           nil))))
            (buffer-list)))

(defun herdr-core--attach-args (address observe)
  "hc arguments attaching to ADDRESS (watching only with OBSERVE)."
  (append '("attach") (and observe '("--observe"))
          (and (> herdr-core-attach-history 0)
               (list "--history" (number-to-string herdr-core-attach-history)))
          (list address)))

(defun herdr-core--create-attach-buffer (address &optional observe action)
  "Attach to ADDRESS in a new buffer (watching only with OBSERVE) and show it.
ACTION is the `display-buffer' action, same window by default."
  (let* ((name (herdr-core-buffer-name address (and observe "observe")))
         (command (apply #'herdr-core--command (herdr-core--attach-args address observe)))
         (buf (funcall herdr-core-terminal-function name (car command) (cdr command) action)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq herdr-core--attached-address address
              herdr-core--observing (and observe t))
        (when herdr-core-attach-header-line
          (setq header-line-format herdr-core--header-line-format))
        (run-hooks 'herdr-core-attach-hook)))
    buf))

(defun herdr-core--attach-agent (agent &optional reattach action observe)
  "Pop to the buffer attached to AGENT, creating it if needed.
With REATTACH, drop an existing buffer and attach afresh.  ACTION is the
`display-buffer' action, else the `attach' (or `observe') one.  With
OBSERVE, watch only."
  (let* ((address (alist-get 'address agent))
         (action (or action (herdr-core--display-action (if observe 'observe 'attach))))
         (buf (herdr-core--find-attach-buffer address observe)))
    (when (and buf reattach)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (setq buf nil))
    (if buf
        (progn (pop-to-buffer buf (or action '((display-buffer-same-window)))) buf)
      (herdr-core--create-attach-buffer address observe action))))

(defun herdr-core--buffer-kind (buf)
  "BUF's naming kind: nil, \"observe\" or \"read\"."
  (cond ((buffer-local-value 'herdr-core--observing buf) "observe")
        ((herdr-core--read-buffer-p buf) "read")))

(defun herdr-core--sync-attach-buffer-names ()
  "Rename attach and read buffers whose agent's session title changed."
  (dolist (buf (buffer-list))
    (when-let* ((address (buffer-local-value 'herdr-core--attached-address buf)))
      (let* ((kind (herdr-core--buffer-kind buf))
             (prefix (herdr-core--base-buffer-name address kind)))
        (unless (string-prefix-p prefix (buffer-name buf))
          (with-current-buffer buf
            (rename-buffer (herdr-core-buffer-name address kind) t)))))))

(defun herdr-core--attach-buffers ()
  "Live attach buffers, most recently selected first."
  (seq-filter (lambda (buf)
                (and (buffer-local-value 'herdr-core--attached-address buf)
                     (not (herdr-core--read-buffer-p buf))
                     (herdr-core--buffer-live-p buf)))
              (buffer-list)))

(defun herdr-core-attach-in-window (window agent &optional observe)
  "Attach to AGENT in WINDOW (watching only with OBSERVE); its buffer."
  (with-selected-window window
    (herdr-core--attach-agent agent nil '((display-buffer-same-window)) observe)))

;;; Layouts

(defun herdr-core--split (window n direction)
  "Split WINDOW into N windows along DIRECTION (`right' or `below'); the list."
  (let ((windows (list window)))
    (dotimes (_ (1- n))
      (push (split-window (car windows) nil direction) windows))
    (nreverse windows)))

(defun herdr-core-layout-windows (n &optional layout)
  "Make N windows in LAYOUT (default `herdr-core-layout'); return them in order."
  (let* ((layout (or layout herdr-core-layout))
         (root (progn (when herdr-core-layout-delete-other-windows (delete-other-windows))
                      (selected-window)))
         (windows
          (pcase layout
            ('rows (herdr-core--split root n 'below))
            ('grid
             (let* ((cols (ceiling (sqrt n)))
                    (rows (ceiling n cols))
                    (columns (herdr-core--split root cols 'right))
                    (all nil) (left n))
               (dolist (col columns)
                 (let ((k (max 1 (min left rows))))
                   (when (> left 0)
                     (setq all (append all (herdr-core--split col k 'below))
                           left (- left k)))))
               (seq-take all n)))
            (_ (herdr-core--split root n 'right)))))
    (balance-windows (or (window-parent root) root))
    windows))

(defun herdr-core-show-several (agents &optional observe)
  "Show AGENTS side by side in `herdr-core-layout' (at most `herdr-core-layout-max').
With OBSERVE, watch only.  Returns their buffers."
  (let ((agents (seq-take agents (max 1 herdr-core-layout-max))))
    (unless agents (user-error "No agents to show"))
    (if (functionp herdr-core-layout)
        (funcall herdr-core-layout agents)
      (let* ((windows (herdr-core-layout-windows (length agents)))
             (buffers (seq-mapn (lambda (w a) (herdr-core-attach-in-window w a observe))
                                windows agents)))
        (select-window (car windows))
        buffers))))

;;;###autoload
(defun herdr-core-attach-several (&optional observe)
  "Pick agents one by one (empty input ends) and show them side by side.
With prefix OBSERVE, watch only."
  (interactive "P")
  (let ((pool (herdr-core--fetch-agents-sync)) picked)
    (catch 'done
      (while (and pool (< (length picked) herdr-core-layout-max))
        (let* ((candidates (mapcar (lambda (a) (cons (herdr-core--format-candidate a) a)) pool))
               (choice (herdr-core--completing-read
                        (format "Agent %d (empty: done): " (1+ (length picked))) candidates))
               (agent (cdr (assoc choice candidates))))
          (unless agent (throw 'done nil))
          (push agent picked)
          (setq pool (delq agent pool)))))
    (herdr-core-show-several (nreverse picked) observe)))

;;;###autoload
(defun herdr-core-attach-group (&optional by)
  "Pick a group (under `herdr-core-group-by', else by space) and show its agents
side by side.  With a prefix argument, choose BY first."
  (interactive
   (list (and current-prefix-arg
              (intern (completing-read "Group by: " '("machine" "space" "tab" "kind" "status")
                                       nil t)))))
  (let ((group (herdr-core--pick-group (or by herdr-core-group-by 'space) "Group: ")))
    (herdr-core-show-several (cdr group))))

;;; Switching

(defun herdr-core--current-address ()
  "The agent address of the current buffer (attach, read or list row), or nil."
  (or herdr-core--attached-address
      (and (derived-mode-p 'herdr-core-agents-mode) (herdr-core--list-address))))

;;;###autoload
(defun herdr-core-last-agent ()
  "Pop to the most recently used agent buffer other than the current one."
  (interactive)
  (pop-to-buffer (or (seq-find (lambda (b) (not (eq b (current-buffer))))
                               (herdr-core--attach-buffers))
                     (user-error "No other agent buffer"))
                 (herdr-core--display-action 'switch)))

;;;###autoload
(defun herdr-core-show-agent (address)
  "Raise Emacs and show the agent at ADDRESS in another window.
Run from outside Emacs (a notification click) via emacsclient."
  (interactive (list (alist-get 'address (herdr-core--pick-agent "Show agent: "))))
  (select-frame-set-input-focus (selected-frame))
  (herdr-core--attach-agent (or (herdr-core--find-agent address) `((address . ,address)))
                            nil (herdr-core--display-action 'show)))

;;;###autoload
(defun herdr-core-switch-agent ()
  "Pick an agent and show its buffer (attaching when there is none)."
  (interactive)
  (herdr-core--attach-agent (herdr-core--pick-agent "Switch to agent: ")))

(defun herdr-core--cycle-pool (agents current)
  "AGENTS within `herdr-core-cycle-scope' of the CURRENT agent."
  (if (or (eq herdr-core-cycle-scope 'all) (null current))
      agents
    (let ((key herdr-core-cycle-scope))
      (seq-filter (lambda (a) (equal (alist-get key a) (alist-get key current))) agents))))

(defun herdr-core--next (agents address step &optional pred)
  "The agent STEP (1 or -1) from ADDRESS in AGENTS satisfying PRED, wrapping."
  (let* ((pool (if pred (seq-filter pred agents) agents))
         (n (length agents))
         (i (seq-position agents address (lambda (a addr) (equal (alist-get 'address a) addr)))))
    (cond
     ((null pool) nil)
     ((null i) (if (> step 0) (car pool) (car (last pool))))
     (t (let ((k 1) found)
          (while (and (not found) (<= k n))
            (let ((a (nth (mod (+ i (* k step)) n) agents)))
              (when (or (null pred) (funcall pred a)) (setq found a)))
            (setq k (1+ k)))
          found)))))

(defun herdr-core--cycle (step &optional pred what)
  "Show the agent STEP away from the current one satisfying PRED; WHAT names it."
  (let* ((agents (herdr-core--fetch-agents-sync))
         (address (herdr-core--current-address))
         (pool (herdr-core--cycle-pool agents (herdr-core--find-agent address)))
         (next (herdr-core--next pool address step pred)))
    (unless next (user-error "No %s" (or what "other agent")))
    (herdr-core--attach-agent next nil (herdr-core--display-action 'switch))))

;;;###autoload
(defun herdr-core-next-agent ()
  "Show the next agent in list order (within `herdr-core-cycle-scope')."
  (interactive)
  (herdr-core--cycle 1))

;;;###autoload
(defun herdr-core-previous-agent ()
  "Show the previous agent in list order (within `herdr-core-cycle-scope')."
  (interactive)
  (herdr-core--cycle -1))

;;;###autoload
(defun herdr-core-goto-blocked ()
  "Show the next blocked agent: the first one, or the one after the current."
  (interactive)
  (let ((herdr-core-cycle-scope 'all))
    (herdr-core--cycle 1 (lambda (a) (equal (alist-get 'status a) "blocked")) "blocked agents")))

;;;###autoload
(defun herdr-core-next-attention ()
  "Show the next agent in `herdr-core-attention-statuses'."
  (interactive)
  (let ((herdr-core-cycle-scope 'all))
    (herdr-core--cycle 1 (lambda (a) (member (alist-get 'status a) herdr-core-attention-statuses))
                       "agents needing attention")))

(defun herdr-core--pick-in (agents prompt)
  "Show the only one of AGENTS, else pick one with PROMPT."
  (herdr-core--attach-agent (if (cdr agents) (herdr-core--pick-from agents prompt) (car agents))
                            nil (herdr-core--display-action 'switch)))

(defun herdr-core--pick-group (by prompt)
  "Pick a group of agents under BY with PROMPT; (GROUP . AGENTS)."
  (let* ((groups (seq-filter #'car (herdr-core-groups (herdr-core--fetch-agents-sync) by)))
         (candidates (mapcar (lambda (g)
                               (cons (format "%s · %s" (car g) (herdr-core--count-phrase (cdr g))) g))
                             groups)))
    (unless candidates (user-error "No agents"))
    (cdr (assoc (herdr-core--completing-read prompt candidates nil t) candidates))))

(defun herdr-core--machines-by-attention (machines)
  "MACHINES ordered by their first agent's place in the list."
  (let ((order (mapcar (lambda (a) (alist-get 'machine a)) (herdr-core-list))))
    (sort machines (lambda (a b) (< (or (seq-position order (alist-get 'name a)) 999)
                                    (or (seq-position order (alist-get 'name b)) 999))))))

;;;###autoload
(defun herdr-core-switch-machine (machine)
  "Pick a MACHINE (this host, ssh and pod machines), then one of its agents."
  (interactive
   (let* ((_ (herdr-core--fetch-agents-sync))
          (machines (seq-filter (lambda (m) (alist-get 'agents m)) (herdr-core-machines)))
          (candidates (mapcar (lambda (m)
                                (cons (format "%s (%s, %s) · %s" (alist-get 'name m) (alist-get 'kind m)
                                              (alist-get 'state m)
                                              (herdr-core--count-phrase (alist-get 'agents m)))
                                      m))
                              (herdr-core--machines-by-attention machines))))
     (unless candidates (user-error "No machine has agents"))
     (list (alist-get 'name (cdr (assoc (herdr-core--completing-read "Machine: " candidates nil t)
                                        candidates))))))
  (herdr-core--pick-in (seq-filter (lambda (a) (equal (alist-get 'machine a) machine))
                                   (herdr-core--fetch-agents-sync))
                       (format "Agent on %s: " machine)))

;;;###autoload
(defun herdr-core-switch-space ()
  "Pick a space (`herdr-core-space-label'), then one of its agents."
  (interactive)
  (let ((group (herdr-core--pick-group 'space "Space: ")))
    (herdr-core--pick-in (cdr group) (format "Agent in %s: " (car group)))))

(defun herdr-core--space-tab (agent)
  "AGENT's \"space / tab\"."
  (format "%s / %s" (or (alist-get 'space agent) "-")
          (or (alist-get 'tab agent) (herdr-core--agent-display-name agent))))

;;;###autoload
(defun herdr-core-switch-tab ()
  "Pick a tab (space / tab), then its agent."
  (interactive)
  (let ((group (herdr-core--pick-group #'herdr-core--space-tab "Tab: ")))
    (herdr-core--pick-in (cdr group) (format "Agent in %s: " (car group)))))

;;;###autoload
(defun herdr-core-attach-terminal (address &optional observe)
  "Attach to any terminal at ADDRESS: finished, unlabelled or hidden ones too,
or an address typed in.  With prefix OBSERVE, watch only."
  (interactive
   (progn
     (herdr-core--fetch-agents-sync)
     (let (candidates)
       (maphash (lambda (addr r)
                  (push (cons (format "%s · %s · %s" addr (alist-get 'state r)
                                      (herdr-core--agent-display-name (herdr-core--agent r)))
                              addr)
                        candidates))
                herdr-core--terminals)
       (setq candidates (sort candidates (lambda (a b) (string< (cdr a) (cdr b)))))
       (let ((choice (herdr-core--completing-read "Attach to terminal: " candidates)))
         (list (or (cdr (assoc choice candidates)) choice) current-prefix-arg)))))
  (when (string-empty-p address) (user-error "No terminal"))
  (herdr-core--attach-agent (or (herdr-core--find-agent address) `((address . ,address)))
                            nil nil observe))

;;;###autoload
(defun herdr-core-attach-agent (&optional reattach)
  "Pick an agent and attach to it in its own buffer (ctrl+\\ detaches).
With prefix REATTACH, replace an existing buffer with a fresh attach."
  (interactive "P")
  (herdr-core--attach-agent (herdr-core--pick-agent "Attach to agent: ") reattach))

;;;###autoload
(defun herdr-core-observe-agent ()
  "Pick an agent and watch it without typing (hc attach --observe)."
  (interactive)
  (herdr-core--attach-agent (herdr-core--pick-agent "Watch agent: ") nil nil t))

;;; Read buffers and renderers

(defvar-keymap herdr-core-read-mode-map
  :parent special-mode-map
  "g" #'herdr-core-read-refresh
  "f" #'herdr-core-read-toggle-follow
  "a" #'herdr-core-read-attach
  "s" #'herdr-core-read-send
  "d" #'herdr-core-read-open-diff
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(define-derived-mode herdr-core-read-mode special-mode "Herdr-Read"
  "An agent's screen and history (hc read), with what it produced rendered:
links, files, images and diffs (`herdr-core-renderers').
\\{herdr-core-read-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (herdr-core-read-refresh)))
  (setq truncate-lines nil))

(defun herdr-core--read-stamp-of (address)
  "What a read of ADDRESS is current for."
  (when-let* ((r (gethash address herdr-core--terminals)))
    (list (alist-get 'state (herdr-core--status-record r)) (alist-get 'turn r) (alist-get 'status_ms r))))

(defun herdr-core-render (beg end agent)
  "Run `herdr-core-renderers' over BEG..END for AGENT."
  (let ((inhibit-read-only t)
        (end (copy-marker end)))
    (dolist (fn herdr-core-renderers)
      (save-excursion
        (condition-case err
            (funcall fn beg (marker-position end) agent)
          (error (message "herdr-core: renderer %s failed: %s" fn (error-message-string err))))))))

(defun herdr-core--fill-read-buffer (buffer address text)
  "Fill read BUFFER for ADDRESS with TEXT and render it."
  (with-current-buffer buffer
    (unless (derived-mode-p 'herdr-core-read-mode)
      (herdr-core-read-mode)
      (setq herdr-core--read-follow herdr-core-read-follow))
    (setq herdr-core--attached-address address
          herdr-core--read-stamp (herdr-core--read-stamp-of address))
    (let ((inhibit-read-only t)
          (agent (or (herdr-core--find-agent address) `((address . ,address)
                                                       (machine . ,(herdr-core--machine address))))))
      (remove-overlays)
      (erase-buffer)
      (insert text)
      (herdr-core-render (point-min) (point-max) agent))
    (goto-char (point-max))
    (run-hooks 'herdr-core-read-hook)
    buffer))

(defun herdr-core--find-read-buffer (address)
  "The read buffer of ADDRESS, or nil."
  (seq-find (lambda (b) (and (herdr-core--read-buffer-p b)
                             (equal (buffer-local-value 'herdr-core--attached-address b) address)))
            (buffer-list)))

(defun herdr-core--read (address &optional no-display)
  "Show the screen and history of the agent at ADDRESS in its read buffer.
With NO-DISPLAY, only refill it."
  (herdr-core--hc (list "read" "--lines" (number-to-string herdr-core-read-lines) address)
                  (lambda (_code out)
                    (let ((buf (or (herdr-core--find-read-buffer address)
                                   (get-buffer-create (herdr-core-buffer-name address "read")))))
                      (herdr-core--fill-read-buffer buf address out)
                      (unless no-display
                        (pop-to-buffer buf (herdr-core--display-action 'read)))))))

(defun herdr-core--follow-reads ()
  "Read again every following read buffer whose agent's turn or status moved."
  (dolist (buf (buffer-list))
    (when (and (herdr-core--read-buffer-p buf) (buffer-local-value 'herdr-core--read-follow buf))
      (let ((address (buffer-local-value 'herdr-core--attached-address buf)))
        (unless (equal (buffer-local-value 'herdr-core--read-stamp buf)
                       (herdr-core--read-stamp-of address))
          (with-current-buffer buf
            (setq herdr-core--read-stamp (herdr-core--read-stamp-of address)))
          (herdr-core--read address t))))))

;;;###autoload
(defun herdr-core-read-agent ()
  "Pick an agent and show its screen and history (hc read), rendered."
  (interactive)
  (herdr-core--read (alist-get 'address (herdr-core--pick-agent "Read agent: "))))

(defun herdr-core-read-refresh ()
  "Read this buffer's agent again."
  (interactive)
  (herdr-core--read herdr-core--attached-address t))

(defun herdr-core-read-toggle-follow ()
  "Toggle reading again when the agent's turn or status changes."
  (interactive)
  (setq herdr-core--read-follow (not herdr-core--read-follow))
  (message "herdr-core: follow %s" (if herdr-core--read-follow "on" "off")))

(defun herdr-core--this-agent ()
  "The agent this buffer shows."
  (or (herdr-core--find-agent herdr-core--attached-address)
      `((address . ,herdr-core--attached-address))))

(defun herdr-core-read-attach ()
  "Attach to this read buffer's agent."
  (interactive)
  (herdr-core--attach-agent (herdr-core--this-agent)))

(defun herdr-core-read-send (text)
  "Send TEXT to this read buffer's agent."
  (interactive "sSend: ")
  (herdr-core--send (herdr-core--this-agent) text))

(defun herdr-core-render-ansi (beg end _agent)
  "Turn ANSI escapes in BEG..END into faces."
  (ansi-color-apply-on-region beg end))

(defun herdr-core--button-at-p (pos)
  "Non-nil when POS is already in a button."
  (button-at pos))

(defun herdr-core-render-links (beg end _agent)
  "Make URLs in BEG..END buttons (`herdr-core-browse-url-function')."
  (goto-char beg)
  (while (re-search-forward herdr-core-url-regexp end t)
    (let ((url (match-string-no-properties 0)))
      (unless (herdr-core--button-at-p (match-beginning 0))
        (make-text-button (match-beginning 0) (match-end 0)
                          'herdr-core-url url
                          'face 'herdr-core-link
                          'help-echo (concat "Open " url)
                          'action (lambda (_) (funcall herdr-core-browse-url-function url)))))))

(defun herdr-core--local-p (agent)
  "Non-nil when AGENT runs on this host."
  (equal (or (alist-get 'machine agent) "local") "local"))

(defun herdr-core--agent-file (agent path)
  "PATH as AGENT sees it: expanded against its cwd (on this host)."
  (if (or (string-prefix-p "/" path) (string-prefix-p "~" path))
      (expand-file-name path)
    (expand-file-name path (or (alist-get 'cwd agent) default-directory))))

(defun herdr-core-open-file (agent path &optional line)
  "Open PATH (at LINE) as AGENT sees it: here, or through `hc file read --on'."
  (if (herdr-core--local-p agent)
      (progn (funcall herdr-core-find-file-function (herdr-core--agent-file agent path))
             (when line (goto-char (point-min)) (forward-line (1- line))))
    (let* ((machine (alist-get 'machine agent))
           (got (apply #'herdr-core--call-sync
                       `("file" "read" "--on" ,machine
                         ,@(and (alist-get 'cwd agent) (list "--cwd" (alist-get 'cwd agent)))
                         ,path)))
           (buf (get-buffer-create (format "*hc file: %s:%s*" machine path))))
      (unless (eql (car got) 0)
        (user-error "herdr-core: hc file read %s on %s failed: %s" path machine (string-trim (cdr got))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (cdr got))
          (let ((buffer-file-name path))
            (delay-mode-hooks (set-auto-mode)))
          (setq buffer-read-only t))
        (goto-char (point-min))
        (when line (forward-line (1- line))))
      (pop-to-buffer buf '(nil (inhibit-same-window . t)))
      buf)))

(defun herdr-core-render-files (beg end agent)
  "Make file paths in BEG..END buttons: existing ones here, absolute ones elsewhere."
  (goto-char beg)
  (let ((local (herdr-core--local-p agent)))
    (while (re-search-forward herdr-core-file-regexp end t)
      (let* ((path (match-string-no-properties 1))
             (line (and (match-string 2) (string-to-number (match-string 2))))
             (b (match-beginning 1)) (e (or (match-end 2) (match-end 1))))
        (when (and (not (herdr-core--button-at-p b))
                   (not (string-match-p "\\`/+\\'" path))
                   (if local (file-exists-p (herdr-core--agent-file agent path))
                     (string-prefix-p "/" path)))
          (make-text-button b e
                            'herdr-core-file path
                            'face 'herdr-core-link
                            'help-echo (format "Open %s%s" path
                                               (if local "" (format " on %s" (alist-get 'machine agent))))
                            'action (lambda (_) (herdr-core-open-file agent path line))))))))

(defun herdr-core--create-image (file-or-data &optional data-p)
  "An image of FILE-OR-DATA (bytes when DATA-P), or nil when this Emacs cannot show it."
  (ignore-errors
    (let ((image (create-image file-or-data nil data-p :max-width herdr-core-image-max-width)))
      (and image (display-images-p) (image-type-available-p (plist-get (cdr image) :type))
           image))))

(defun herdr-core--remote-image-data (agent path)
  "PATH's bytes on AGENT's machine (`hc exec --on M base64 PATH'), or nil."
  (let ((got (herdr-core--call-sync "exec" "--on" (alist-get 'machine agent) "base64" path)))
    (and (eql (car got) 0)
         (ignore-errors (base64-decode-string (replace-regexp-in-string "[[:space:]]" "" (cdr got)))))))

(defun herdr-core--insert-image-after (pos image path)
  "Show IMAGE of PATH on its own line after the line at POS."
  (save-excursion
    (goto-char pos)
    (let ((ov (make-overlay (line-end-position) (line-end-position))))
      (overlay-put ov 'herdr-core-image path)
      (overlay-put ov 'after-string (concat "\n" (propertize " " 'display image))))))

(defun herdr-core--remote-image-action (agent path)
  "Button action fetching PATH from AGENT's machine and showing it."
  (lambda (button)
    (let ((image (herdr-core--create-image
                  (or (herdr-core--remote-image-data agent path)
                      (user-error "Cannot fetch %s" path))
                  t)))
      (unless image (user-error "Cannot show %s here" path))
      (herdr-core--insert-image-after (button-start button) image path))))

(defun herdr-core-render-images (beg end agent)
  "Show image files named in BEG..END inline, below their line.
Remote ones per `herdr-core-render-remote-images'."
  (goto-char beg)
  (let ((local (herdr-core--local-p agent)))
    (while (re-search-forward herdr-core-file-regexp end t)
      (let* ((path (match-string-no-properties 1))
             (b (match-beginning 1)) (e (match-end 1)))
        (when (and (string-match-p herdr-core-image-regexp path)
                   (not (herdr-core--button-at-p b)))
          (cond
           (local
            (let ((file (herdr-core--agent-file agent path)))
              (when-let* ((image (and (file-exists-p file) (herdr-core--create-image file))))
                (herdr-core--insert-image-after b image path))))
           ((not (and herdr-core-render-remote-images (string-prefix-p "/" path))))
           ((eq herdr-core-render-remote-images 'inline)
            (when-let* ((data (herdr-core--remote-image-data agent path))
                        (image (herdr-core--create-image data t)))
              (herdr-core--insert-image-after b image path)))
           (t
            (make-text-button b e
                              'herdr-core-image path
                              'face 'herdr-core-link
                              'help-echo (format "Show %s from %s" path (alist-get 'machine agent))
                              'action (herdr-core--remote-image-action agent path)))))))))

(defconst herdr-core--diff-line-regexp
  "\\(?:[-+ @\\]\\|diff \\|index \\|new file\\|deleted file\\|similarity\\|rename \\|old mode\\|new mode\\)"
  "Lines that continue a diff.")

(defun herdr-core--diff-regions (beg end)
  "Diffs in BEG..END: a list of (START . END)."
  (goto-char beg)
  (let (regions)
    (while (re-search-forward "^\\(?:diff --git \\|--- \\S-.*\n\\+\\+\\+ \\)" end t)
      (let ((start (match-beginning 0)))
        (forward-line 1)
        (while (and (< (point) end) (not (eolp)) (looking-at herdr-core--diff-line-regexp))
          (forward-line 1))
        (push (cons start (point)) regions)))
    (nreverse regions)))

(defun herdr-core-render-diffs (beg end _agent)
  "Color diffs in BEG..END as `diff-mode' does; `d' opens one in a diff buffer."
  (require 'diff-mode)
  (dolist (region (herdr-core--diff-regions beg end))
    (let ((ov (make-overlay (car region) (cdr region))))
      (overlay-put ov 'herdr-core-diff t)
      (overlay-put ov 'help-echo "d: open this diff in diff-mode"))
    (goto-char (car region))
    (while (< (point) (cdr region))
      (let ((face (cond ((looking-at "\\(?:diff \\|index \\|--- \\|\\+\\+\\+ \\)") 'diff-file-header)
                        ((looking-at "@@") 'diff-hunk-header)
                        ((looking-at "\\+") 'diff-added)
                        ((looking-at "-") 'diff-removed))))
        (when face
          (overlay-put (make-overlay (line-beginning-position) (min (1+ (line-end-position)) (cdr region)))
                       'face face)))
      (forward-line 1))))

(defun herdr-core-read-open-diff ()
  "Open the diff at point in a `diff-mode' buffer."
  (interactive)
  (let ((ov (seq-find (lambda (o) (overlay-get o 'herdr-core-diff)) (overlays-at (point)))))
    (unless ov (user-error "No diff here"))
    (let ((text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
          (buf (get-buffer-create (format "*hc diff: %s*" (herdr-core-title herdr-core--attached-address)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (diff-mode)
        (goto-char (point-min)))
      (pop-to-buffer buf '(nil (inhibit-same-window . t)))
      buf)))

;;; Talking to agents

(defun herdr-core--detect-language ()
  "Derive a fence language name from `major-mode'."
  (let ((name (symbol-name major-mode)))
    (cond
     ((string-match "\\`\\(.+\\)-ts-mode\\'" name) (match-string 1 name))
     ((string-match "\\`\\(.+\\)-mode\\'" name) (match-string 1 name))
     (t name))))

(defun herdr-core--region-or-line-bounds ()
  "Return (BEG END) for the active region, or the line at point."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position) (line-end-position))))

(defun herdr-core-format-region (beg end)
  "Format region BEG..END as a fenced code block with metadata."
  (let* ((lang (herdr-core--detect-language))
         (file (buffer-file-name))
         (proj (project-current))
         (root (and proj (project-root proj)))
         (rel-path (if (and file root)
                       (file-relative-name file root)
                     (and file (file-name-nondirectory file))))
         (line-beg (line-number-at-pos beg t))
         (line-end (line-number-at-pos end t))
         (branch (and root (vc-git--symbolic-ref root)))
         (text (buffer-substring-no-properties beg end))
         (meta (delq nil (list (and root (format "project: %s" (abbreviate-file-name root)))
                               (and branch (format "branch: %s" branch))))))
    (concat "```" lang " " (or rel-path "") (format " L%d-L%d" line-beg line-end) "\n"
            text
            (unless (string-suffix-p "\n" text) "\n")
            "```\n"
            (when meta (concat "\n" (string-join meta "\n") "\n")))))

(defcustom herdr-core-region-format-function #'herdr-core-format-region
  "Function of (BEG END) returning the text `herdr-core-send-region' sends."
  :type '(choice (const :tag "Fenced block with file, lines, project, branch" herdr-core-format-region)
                 (const :tag "The text as is" buffer-substring-no-properties)
                 function)
  :group 'herdr-core-picker)

(defun herdr-core--report (verb agent)
  "A callback reporting hc VERB on AGENT."
  (lambda (code out)
    (if (zerop code)
        (message "herdr-core: %s %s" verb (herdr-core--agent-display-name agent))
      (message "herdr-core: hc %s %s refused: %s" verb (alist-get 'address agent) (string-trim out)))))

(defun herdr-core--send (agent text &optional force)
  "Send TEXT to AGENT with `hc send' (paste, then Enter); FORCE types even
when it is blocked."
  (let ((address (alist-get 'address agent)))
    (herdr-core--hc (append '("send") (and force '("--force")) (list address text))
                    (lambda (code out)
                      (if (zerop code)
                          (message "herdr-core: sent to %s" (herdr-core--agent-display-name agent))
                        (message "herdr-core: hc send %s refused (blocked or gone?): %s"
                                 address (string-trim out)))))))

(defun herdr-core--target-agent (prompt)
  "The current buffer's agent, else one picked with PROMPT."
  (or (herdr-core--find-agent (herdr-core--current-address))
      (herdr-core--pick-agent prompt)))

;;;###autoload
(defun herdr-core-send-region (beg end &optional force)
  "Send region BEG..END to an agent as a prompt (`hc send').
Interactively, BEG and END are the active region, or the line at point
when no region is active.  hc pastes it and presses Enter; with prefix
arg FORCE it types even into a blocked agent (answering its dialog)."
  (interactive
   (append (herdr-core--region-or-line-bounds) (list current-prefix-arg)))
  (when (string-match-p "\\`[[:space:]]*\\'" (buffer-substring-no-properties beg end))
    (user-error "Nothing to send"))
  (herdr-core--send (herdr-core--pick-agent "Send to agent: ")
                    (funcall herdr-core-region-format-function beg end) force))

;;;###autoload
(defun herdr-core-submit-region (beg end)
  "Send region BEG..END to an agent even when it is blocked (`hc send --force')."
  (interactive (herdr-core--region-or-line-bounds))
  (herdr-core-send-region beg end t))

;;;###autoload
(defun herdr-core-send-keys (agent keys)
  "Press KEYS (space separated: enter, esc, up, ctrl+c, y) in AGENT (`hc keys')."
  (interactive
   (let ((agent (herdr-core--target-agent "Keys to agent: ")))
     (list agent (read-string (format "Keys for %s: " (herdr-core--agent-display-name agent))))))
  (herdr-core--hc (append (list "keys" (alist-get 'address agent)) (split-string keys))
                  (herdr-core--report "pressed keys in" agent)))

;;;###autoload
(defun herdr-core-interrupt-agent (agent)
  "Press AGENT's stop key (`hc interrupt')."
  (interactive (list (herdr-core--target-agent "Interrupt agent: ")))
  (herdr-core--hc (list "interrupt" "--json" (alist-get 'address agent))
                  (lambda (code out)
                    (let ((reply (herdr-core--parse (string-trim out))))
                      (message "herdr-core: interrupt %s: %s" (herdr-core--agent-display-name agent)
                               (if (zerop code) (or (alist-get 'outcome reply) "sent")
                                 (string-trim out)))))))

;;;###autoload
(defun herdr-core-put-file (agent file)
  "Send FILE beside AGENT (`hc put') and put where it landed in the kill ring."
  (interactive
   (let ((agent (herdr-core--target-agent "Put a file beside agent: ")))
     (list agent (read-file-name (format "File for %s: " (herdr-core--agent-display-name agent))
                                 nil nil t))))
  (herdr-core--hc (list "put" "--json" (alist-get 'address agent) (expand-file-name file))
                  (lambda (code out)
                    (let ((path (alist-get 'path (herdr-core--parse (string-trim out)))))
                      (if (and (zerop code) path)
                          (progn (kill-new path)
                                 (message "herdr-core: put %s at %s (copied)" file path))
                        (message "herdr-core: hc put failed: %s" (string-trim out)))))))

;;;###autoload
(defun herdr-core-wait-agent (agent)
  "Notify when AGENT stops working (`hc wait'), without blocking Emacs."
  (interactive (list (herdr-core--target-agent "Wait for agent: ")))
  (let ((name (herdr-core--agent-display-name agent)))
    (message "herdr-core: waiting for %s" name)
    (herdr-core--hc (append '("wait" "--json")
                            (and herdr-core-wait-timeout
                                 (list "--timeout" (number-to-string herdr-core-wait-timeout)))
                            (list (alist-get 'address agent)))
                    (lambda (_code out)
                      (let* ((reply (herdr-core--parse (string-trim out)))
                             (status (alist-get 'status reply)))
                        (funcall herdr-core-notify-function
                                 (format "%s: stopped (%s)" name (or (alist-get 'reason reply) "?"))
                                 (or (and (consp status) (alist-get 'state status))
                                     (and (stringp status) status) "")
                                 (alist-get 'address agent)))))))

;;;###autoload
(defun herdr-core-rename-agent (agent title)
  "Set AGENT's session TITLE (the `herdr-core-title-label' label; `hc label')."
  (interactive
   (let ((agent (herdr-core--target-agent "Rename agent: ")))
     (list agent (read-string "Title: " (herdr-core-title (alist-get 'address agent))))))
  (herdr-core--hc (list "label" "--json" (alist-get 'address agent)
                        (format "%s=%s" herdr-core-title-label title))
                  (herdr-core--report "renamed" agent)))

;;;###autoload
(defun herdr-core-kill-agent (agent)
  "End AGENT's program (`hc kill'), after asking."
  (interactive (list (herdr-core--target-agent "Kill agent: ")))
  (when (yes-or-no-p (format "Kill %s? " (herdr-core--agent-display-name agent)))
    (herdr-core--hc (list "kill" "--json" (alist-get 'address agent))
                    (herdr-core--report "killed" agent))))

;;;###autoload
(defun herdr-core-schema ()
  "Show `hc schema' (the OpenRPC API) in a buffer."
  (interactive)
  (let ((got (herdr-core--call-sync "schema")))
    (with-current-buffer (get-buffer-create "*hc schema*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (cdr got)))
      (when (fboundp 'js-json-mode) (js-json-mode))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Starting agents

(defun herdr-core-project-directory ()
  "The current project's root, else `default-directory'."
  (expand-file-name (if-let* ((proj (project-current nil))) (project-root proj) default-directory)))

(defun herdr-core--sanitize-name (str)
  "Sanitize STR to a valid hc terminal name (1-32 of a-z 0-9 - _)."
  (let ((clean (replace-regexp-in-string "[^a-z0-9_-]" "-" (downcase str))))
    (setq clean (replace-regexp-in-string "^[^a-z]+" "" clean))
    (setq clean (replace-regexp-in-string "-+" "-" clean))
    (if (string-empty-p clean) "agent" clean)))

(defun herdr-core-default-agent-name (dir kind)
  "PROJECT-KIND for DIR and KIND."
  (herdr-core--sanitize-name (concat (herdr-core--project-name dir) "-" kind)))

(defun herdr-core--unique-agent-name (base agents)
  "Return BASE or BASE-N not used by any of AGENTS, at most 32 chars."
  (let ((existing (delq nil (mapcar (lambda (a) (alist-get 'name a)) agents)))
        (name (truncate-string-to-width base 32))
        (n 2))
    (while (member name existing)
      (setq name (truncate-string-to-width (format "%s-%d" base n) 32)
            n (1+ n)))
    name))

(defun herdr-core--launched-address (output name)
  "The address in the launcher's OUTPUT (hc's JSON line), else NAME."
  (or (seq-some (lambda (line) (alist-get 'address (herdr-core--parse line)))
                (reverse (split-string output "\n" t)))
      name))

(defun herdr-core-launch-on-args (spec)
  "(\"--on\" MACHINE) for format SPEC off this host, else nil."
  (let ((m (alist-get ?m spec)))
    (and m (not (equal m "local")) (list "--on" m))))

(defun herdr-core--format-spec (spec)
  "SPEC (an agent spec alist) as a `format-spec' alist."
  `((?n . ,(or (alist-get 'name spec) "")) (?d . ,(or (alist-get 'dir spec) ""))
    (?k . ,(or (alist-get 'kind spec) "")) (?m . ,(or (alist-get 'machine spec) "local"))
    (?s . ,(or (alist-get 'space spec) "")) (?t . ,(or (alist-get 'tab spec) ""))))

(defun herdr-core-launcher-arguments (spec)
  "`herdr-core-launcher-args' filled in for SPEC."
  (let ((fspec (herdr-core--format-spec spec)))
    (mapcan (lambda (arg)
              (cond ((stringp arg) (list (format-spec arg fspec)))
                    ((functionp arg) (let ((v (funcall arg fspec)))
                                       (if (listp v) (copy-sequence v) (list v))))))
            herdr-core-launcher-args)))

(defun herdr-core--launcher-machine-aware-p ()
  "Non-nil when `herdr-core-launcher-args' place the machine."
  (seq-some (lambda (a) (or (functionp a) (and (stringp a) (string-match-p "%m" a))))
            herdr-core-launcher-args))

(defun herdr-core-launch-command (spec callback)
  "Start SPEC's agent with `herdr-core-launcher'; CALLBACK gets its address and output."
  (let ((name (alist-get 'name spec)))
    (herdr-core--run herdr-core-launcher (herdr-core-launcher-arguments spec)
                     (lambda (code out)
                       (if (not (zerop code))
                           (progn (message "herdr-core: FAILED to start %s: %s" name (string-trim out))
                                  (funcall callback nil out))
                         (funcall callback (herdr-core--launched-address out name) out))))))

;;;###autoload
(defun herdr-core-new-agent (kind &optional machine)
  "Start a KIND agent (a harness) for the current project and attach to it.
Goes through `herdr-core-launch-function' (by default `herdr-core-launcher':
hc run), which names it and labels it herdr.space/herdr.tab.  With a
prefix argument, pick the MACHINE."
  (interactive
   (list (completing-read "Agent kind: " herdr-core-agent-kinds nil nil
                          nil nil herdr-core-default-agent-kind)
         (and current-prefix-arg
              (completing-read "Machine: " (mapcar (lambda (m) (alist-get 'name m)) (herdr-core-machines))
                               nil t nil nil "local"))))
  (when (and machine (not (equal machine "local"))
             (eq herdr-core-launch-function #'herdr-core-launch-command)
             (not (herdr-core--launcher-machine-aware-p)))
    (user-error "herdr-core-launcher-args has no %%m or `herdr-core-launch-on-args'"))
  (let* ((root (funcall herdr-core-launch-directory-function))
         (name (herdr-core--unique-agent-name
                (funcall herdr-core-agent-name-function root kind)
                (ignore-errors (herdr-core--fetch-agents-sync))))
         (spec `((name . ,name) (dir . ,root) (kind . ,kind) (machine . ,(or machine "local"))
                 (space . ,(herdr-core--project-name root)) (tab . ,name)))
         (window (selected-window)))
    (message "herdr-core: starting %s in %s..." name (abbreviate-file-name root))
    (funcall herdr-core-launch-function spec
             (lambda (address &optional out)
               (when address
                 (message "herdr-core: started %s" name)
                 (run-hook-with-args 'herdr-core-after-launch-functions address out)
                 (when herdr-core-attach-after-launch
                   (when (window-live-p window) (select-window window))
                   (herdr-core--attach-agent `((address . ,address))
                                             nil (herdr-core--display-action 'launch))))))))

;;; Notifications

(defun herdr-core-message-notify (title body &optional _address)
  "Show TITLE and BODY in the echo area."
  (message "%s: %s" title body))

(defun herdr-core-system-notify (title body &optional address)
  "Show TITLE and BODY as a desktop notification, else `message' them.
With ADDRESS, clicking the notification shows that agent (`herdr-core-show-agent')."
  (cond
   ((and (eq system-type 'darwin) (executable-find "terminal-notifier"))
    (make-process
     :name "herdr-core-notify" :noquery t
     :command `("terminal-notifier" "-title" ,title "-message" ,(if (string-empty-p body) " " body)
                "-activate" "org.gnu.Emacs"
                ,@(and address (list "-execute" (herdr-core--show-agent-command address))))))
   ((eq system-type 'darwin)
    (make-process
     :name "herdr-core-notify" :noquery t
     :command (list "osascript"
                    "-e" "on run argv"
                    "-e" "display notification (item 2 of argv) with title (item 1 of argv)"
                    "-e" "end run"
                    title body)))
   ((and (not noninteractive) (featurep 'dbusbind) (require 'notifications nil t))
    (notifications-notify :title title :body body
                          :actions (and address '("default" "Show agent"))
                          :on-action (and address
                                          (lambda (_id _key) (herdr-core-show-agent address)))))
   (t (herdr-core-message-notify title body))))

(defun herdr-core--agent-in-view-p (agent)
  "Non-nil when AGENT's buffer is in the selected window of a focused frame."
  (and (eq (frame-focus-state) t)
       (equal (buffer-local-value 'herdr-core--attached-address (window-buffer (selected-window)))
              (alist-get 'address agent))))

(defun herdr-core--notify-transition-p (old new)
  "Non-nil if going from status OLD to NEW should notify.
Leaving `working' for `idle' counts as `done'."
  (or (member new herdr-core-notify-statuses)
      (and (equal old "working") (equal new "idle")
           (member "done" herdr-core-notify-statuses))))

(defun herdr-core-notify (agent old new)
  "Notify (`herdr-core-notify-function') when AGENT enters a status in
`herdr-core-notify-statuses', from OLD to NEW.  Quiet while you are already
looking at the agent's buffer, unless `herdr-core-notify-when-visible'."
  (when (and (herdr-core--notify-transition-p old new)
             (or herdr-core-notify-when-visible (not (herdr-core--agent-in-view-p agent))))
    (let ((blocked (alist-get 'blocked-kind agent)))
      (funcall herdr-core-notify-function
               (format "%s: %s%s" (herdr-core--agent-display-name agent)
                       (if (equal new "idle") "done" new)
                       (if (and blocked (equal new "blocked")) (format " (%s)" blocked) ""))
               (string-join (delq nil (list (alist-get 'machine agent) (alist-get 'msg agent))) " · ")
               (alist-get 'address agent)))))

(defun herdr-core--show-agent-command (address)
  "Shell command that makes this Emacs show the agent at ADDRESS."
  (format "%s -s %s --eval %s"
          (shell-quote-argument (or (executable-find "emacsclient") "emacsclient"))
          (shell-quote-argument (or (bound-and-true-p server-name) "server"))
          (shell-quote-argument (format "(herdr-core-show-agent %S)" address))))

;;; Agent list buffer

(defun herdr-core-column-status (agent)
  "Status cell of AGENT: status (and blocked kind), in its face."
  (let ((status (alist-get 'status agent))
        (blocked (alist-get 'blocked-kind agent)))
    (propertize (if blocked (format "%s:%s" status blocked) status)
                'face (herdr-core--status-face status))))

(defun herdr-core-column-glyph (agent)
  "Glyph cell of AGENT."
  (let ((status (alist-get 'status agent)))
    (propertize (herdr-core--glyph status) 'face (herdr-core--status-face status))))

(defun herdr-core-column-name (agent)
  "Display-name cell of AGENT, clickable."
  (propertize (herdr-core--agent-display-name agent)
              'mouse-face 'highlight
              'help-echo "mouse-1: show this agent here"))

(defun herdr-core-column-kind (agent) "Kind cell of AGENT." (or (alist-get 'kind agent) ""))
(defun herdr-core-column-machine (agent) "Machine cell of AGENT." (alist-get 'machine agent))
(defun herdr-core-column-space (agent) "Space cell of AGENT." (or (alist-get 'space agent) ""))
(defun herdr-core-column-tab (agent) "Tab cell of AGENT." (or (alist-get 'tab agent) ""))
(defun herdr-core-column-address (agent) "Address cell of AGENT." (alist-get 'address agent))
(defun herdr-core-column-message (agent) "Status message cell of AGENT." (or (alist-get 'msg agent) ""))

(defun herdr-core-column-cwd (agent)
  "Working directory cell of AGENT."
  (if-let* ((cwd (alist-get 'cwd agent))) (abbreviate-file-name cwd) ""))

(defun herdr-core-column-age (agent)
  "Time since AGENT's last status change, as 5s, 3m, 2h or 4d."
  (let ((ms (alist-get 'seq agent)))
    (if (or (null ms) (zerop ms)) ""
      (let ((s (truncate (max 0 (- (float-time) (/ ms 1000.0))))))
        (cond ((< s 60) (format "%ds" s))
              ((< s 3600) (format "%dm" (/ s 60)))
              ((< s 86400) (format "%dh" (/ s 3600)))
              (t (format "%dd" (/ s 86400))))))))

;; The list is a `vuistack-table' (a vui.el component) mounted in the
;; buffer.  Its columns are `herdr-core-list-columns', then hidden
;; helper columns the default sort and the narrowing read (attention,
;; changed, machine, space, tab), then, while grouped, the group column.
;; Sort, filters, grouping, expansion, selection (the marks) and paging
;; are the table's state, buffer-local, so a re-render on an hc event
;; keeps them.

(defconst herdr-core--table-id 'herdr-core-agents
  "The `vuistack-table' id of the agent list.")

(defconst herdr-core--helper-columns '(attention changed machine space tab)
  "Hidden columns after `herdr-core-list-columns', in order.")

(defun herdr-core--status-of-cell (cell)
  "The status a status or glyph CELL string shows."
  (let ((text (substring-no-properties (format "%s" cell))))
    (or (car (rassoc text herdr-core-status-glyphs))
        (car (split-string text ":")))))

(defun herdr-core--compare-status-cells (a b)
  "Order status or glyph cells A and B by `herdr-core-status-priority'."
  (let ((pa (herdr-core--priority `((status . ,(herdr-core--status-of-cell a)))))
        (pb (herdr-core--priority `((status . ,(herdr-core--status-of-cell b))))))
    (- pa pb)))

(defun herdr-core--age-seconds (cell)
  "Seconds an age CELL (5s, 3m, 2h, 4d) stands for, or nil."
  (let ((text (format "%s" cell)))
    (when (string-match "\\`\\([0-9]+\\)\\([smhd]\\)\\'" text)
      (* (string-to-number (match-string 1 text))
         (pcase (match-string 2 text) ("s" 1) ("m" 60) ("h" 3600) (_ 86400))))))

(defun herdr-core--compare-ages (a b)
  "Order age cells A and B, youngest first, blanks last."
  (let ((sa (herdr-core--age-seconds a)) (sb (herdr-core--age-seconds b)))
    (cond ((and sa sb) (- sa sb)) (sa -1) (sb 1) (t 0))))

(defun herdr-core--aggregate-attention (_values rows)
  "A group's attention cell: a glyph and count per attention status in ROWS."
  (let ((statuses (mapcar (lambda (r) (alist-get 'status (plist-get r :agent))) rows)))
    (string-join
     (delq nil (mapcar (lambda (status)
                         (let ((n (seq-count (lambda (s) (equal s status)) statuses)))
                           (and (> n 0)
                                (propertize (format "%s %d" (herdr-core--glyph status) n)
                                            'face (herdr-core--status-face status)))))
                       herdr-core-attention-statuses))
     " ")))

(defconst herdr-core--column-props
  '((herdr-core-column-status :sort-fn herdr-core--compare-status-cells
                              :aggregation herdr-core--aggregate-attention)
    (herdr-core-column-glyph :sort-fn herdr-core--compare-status-cells
                             :aggregation herdr-core--aggregate-attention)
    (herdr-core-column-age :sort-fn herdr-core--compare-ages :align right))
  "`vuistack-table' column keys the built-in cell functions add.")

(defun herdr-core--list-column (spec)
  "The `vuistack-table' column of a `herdr-core-list-columns' SPEC."
  (pcase-let ((`(,title ,width ,fn . ,rest) spec))
    (append (car rest)
            (list :header title
                  :get (lambda (row) (or (funcall fn (plist-get row :agent)) "")))
            (and (> width 0) (list :min-width width))
            (cdr (assq fn herdr-core--column-props)))))

(defun herdr-core--helper-index (helper)
  "The column index of HELPER, one of `herdr-core--helper-columns'."
  (+ (length herdr-core-list-columns) (seq-position herdr-core--helper-columns helper)))

(defun herdr-core--group-index ()
  "The column index of the group column."
  (+ (length herdr-core-list-columns) (length herdr-core--helper-columns)))

(defun herdr-core--group-column (headers)
  "The group column: a group row shows its header from HEADERS (GROUP -> text)."
  (list :header (if (symbolp herdr-core-group-by)
                    (capitalize (symbol-name herdr-core-group-by))
                  "Group")
        :id :herdr-group
        :get #'ignore
        :grouping-value (lambda (row) (plist-get row :group))
        :format (lambda (group)
                  (if-let* ((text (and group (gethash group headers))))
                      (propertize text 'face 'herdr-core-group-header)
                    ""))
        :disable-sort t :disable-hide t))

(defun herdr-core--table-columns (headers)
  "Every column of the list table; HEADERS as in `herdr-core--group-column'."
  (append (mapcar #'herdr-core--list-column herdr-core-list-columns)
          (list (list :header "attention" :key :attention :hidden t)
                (list :header "changed" :key :changed :hidden t :sort-desc-first t)
                (list :header "machine" :key :machine :hidden t)
                (list :header "space" :key :space :hidden t)
                (list :header "tab" :key :tab :hidden t))
          (and herdr-core-group-by (list (herdr-core--group-column headers)))))

(defun herdr-core--default-sort ()
  "The table sort standing for `herdr-core-sort-predicate'.
Attention is the multi-sort attention, then most recent change; most
recent is the change column; any other predicate already ordered the
rows, so no sort keeps its order."
  (let ((attention (herdr-core--helper-index 'attention))
        (changed (herdr-core--helper-index 'changed)))
    (pcase herdr-core-sort-predicate
      ('herdr-core-sort-by-attention (list (cons attention nil) (cons changed t)))
      ('herdr-core-sort-by-recent (list (cons changed t))))))

(defun herdr-core--agent-rows (agents)
  "AGENTS as table rows, in groups when grouped; and the group headers.
Returns (ROWS . HEADERS), HEADERS a hash table GROUP -> header text."
  (let ((headers (make-hash-table :test 'equal))
        (rows nil))
    (dolist (group (if herdr-core-group-by (herdr-core-groups agents) (list (cons nil agents))))
      (when herdr-core-group-by
        (puthash (car group) (funcall herdr-core-group-header-function (car group) (cdr group))
                 headers))
      (dolist (a (cdr group))
        (push (list :address (alist-get 'address a) :agent a :group (car group)
                    :attention (herdr-core--priority a) :changed (alist-get 'seq a)
                    :machine (alist-get 'machine a) :space (alist-get 'space a)
                    :tab (alist-get 'tab a))
              rows)))
    (cons (nreverse rows) headers)))

(defun herdr-core--row-agent (row)
  "The agent of table ROW, as known now."
  (or (herdr-core--find-agent (plist-get row :address)) (plist-get row :agent)))

(defun herdr-core--row-action (row)
  "The table's row action: show ROW's agent here."
  (herdr-core--attach-agent (herdr-core--row-agent row) nil))

(defun herdr-core--list-address (&optional pos)
  "The agent address of the list line at POS (default point), or nil."
  (plist-get (get-text-property (save-excursion (goto-char (or pos (point)))
                                                (line-beginning-position))
                                'vuistack-table-row)
             :address))

(defvar-keymap herdr-core-agents-narrow-map
  "m" #'herdr-core-agents-narrow-machine
  "s" #'herdr-core-agents-narrow-space
  "t" #'herdr-core-agents-narrow-tab
  "f" #'vuistack-table-filter
  "/" #'herdr-core-agents-widen)

(defvar-keymap herdr-core-agents-mode-map
  :parent (make-composed-keymap vuistack-table-mode-map vui-mode-map)
  "RET" #'herdr-core-agents-show
  "<mouse-1>" #'herdr-core-agents-mouse-show
  "a" #'herdr-core-agents-attach
  "o" #'herdr-core-agents-observe
  "r" #'herdr-core-agents-read
  "s" #'herdr-core-agents-send
  "b" #'herdr-core-agents-next-blocked
  "S" #'herdr-core-agents-sort
  "G" #'herdr-core-agents-group
  "m" #'herdr-core-agents-mark
  "u" #'herdr-core-agents-unmark
  "v" #'herdr-core-agents-show-marked
  "i" #'herdr-core-agents-interrupt
  "e" #'herdr-core-agents-rename
  "k" #'herdr-core-agents-kill
  "/" herdr-core-agents-narrow-map
  "c" #'herdr-core-new-agent
  "g" #'herdr-core-agents-refresh
  "?" #'herdr-core-menu
  "n" #'herdr-core-agents-next-line
  "p" #'herdr-core-agents-previous-line)

(defvar-local herdr-core--list-columns nil
  "The `herdr-core-list-columns' the table state was made for.")

(defvar-local herdr-core--list-group-by 'unset
  "The `herdr-core-group-by' the table state groups by.")

(defvar-local herdr-core--agents-root nil
  "The vui root instance of the list table.")

(define-derived-mode herdr-core-agents-mode vui-mode "Herdr-Agents"
  "Live list of herdr-core agents on every machine, from `hc events'.
A `vuistack-table': RET or mouse-1 shows the agent (the row action), `a'
attaches afresh with a prefix, `o' watches, `r' reads its screen, `s'
sends a line, `b' jumps to the next blocked agent, `S' sorts (with a
prefix, adds to the multi-sort), `G' groups, `m'/`u' mark and `v' shows
the marked side by side, `/' narrows and filters, `c' starts a new
agent, `g' reconnects.  The table's own keys (`f' filter a column, `F'
clear filters, `x' fold a group, `H'/`V' columns, `|' pin, ...) work too.
\\{herdr-core-agents-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function (lambda (&rest _) (herdr-core-agents-refresh)))
  ;; The stream outlives this buffer only while `herdr-core-mode' wants it.
  (add-hook 'kill-buffer-hook
            (lambda () (unless (bound-and-true-p herdr-core-mode) (herdr-core-stop)))
            nil t))

(defun herdr-core--table-seeded-p ()
  "Non-nil once the list table has state in this buffer."
  (assoc herdr-core--table-id vuistack-table--table-states))

(defun herdr-core--table-get (key)
  "KEY of the list table's state."
  (plist-get (vuistack-table-state herdr-core--table-id) key))

(defun herdr-core--table-put (&rest plist)
  "Set the keys of PLIST in the list table's state."
  (cl-loop for (key value) on plist by #'cddr
           do (vuistack-table--put herdr-core--table-id key value)))

(defun herdr-core--sync-table-state ()
  "Start the table state afresh when the columns changed; follow the grouping."
  (unless (equal herdr-core--list-columns herdr-core-list-columns)
    (setq herdr-core--list-columns herdr-core-list-columns)
    (vuistack-table-reset-state))
  (unless (equal herdr-core--list-group-by herdr-core-group-by)
    (setq herdr-core--list-group-by herdr-core-group-by)
    (when (herdr-core--table-seeded-p)
      (herdr-core--table-put :grouping (and herdr-core-group-by (list (herdr-core--group-index)))
                             :expanded t :page 0))))

(defun herdr-core--agents-props (agents)
  "The props of the list table showing AGENTS."
  (pcase-let ((`(,rows . ,headers) (herdr-core--agent-rows agents)))
    (list :id herdr-core--table-id
          :columns (herdr-core--table-columns headers)
          :rows rows
          :row-id (lambda (row) (plist-get row :address))
          :action #'herdr-core--row-action
          :enable-row-selection t
          :page-size herdr-core-list-page-size
          :initial-state `(:sort ,(herdr-core--default-sort)
                           :grouping ,(and herdr-core-group-by (list (herdr-core--group-index)))
                           :expanded t)
          :empty (propertize "No agents (c starts one)" 'face 'shadow))))

(defun herdr-core--goto-address (address &optional column)
  "Move to ADDRESS's line (at COLUMN), else the first agent line."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (and address (equal (herdr-core--list-address) address))
          (setq found t)
        (forward-line 1)))
    (if found
        (move-to-column (or column 0))
      (goto-char (point-min))
      (herdr-core--skip-header 1))))

(defun herdr-core--agents-render ()
  "Render the list table from the current agents, keeping point on its agent."
  (let ((address (herdr-core--list-address))
        (column (current-column))
        (agents (herdr-core-list)))
    (herdr-core--sync-table-state)
    (setq herdr-core--agents agents)
    (let ((props (herdr-core--agents-props agents)))
      (if (and herdr-core--agents-root (eq herdr-core--agents-root (herdr-core--vui-root)))
          (vui-update herdr-core--agents-root props)
        (setq herdr-core--agents-root
              (herdr-core--vui-mount (apply #'vui-component 'vuistack-table props) (buffer-name)))))
    (set-buffer-modified-p nil)
    (herdr-core--goto-address address column)))

(defun herdr-core--vui-root ()
  "The vui root instance mounted in this buffer, on any vui.
Newer vui names it `vui--mounted-root', older `vui--root-instance'."
  (cond ((boundp 'vui--mounted-root) (symbol-value 'vui--mounted-root))
        ((boundp 'vui--root-instance) (symbol-value 'vui--root-instance))))

(defun herdr-core--vui-mount (vnode name)
  "Mount VNODE in buffer NAME without showing it, on any vui.
A vui whose `vui-mount' has no NO-DISPLAY argument shows the buffer, so
the window configuration is restored around it."
  (if (>= (or (cdr-safe (func-arity #'vui-mount)) 0) 3)
      (vui-mount vnode name t)
    (save-window-excursion (save-current-buffer (vui-mount vnode name)))))

(defun herdr-core--skip-header (dir)
  "Move off lines without an agent (headers, group rows) in DIR (1 or -1)."
  (while (and (not (herdr-core--list-address))
              (zerop (forward-line dir))
              (not (eobp)))))

(defun herdr-core-agents-next-line (&optional n)
  "Move to the next agent line (N times), skipping headers and group rows."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((here (point)))
      (forward-line 1)
      (herdr-core--skip-header 1)
      (unless (herdr-core--list-address) (goto-char here)))))

(defun herdr-core-agents-previous-line (&optional n)
  "Move to the previous agent line (N times), skipping headers and group rows."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((here (point)))
      (forward-line -1)
      (herdr-core--skip-header -1)
      (unless (herdr-core--list-address) (goto-char here)))))

(defun herdr-core--refresh ()
  "Redraw the list buffer (keeping point), buffer names, read buffers and the mode line."
  (setq herdr-core--refresh-timer nil)
  (when-let* ((buf (get-buffer herdr-core--agents-buffer-name)))
    (with-current-buffer buf
      (when (derived-mode-p 'herdr-core-agents-mode)
        (herdr-core--agents-render))))
  (herdr-core--sync-attach-buffer-names)
  (when herdr-core-read-follow (herdr-core--follow-reads))
  (force-mode-line-update t)
  (run-hooks 'herdr-core-refresh-hook))

(defun herdr-core--schedule-refresh ()
  "Redraw soon, once for a burst of events."
  (unless herdr-core--refresh-timer
    (setq herdr-core--refresh-timer (run-at-time herdr-core-refresh-delay nil #'herdr-core--refresh))))

(defun herdr-core-agents-refresh ()
  "Reconnect for a fresh snapshot, listing what is known meanwhile."
  (interactive)
  (herdr-core-reconnect)
  (herdr-core--agents-render))

(defun herdr-core--agent-at-point ()
  "Agent alist for the list-buffer row at point."
  (let ((address (herdr-core--list-address)))
    (or (and address (or (herdr-core--find-agent address)
                         (seq-find (lambda (a) (equal (alist-get 'address a) address))
                                   herdr-core--agents)))
        (user-error "No agent on this line"))))

(defconst herdr-core--attention-sort-label "attention (default)"
  "Sort choice that restores the `herdr-core-sort-predicate' order.")

(defun herdr-core--agents-redraw ()
  "Rebuild and print the list buffer."
  (herdr-core--agents-render))

(defun herdr-core--sort-choices ()
  "Column headers `herdr-core-agents-sort' offers, with their indices."
  (append (seq-filter (lambda (c) (not (string-empty-p (car c))))
                      (seq-map-indexed (lambda (spec i) (cons (car spec) i))
                                       herdr-core-list-columns))
          (list (cons "changed" (herdr-core--helper-index 'changed)))))

(defun herdr-core-agents-sort (&optional multi)
  "Pick a column to sort by; picking the current one reverses the order.
The attention entry restores the default sort (`herdr-core-sort-predicate').
With prefix MULTI, add the column to the multi-sort (again: reverse it,
a third time: drop it).  The table's state keeps the sort across updates."
  (interactive "P")
  (let* ((choices (herdr-core--sort-choices))
         (at (get-text-property (point) 'vuistack-table-column-index))
         (default (car (rassoc at choices)))
         (choice (completing-read (format-prompt "Sort by" default)
                                  (cons herdr-core--attention-sort-label (mapcar #'car choices))
                                  nil t nil nil default))
         (index (cdr (assoc choice choices)))
         (sorts (vuistack-table--sort-list (herdr-core--table-get :sort)))
         (current (assq index sorts))
         (next (and index (cons index (if current (not (cdr current))
                                        (equal choice "changed"))))))
    (herdr-core--table-put
     :page 0
     :sort (cond ((null index) (herdr-core--default-sort))
                 ((not multi) (list next))
                 ((null current) (append sorts (list next)))
                 ((eq (cdr current) (equal choice "changed"))
                  (mapcar (lambda (s) (if (eq s current) next s)) sorts))
                 (t (remq current sorts))))
    (herdr-core--agents-redraw)))

(defun herdr-core-agents-group (by)
  "Group this list BY machine, space, tab, kind, status or nothing.
The groups are the table's grouping: `x' folds one, `X' all."
  (interactive
   (list (let ((c (completing-read "Group by: " '("none" "machine" "space" "tab" "kind" "status")
                                   nil t)))
           (unless (equal c "none") (intern c)))))
  (setq-local herdr-core-group-by by)
  (herdr-core--agents-redraw))

(defun herdr-core--narrow (key prompt)
  "Narrow the list to agents whose KEY is one read with PROMPT.
The narrowing is an exact column filter on the table's KEY column."
  (let* ((values (delete-dups (delq nil (mapcar (lambda (a) (alist-get key a)) (herdr-core-list)))))
         (here (ignore-errors (alist-get key (herdr-core--agent-at-point))))
         (value (completing-read prompt values nil t nil nil here))
         (narrowing (mapcar #'herdr-core--helper-index '(machine space tab))))
    (herdr-core--table-put
     :page 0
     :column-filters (append (seq-remove (lambda (f) (memq (car f) narrowing))
                                         (herdr-core--table-get :column-filters))
                             (list (cons (herdr-core--helper-index key) (concat "==" value)))))
    (herdr-core--agents-redraw)
    (message "herdr-core: showing %s %s (/ / widens)" key value)))

(defun herdr-core-agents-narrow-machine ()
  "Show only one machine's agents."
  (interactive)
  (herdr-core--narrow 'machine "Machine: "))

(defun herdr-core-agents-narrow-space ()
  "Show only one space's agents."
  (interactive)
  (herdr-core--narrow 'space "Space: "))

(defun herdr-core-agents-narrow-tab ()
  "Show only one tab's agents."
  (interactive)
  (herdr-core--narrow 'tab "Tab: "))

(defun herdr-core-agents-widen ()
  "Show every agent again: drop the narrowing and every filter."
  (interactive)
  (herdr-core--table-put :filter nil :column-filters nil :page 0)
  (herdr-core--agents-redraw))

(defun herdr-core-agents-marked ()
  "Addresses of the marked (selected) agents still listed, in marking order."
  (seq-filter #'herdr-core--find-agent (herdr-core--table-get :selected)))

(defun herdr-core--set-marked (address marked)
  "Mark ADDRESS when MARKED, else unmark it; then go to the next agent."
  (let ((selected (remove address (herdr-core--table-get :selected))))
    (herdr-core--table-put :selected (if marked (append selected (list address)) selected)))
  (herdr-core--agents-redraw)
  (herdr-core-agents-next-line))

(defun herdr-core-agents-mark ()
  "Mark (select) the agent at point for `herdr-core-agents-show-marked'."
  (interactive)
  (herdr-core--set-marked (alist-get 'address (herdr-core--agent-at-point)) t))

(defun herdr-core-agents-unmark ()
  "Unmark the agent at point."
  (interactive)
  (herdr-core--set-marked (alist-get 'address (herdr-core--agent-at-point)) nil))

(defun herdr-core-agents-show-marked (&optional observe)
  "Show the marked agents (else the one at point) side by side.
With prefix OBSERVE, watch only."
  (interactive "P")
  (let ((agents (or (delq nil (mapcar #'herdr-core--find-agent (herdr-core-agents-marked)))
                    (list (herdr-core--agent-at-point)))))
    (herdr-core--table-put :selected nil)
    (herdr-core--agents-redraw)
    (herdr-core-show-several agents observe)))

(defun herdr-core-agents-mouse-show (event)
  "Show the agent on the clicked line (EVENT), in the clicked window."
  (interactive "e")
  (let ((pos (event-start event)))
    (select-window (posn-window pos))
    (goto-char (posn-point pos))
    (herdr-core-agents-show)))

(defun herdr-core-agents-show (&optional reattach)
  "Show the agent at point in this window; with prefix REATTACH, afresh.
Runs the table's row action (`vuistack-table-activate') on a cell."
  (interactive "P")
  (unless (and (not reattach) (vuistack-table-activate))
    (herdr-core--attach-agent (herdr-core--agent-at-point) reattach)))

(defun herdr-core-agents-attach (&optional reattach)
  "Attach to the agent at point in its own buffer, in this window.
With prefix REATTACH, replace an existing buffer with a fresh attach."
  (interactive "P")
  (herdr-core--attach-agent (herdr-core--agent-at-point) reattach))

(defun herdr-core-agents-observe ()
  "Watch the agent at point without typing."
  (interactive)
  (herdr-core--attach-agent (herdr-core--agent-at-point) nil nil t))

(defun herdr-core-agents-read ()
  "Show the screen and history of the agent at point."
  (interactive)
  (herdr-core--read (alist-get 'address (herdr-core--agent-at-point))))

(defun herdr-core-agents-send (text)
  "Send TEXT to the agent at point, then Enter."
  (interactive
   (list (read-string (format "Send to %s: "
                              (herdr-core--agent-display-name (herdr-core--agent-at-point))))))
  (herdr-core--send (herdr-core--agent-at-point) text))

(defun herdr-core-agents-interrupt ()
  "Interrupt the agent at point."
  (interactive)
  (herdr-core-interrupt-agent (herdr-core--agent-at-point)))

(defun herdr-core-agents-rename (title)
  "Set the session TITLE of the agent at point."
  (interactive (list (read-string "Title: " (herdr-core-title (herdr-core--list-address)))))
  (herdr-core-rename-agent (herdr-core--agent-at-point) title))

(defun herdr-core-agents-kill ()
  "Kill the agent at point, after asking."
  (interactive)
  (herdr-core-kill-agent (herdr-core--agent-at-point)))

(defun herdr-core-agents-next-blocked ()
  "Move to the next blocked agent, wrapping around."
  (interactive)
  (let ((here (line-beginning-position)) first next)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (equal (alist-get 'status (herdr-core--find-agent (herdr-core--list-address)))
                     "blocked")
          (unless first (setq first (point)))
          (when (and (not next) (> (point) here)) (setq next (point))))
        (forward-line 1)))
    (if (or next first)
        (goto-char (or next first))
      (message "No blocked agent"))))

;;;###autoload
(defun herdr-core-agents ()
  "Pop to the live agent list of every machine; it follows `hc events'."
  (interactive)
  (let ((buf (get-buffer-create herdr-core--agents-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'herdr-core-agents-mode)
        (herdr-core-agents-mode))
      (herdr-core-start)
      (herdr-core--agents-render))
    (pop-to-buffer buf (herdr-core--display-action 'list))))

;;; Keymap and transient menu

(defvar-keymap herdr-core-command-map
  :doc "herdr-core commands, under `herdr-core-keymap-prefix'."
  "l" #'herdr-core-agents
  "o" #'herdr-core
  "s" #'herdr-core-switch-agent
  "a" #'herdr-core-attach-agent
  "t" #'herdr-core-attach-terminal
  "A" #'herdr-core-attach-several
  "G" #'herdr-core-attach-group
  "w" #'herdr-core-observe-agent
  "v" #'herdr-core-read-agent
  "SPC" #'herdr-core-last-agent
  "]" #'herdr-core-next-agent
  "[" #'herdr-core-previous-agent
  "." #'herdr-core-next-attention
  "b" #'herdr-core-goto-blocked
  "M" #'herdr-core-switch-machine
  "S" #'herdr-core-switch-space
  "T" #'herdr-core-switch-tab
  "n" #'herdr-core-new-agent
  "r" #'herdr-core-send-region
  "R" #'herdr-core-submit-region
  "k" #'herdr-core-send-keys
  "i" #'herdr-core-interrupt-agent
  "p" #'herdr-core-put-file
  "W" #'herdr-core-wait-agent
  "e" #'herdr-core-rename-agent
  "m" #'herdr-core-menu)

;; `herdr-core-keymap-prefix' may have been set before the map existed.
(when herdr-core-keymap-prefix
  (herdr-core--bind-global herdr-core-keymap-prefix herdr-core-command-map t))

(defvar herdr-core--menu-repeat-key nil
  "Key that runs `herdr-core-menu-repeat-command' from inside `herdr-core-menu'.")

(defun herdr-core--invoking-key ()
  "The key that invoked the current command as a `key-valid-p' string, or nil."
  (let ((ev last-command-event))
    (when (integerp ev)
      (let ((desc (key-description (vector ev))))
        (unless (member desc '("RET" "TAB" "ESC" "C-j" "DEL" "C-g"))
          desc)))))

(defun herdr-core--menu-description ()
  "Menu heading with the live blocked count."
  (let ((n (herdr-core-blocked-count)))
    (if (> n 0)
        (format "Herdr  %s" (propertize (format "● %d blocked" n) 'face 'herdr-core-mode-line))
      "Herdr")))

(defun herdr-core--menu-blocked-description ()
  "Menu label for `herdr-core-goto-blocked'."
  (format "Blocked (%d)" (herdr-core-blocked-count)))

(defun herdr-core--menu-mode-description ()
  "Menu label for `herdr-core-mode'."
  (format "Events & mode-line (%s)" (if (bound-and-true-p herdr-core-mode) "on" "off")))

(defun herdr-core--repeat-description ()
  "Menu label for `herdr-core-menu-repeat-command'."
  (pcase herdr-core-menu-repeat-command
    ('herdr-core-toggle "Open / hide *herdr-core*")
    ('herdr-core-agents "Open agent list")
    (cmd (format "%s" cmd))))

(defun herdr-core--menu-children (_)
  "Build menu suffixes from `herdr-core-menu-suffixes'; the repeat key wins."
  (let* ((k herdr-core--menu-repeat-key)
         (specs `(,@(and k `((,k ,(herdr-core--repeat-description)
                              ,herdr-core-menu-repeat-command)))
                  ,@(mapcar (lambda (s)
                              (pcase-let ((`(,key ,desc ,cmd) s))
                                (if (stringp desc) (list key desc cmd)
                                  (list key cmd :description desc))))
                            herdr-core-menu-suffixes)))
         (seen nil)
         (unique (seq-filter (lambda (s)
                               (unless (member (car s) seen)
                                 (push (car s) seen)))
                             specs)))
    (transient-parse-suffixes 'herdr-core-menu unique)))

;;;###autoload (autoload 'herdr-core-menu "herdr-core" nil t)
(transient-define-prefix herdr-core-menu ()
  "herdr-core commands (`herdr-core-menu-suffixes').  Pressing
`herdr-core-menu-key' again runs `herdr-core-menu-repeat-command'."
  [:class transient-column
   :description herdr-core--menu-description
   :setup-children herdr-core--menu-children]
  (interactive)
  (setq herdr-core--menu-repeat-key
        (or herdr-core-menu-key (herdr-core--invoking-key)))
  (transient-setup 'herdr-core-menu))

;;; Global minor mode

(defun herdr-core-mode-line-segment ()
  "Mode-line string counting agents in `herdr-core-mode-line-statuses', empty at zero."
  (let ((n (herdr-core-count herdr-core-mode-line-statuses)))
    (if (zerop n) ""
      (propertize (format herdr-core-mode-line-format n)
                  'face 'herdr-core-mode-line
                  'mouse-face 'mode-line-highlight
                  'local-map (make-mode-line-mouse-map 'mouse-1 #'herdr-core-agents)
                  'help-echo (format "%d %s hc agent(s): mouse-1 for the list" n
                                     (string-join herdr-core-mode-line-statuses "/"))))))

(defun herdr-core--mode-line-segment ()
  "The mode-line segment (`herdr-core-mode-line-function')."
  (funcall herdr-core-mode-line-function))

(defconst herdr-core--mode-line-string '(:eval (herdr-core--mode-line-segment)))

;;;###autoload
(define-minor-mode herdr-core-mode
  "Global minor mode owning the hc event stream, the mode-line count and notifications."
  :global t
  :lighter nil
  :group 'herdr-core-notify
  (if herdr-core-mode
      (progn
        (unless (member herdr-core--mode-line-string global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '("")) (list herdr-core--mode-line-string))))
        (herdr-core-start))
    (setq global-mode-string (delete herdr-core--mode-line-string global-mode-string))
    (unless (get-buffer herdr-core--agents-buffer-name) (herdr-core-stop))
    (force-mode-line-update t)))

(custom-add-to-group 'herdr-core-notify 'herdr-core-mode-hook 'custom-variable)

(provide 'herdr-core)
;;; herdr-core.el ends here
