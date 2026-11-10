;;; test-herdr-core.el --- Tests for herdr.el on herdr-core -*- lexical-binding: t; -*-

;;; Commentary:
;;   Every command path against a fake hc that serves the
;; JSON fixtures in fixtures/ (events.jsonl: a 0.1.19 `hc events --json'
;; snapshot of this host, orion, quasar and a pod; changes.jsonl: the
;; event lines after it; ls.json: `hc ls --json'), a fake launcher for
;; `herdr-core-new-agent', and, when a real herdr-core is at hand (HC_BIN, else
;; hc on PATH), a pod machine joining and leaving a scratch core.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; The live tests give hc a scratch HOME; a "~/..." default-directory (batch Emacs
;; abbreviates it) would then not resolve in process sentinels, so expand it now.
(setq default-directory (expand-file-name default-directory))
(defconst test-herdr-core--dir (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." test-herdr-core--dir))
(require 'herdr-core)

;; cl-letf on a primitive (completing-read) needs a native trampoline; an Emacs whose
;; libgccjit cannot link (ld: library 'emutls_w' not found) fails there, so use none.
(setq native-comp-enable-subr-trampolines nil)

(defconst test-herdr-core--fixtures (expand-file-name "fixtures" test-herdr-core--dir))

(defun test-herdr-core--fixture (name)
  "The contents of fixture NAME."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (expand-file-name name test-herdr-core--fixtures)))
    (buffer-string)))

(defun test-herdr-core--lines (name)
  "Fixture NAME's lines."
  (split-string (test-herdr-core--fixture name) "\n" t))

(defun test-herdr-core--json (value)
  "VALUE as one JSON line."
  (decode-coding-string (json-serialize value :null-object :null :false-object :false) 'utf-8))

(defun test-herdr-core--feed (&rest lines)
  "Feed LINES to the stream filter, the last one split in two."
  (let ((text (concat (string-join lines "\n") "\n")))
    (herdr-core--filter nil (substring text 0 (- (length text) 7)))
    (herdr-core--filter nil (substring text (- (length text) 7)))))

(defmacro test-herdr-core--fresh (&rest body)
  "Run BODY with empty herdr state and no notifications."
  (declare (indent 0))
  `(let ((herdr-core--terminals (make-hash-table :test 'equal))
         (herdr-core--pending "")
         (herdr-core--refresh-timer nil)
         (herdr-core--snapshot-seen nil)
         (herdr-core--error-notified nil)
         (herdr-core--machines nil)
         (herdr-core--machine-names-cache nil)
         (herdr-core--poll-timer nil)
         (herdr-core-refresh-policy 'events)
         (herdr-core-group-by nil)
         (herdr-core-sort-predicate #'herdr-core-sort-by-attention)
         (herdr-core-labelled-only nil)
         (herdr-core-status-change-functions nil))
     (unwind-protect (progn ,@body)
       (when herdr-core--refresh-timer (cancel-timer herdr-core--refresh-timer))
       (herdr-core-stop))))

(defun test-herdr-core--addresses ()
  "The listed addresses, in order."
  (mapcar (lambda (a) (alist-get 'address a)) (herdr-core-list)))

(defun test-herdr-core--wait (pred &optional seconds)
  "Wait up to SECONDS (default 10) for PRED, letting processes run."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

;;;; The fake hc

(defun test-herdr-core--fake-hc (dir)
  "A fake hc in DIR serving the fixtures.  Every call is logged to args.log.
events: the snapshot, then waits (exits 1 when DIR/events-down exists);
ls: ls.json (exits 1 when DIR/down exists); get ADDR: that terminal of
the snapshot; read: a screen; server start: clears DIR/down."
  (let ((file (expand-file-name "hc" dir))
        (get (expand-file-name "get" dir))
        (q (lambda (f) (shell-quote-argument (expand-file-name f dir)))))
    (make-directory get t)
    (seq-doseq (r (alist-get 'terminals (json-parse-string (car (test-herdr-core--lines "events.jsonl"))
                                                        :object-type 'alist :array-type 'array
                                                        :null-object :null :false-object :false)))
      (with-temp-file (expand-file-name (concat (subst-char-in-string ?/ ?_ (alist-get 'address r)) ".json") get)
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert (test-herdr-core--json r) "\n")))
    (copy-file (expand-file-name "events.jsonl" test-herdr-core--fixtures) (expand-file-name "events.jsonl" dir) t)
    (copy-file (expand-file-name "ls.json" test-herdr-core--fixtures) (expand-file-name "ls.json" dir) t)
    (copy-file (expand-file-name "machines.json" test-herdr-core--fixtures) (expand-file-name "machines.json" dir) t)
    (with-temp-file file
      (insert "#!/bin/sh\n"
              (format "printf '%%s\\n' \"$*\" >> %s\n" (funcall q "args.log"))
              "case $1 in\n"
              (format "  events) [ ! -e %s ] || exit 1; cat %s; exec sleep 600 ;;\n"
                      (funcall q "events-down") (funcall q "events.jsonl"))
              (format "  ls) [ ! -e %s ] || exit 1; cat %s ;;\n" (funcall q "down") (funcall q "ls.json"))
              (format "  get) f=%s/$(printf %%s \"$3\" | tr / _).json; [ -f \"$f\" ] || exit 1; cat \"$f\" ;;\n"
                      (funcall q "get"))
              (format "  read) if [ -f %s ]; then cat %s; else echo \"screen of $4\"; fi ;;\n"
                      (funcall q "read.txt") (funcall q "read.txt"))
              "  send) echo '{\"sent\":true}' ;;\n"
              (format "  machine) cat %s ;;\n" (funcall q "machines.json"))
              "  interrupt) echo '{\"key\":\"ctrl+c\",\"outcome\":\"interrupted\"}' ;;\n"
              "  put) echo '{\"bytes\":3,\"path\":\"/core/files/'\"$3\"'/f.txt\"}' ;;\n"
              "  wait) echo '{\"reason\":\"done\",\"status\":{\"state\":\"done\"},\"terminal\":\"'\"$3\"'\"}' ;;\n"
              "  label|kill|keys) echo '{}' ;;\n"
              "  file) echo \"remote file: $*\" ;;\n"
              "  exec) printf 'UE5HREFUQQ==' ;;\n"
              "  schema) echo '{\"openrpc\":\"1.3.2\",\"methods\":[{\"name\":\"terminal.run\"}]}' ;;\n"
              (format "  env) echo \"$HERDR_TEST_ENV\" >> %s ;;\n" (funcall q "env.log"))
              (format "  server) rm -f %s; echo '{}' ;;\n" (funcall q "down"))
              "esac\n"))
    (set-file-modes file #o755)
    file))

(defun test-herdr-core--fake-launcher (dir)
  "A fake bin/hc-agent in DIR: logs its args to launch.log, prints hc's line."
  (let ((file (expand-file-name "hc-agent" dir)))
    (with-temp-file file
      (insert "#!/bin/sh\n"
              (format "printf '%%s\\n' \"$*\" >> %s\n"
                      (shell-quote-argument (expand-file-name "launch.log" dir)))
              "echo 'launcher: launching'\n"
              "echo '{\"address\":\"t7\",\"agent\":\"claude\",\"name\":\"'\"$3\"'\",\"state\":\"live\",\"turn\":0}'\n"))
    (set-file-modes file #o755)
    file))

(defun test-herdr-core--log (dir &optional file)
  "DIR's args.log (or FILE), or \"\"."
  (let ((log (expand-file-name (or file "args.log") dir)))
    (if (file-exists-p log)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)) (insert-file-contents log))
          (buffer-string))
      "")))

(defvar test-herdr-core--opened nil "Terminal buffers the fake terminal function opened.")

(defun test-herdr-core--terminal (name program args &optional action)
  "A stand-in `herdr-core-terminal-function': a buffer NAME with a live process."
  (push (list name program args action) test-herdr-core--opened)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (set-process-query-on-exit-flag (start-process "fake-term" buffer "sleep" "600") nil))
    (pop-to-buffer buffer action)
    buffer))

(defmacro test-herdr-core--with-fake (dir &rest body)
  "Run BODY with fresh state against a fake hc and launcher in a new DIR."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "herdr" t))
          (herdr-core-executable (test-herdr-core--fake-hc ,dir))
          (herdr-core-launcher (test-herdr-core--fake-launcher ,dir))
          (herdr-core-terminal-function #'test-herdr-core--terminal)
          (test-herdr-core--opened nil)
          (herdr-core-reconnect-delay 60))
     (test-herdr-core--fresh
       (unwind-protect (progn ,@body)
         (dolist (b (buffer-list))
           (when (or (buffer-local-value 'herdr-core--attached-address b)
                     (member (buffer-name b) (list herdr-core--agents-buffer-name herdr-core--buffer-name)))
             (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
         (dolist (p (process-list))
           (when (string-prefix-p "herdr-core-" (process-name p)) (delete-process p)))
         (ignore-errors (delete-directory ,dir t))))))

(defun test-herdr-core--pick (regexp)
  "A `completing-read' stand-in choosing the first candidate matching REGEXP."
  (lambda (_prompt collection &rest _)
    (or (seq-find (lambda (c) (string-match-p regexp c)) (all-completions "" collection))
        (error "No candidate matches %s" regexp))))

(defun test-herdr-core--load-snapshot ()
  "Feed the fixture snapshot."
  (apply #'test-herdr-core--feed (test-herdr-core--lines "events.jsonl")))

;;;; Records and the event stream

(ert-deftest herdr-core-snapshot-every-machine-sorted-by-attention ()
  "The fixture snapshot: this host, orion, quasar and a pod; blocked first."
  (test-herdr-core--fresh
    (test-herdr-core--load-snapshot)
    (should herdr-core--snapshot-seen)
    ;; t9 is finished: not listed.  blocked > done > working > idle > none.
    (should (equal (test-herdr-core--addresses)
                   '("quasar/t4" "job-p-a1/t1" "t1" "orion/t2" "t3")))
    (let ((blocked (herdr-core--find-agent "quasar/t4")))
      (should (equal (alist-get 'status blocked) "blocked"))
      (should (equal (alist-get 'blocked-kind blocked) "permission"))
      (should (equal (alist-get 'msg blocked) "Run rm -rf build?"))
      (should (equal (alist-get 'machine blocked) "quasar"))
      (should (equal (alist-get 'kind blocked) "claude"))
      (should (equal (alist-get 'space blocked) "juniper"))
      (should (equal (alist-get 'session blocked) "juniper · job-b - Fix the fleet")))
    (should (equal (alist-get 'machine (herdr-core--find-agent "t1")) "local"))
    (let ((shell (herdr-core--find-agent "t3")))
      (should (equal (alist-get 'status shell) "unknown"))
      (should (equal (alist-get 'kind shell) "sh"))
      (should (equal (herdr-core--agent-display-name shell) "scratch")))
    (should (= (herdr-core-blocked-count) 1))
    (should (equal (substring-no-properties (herdr-core--mode-line-segment)) " ● 1 "))
    (should (equal (herdr-core--format-candidate (herdr-core--find-agent "quasar/t4"))
                   (format "[blocked:permission] juniper · job-b - Fix the fleet (claude) · quasar · %s · quasar/t4"
                           (abbreviate-file-name "/home/user/juniper"))))
    (let ((herdr-core-labelled-only t))
      (should (equal (test-herdr-core--addresses) '("quasar/t4" "job-p-a1/t1" "t1" "orion/t2"))))))

(ert-deftest herdr-core-events-change-status-labels-and-machines ()
  "changes.jsonl: a status with its blocked kind, labels, a turn, a pod
joining and one leaving, a finished shell; status changes run the hook."
  (test-herdr-core--fresh
    (let (changes)
      (setq herdr-core-status-change-functions
            (list (lambda (agent old new) (push (list (alist-get 'address agent) old new) changes))))
      (test-herdr-core--load-snapshot)
      (should (null changes))
      (apply #'test-herdr-core--feed (test-herdr-core--lines "changes.jsonl"))
      (should (equal (test-herdr-core--addresses)
                     '("orion/t2" "quasar/t4" "t1" "job-q-a1/t1")))
      (let ((orion (herdr-core--find-agent "orion/t2")))
        (should (equal (alist-get 'status orion) "blocked"))
        (should (equal (alist-get 'blocked-kind orion) "question"))
        (should (equal (alist-get 'state (gethash "orion/t2" herdr-core--terminals)) "live")))
      ;; The turn event's state is the turn's, never the terminal's.
      (should (equal (alist-get 'state (gethash "t1" herdr-core--terminals)) "live"))
      (should (equal (alist-get 'turn (gethash "t1" herdr-core--terminals)) 2))
      (should (equal (herdr-core-title "t1") "juniper · job-a - Renamed"))
      (should (equal (alist-get 'machine (herdr-core--find-agent "job-q-a1/t1")) "job-q-a1"))
      (should (equal (alist-get 'state (gethash "t3" herdr-core--terminals)) "finished"))
      (should (equal (reverse changes) '(("orion/t2" "idle" "blocked"))))
      (should (= (herdr-core-blocked-count) 2))
      ;; missed: a fresh full list.
      (test-herdr-core--feed
       (test-herdr-core--json `((jsonrpc . "2.0") (method . "missed")
                           (params . ((seq . 18) (terminals . [((address . "orion/t5") (state . "live")
                                                                (status . ((state . "idle"))))])))))))
      (should (equal (test-herdr-core--addresses) '("orion/t5")))))

(ert-deftest herdr-core-buffers-are-named-by-the-session-title ()
  "A labelled terminal's buffer is `*hc: TITLE*', as the session-naming
layer's golden table says; an unlabelled one keeps its address; a clash gets <N>."
  (let ((golden (expand-file-name "fixtures/session-name-golden.json"
                                  test-herdr-core--dir)))
    (skip-unless (file-readable-p golden))
    (test-herdr-core--fresh
      (let ((rows (with-temp-buffer
                    (insert-file-contents golden)
                    (json-parse-buffer :object-type 'alist :array-type 'list))))
        (should (> (length rows) 3))
        (dolist (row rows)
          (let* ((expect (alist-get 'expect row))
                 (address (format "m/t%d" (hash-table-count herdr-core--terminals))))
            (puthash address
                     `((address . ,address) (state . "live")
                       (labels . ((herdr.space . ,(alist-get 'herdr_space expect))
                                  (herdr.tab . ,(alist-get 'herdr_tab expect))
                                  (session.title . ,(alist-get 'title expect)))))
                     herdr-core--terminals)
            (should (equal (herdr-core-buffer-name address) (alist-get 'emacs_buffer expect))))))
      (should (equal (herdr-core-buffer-name "t42") "*hc: t42*"))
      (dolist (a '("t1" "t2"))
        (puthash a `((address . ,a) (labels . ((session.title . "juniper · job-1 - Fix it"))))
                 herdr-core--terminals))
      (should (equal (herdr-core-buffer-name "t1" "observe") "*hc observe: juniper · job-1 - Fix it*"))
      (let ((buffer (get-buffer-create "*hc: juniper · job-1 - Fix it*")))
        (unwind-protect
            (progn
              (with-current-buffer buffer (setq herdr-core--attached-address "t1"))
              (should (equal (herdr-core-buffer-name "t1") "*hc: juniper · job-1 - Fix it*"))
              (should (equal (herdr-core-buffer-name "t2") "*hc: juniper · job-1 - Fix it*<2>")))
          (kill-buffer buffer))))))

;;;; Commands against the fake hc

(ert-deftest herdr-core-picker-falls-back-to-ls-and-get ()
  "With no event stream, the picker lists `hc ls' completed by `hc get'."
  (test-herdr-core--with-fake dir
    (with-temp-file (expand-file-name "events-down" dir))
    (let ((agents (herdr-core--fetch-agents-sync)))
      (should (equal (mapcar (lambda (a) (alist-get 'address a)) agents)
                     '("quasar/t4" "job-p-a1/t1" "t1" "orion/t2" "t3")))
      (should (equal (alist-get 'blocked-kind (car agents)) "permission")))
    (let ((log (test-herdr-core--log dir)))
      (should (string-match-p "^events --json$" log))
      (should (string-match-p "^ls --json$" log))
      (should (string-match-p "^get --json orion/t2$" log)))))

(ert-deftest herdr-core-picker-offers-to-start-the-core ()
  "When hc ls fails, the picker offers `hc server start'."
  (test-herdr-core--with-fake dir
    (with-temp-file (expand-file-name "events-down" dir))
    (with-temp-file (expand-file-name "down" dir))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (should (= (length (herdr-core--fetch-agents-sync)) 5)))
    (should (string-match-p "^server start --json$" (test-herdr-core--log dir)))
    (with-temp-file (expand-file-name "down" dir))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (herdr-core--fetch-agents-sync) :type 'user-error))))

(ert-deftest herdr-core-switch-attach-observe-read-goto-blocked ()
  "The picker commands attach, watch, read and jump through hc."
  (test-herdr-core--with-fake dir
    (let ((hc herdr-core-executable))
      (cl-letf (((symbol-function 'completing-read) (test-herdr-core--pick "orion/t2\\'")))
        (herdr-core-switch-agent)
        ;; A second switch reuses the live buffer.
        (herdr-core-switch-agent)
        (should (= (length test-herdr-core--opened) 1))
        (herdr-core-attach-agent t)
        (herdr-core-observe-agent)
        (herdr-core-read-agent))
      (herdr-core-goto-blocked)
      (should (equal (mapcar #'butlast (reverse test-herdr-core--opened))
                     `(("*hc: tidepool · tidepool-pm*" ,hc ("attach" "orion/t2"))
                       ("*hc: tidepool · tidepool-pm*" ,hc ("attach" "orion/t2"))
                       ("*hc observe: tidepool · tidepool-pm*" ,hc ("attach" "--observe" "orion/t2"))
                       ("*hc: juniper · job-b - Fix the fleet*" ,hc ("attach" "quasar/t4")))))
      (with-current-buffer "*hc: juniper · job-b - Fix the fleet*"
        (should (equal herdr-core--attached-address "quasar/t4"))
        (should (equal header-line-format herdr-core--header-line-format))
        (let ((header (herdr-core--header-line)))
          (should (string-match-p "\\` ● job-b  " header))
          (should (string-match-p "· tidepool-pm" header))))
      (should (test-herdr-core--wait (lambda () (get-buffer "*hc read: tidepool · tidepool-pm*"))))
      (with-current-buffer "*hc read: tidepool · tidepool-pm*"
        (should (string-match-p "screen of orion/t2" (buffer-string)))
        (should (derived-mode-p 'herdr-core-read-mode)))
      (should (string-match-p "^read --lines 500 orion/t2$" (test-herdr-core--log dir)))
      ;; Last agent: the most recent other attach buffer.
      (switch-to-buffer "*hc: tidepool · tidepool-pm*")
      (herdr-core-last-agent)
      (should-not (equal (buffer-name) "*hc: tidepool · tidepool-pm*"))
      ;; A notification click.
      (herdr-core-show-agent "orion/t2")
      (should (equal (buffer-name (window-buffer (selected-window))) "*hc: tidepool · tidepool-pm*")))))

(ert-deftest herdr-core-attach-buffers-follow-the-session-title ()
  "A labels event renames the agent's buffers."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core--attach-agent (herdr-core--find-agent "t1"))
    (should (get-buffer "*hc: juniper · job-a - Local work*"))
    (test-herdr-core--feed (nth 1 (test-herdr-core--lines "changes.jsonl")))
    (herdr-core--refresh)
    (should (get-buffer "*hc: juniper · job-a - Renamed*"))
    (should-not (get-buffer "*hc: juniper · job-a - Local work*"))))

(ert-deftest herdr-core-send-region-and-submit ()
  "The region goes to the picked agent as a fenced block through hc send."
  (test-herdr-core--with-fake dir
    (cl-letf (((symbol-function 'completing-read) (test-herdr-core--pick "quasar/t4\\'")))
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun x ()\n  1)\n")
        (herdr-core-send-region (point-min) (point-max))
        (should (test-herdr-core--wait (lambda () (string-match-p "^send quasar/t4 ```emacs-lisp  L1-L3"
                                                             (test-herdr-core--log dir)))))
        (goto-char (point-min))
        (herdr-core-submit-region (line-beginning-position) (line-end-position))
        (should (test-herdr-core--wait (lambda () (string-match-p "^send --force quasar/t4 ```emacs-lisp  L1-L1"
                                                             (test-herdr-core--log dir)))))
        (erase-buffer)
        (insert "   ")
        (should-error (herdr-core-send-region (point-min) (point-max)) :type 'user-error)))))

(ert-deftest herdr-core-new-agent-goes-through-the-one-launcher ()
  "herdr-core-new-agent runs hc-agent run --name --cwd -- KIND and attaches to its address."
  (test-herdr-core--with-fake dir
    (let* ((root (file-name-as-directory (expand-file-name "proj" dir)))
           (default-directory root))
      (make-directory root t)
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) (cons 'transient root))))
        (herdr-core-new-agent "claude"))
      (should (test-herdr-core--wait (lambda () (assoc "*hc: t7*" test-herdr-core--opened))))
      (should (equal (test-herdr-core--log dir "launch.log")
                     (format "run --name proj-claude --cwd %s -- claude\n" root)))
      (should (equal (nth 2 (assoc "*hc: t7*" test-herdr-core--opened)) '("attach" "t7")))))
  (should (equal (herdr-core--sanitize-name "1My Proj!-claude") "my-proj-claude"))
  (should (equal (herdr-core--unique-agent-name "job-a" '(((name . "job-a")) ((name . "job-a-2"))))
                 "job-a-3"))
  (should (equal (herdr-core--launched-address "noise\n{\"address\":\"orion/t3\"}\n" "x") "orion/t3"))
  (should (equal (herdr-core--launched-address "no json" "x") "x")))

(ert-deftest herdr-core-picker-buffer-runs-hc ()
  "`herdr' runs hc's own picker in *herdr*; toggle hides it."
  (test-herdr-core--with-fake dir
    (herdr-core)
    (should (equal (car test-herdr-core--opened) (list herdr-core--buffer-name herdr-core-executable nil
                                                         '((display-buffer-same-window)))))
    (should (eq (current-buffer) (get-buffer herdr-core--buffer-name)))
    (herdr-core)
    (should (= (length test-herdr-core--opened) 1))
    (switch-to-buffer herdr-core--buffer-name)
    (herdr-core-toggle)
    (should-not (eq (window-buffer (selected-window)) (get-buffer herdr-core--buffer-name)))))

(ert-deftest herdr-core-agents-list-buffer-and-keys ()
  "herdr-core-agents follows the fake hc's stream; its keys call hc."
  (test-herdr-core--with-fake dir
    (herdr-core-agents)
    (should (test-herdr-core--wait
             (lambda () (with-current-buffer herdr-core--agents-buffer-name
                          (goto-char (point-min))
                          (tabulated-list-get-id)))))
    (with-current-buffer herdr-core--agents-buffer-name
      (goto-char (point-min))
      (should (equal (tabulated-list-get-id) "quasar/t4"))
      (should (string-match-p "blocked:permission +juniper · job-b - Fix the fleet +claude +quasar +juniper +quasar/t4"
                              (buffer-substring (point) (line-end-position))))
      (let ((text (buffer-string)))
        (dolist (want '("local" "orion" "quasar" "job-p-a1"))
          (should (string-match-p (concat " " want " ") text))))
      (forward-line 2)
      (should (equal (tabulated-list-get-id) "t1"))
      (herdr-core-agents-next-blocked)
      (should (equal (tabulated-list-get-id) "quasar/t4"))
      (forward-line 3)
      (should (equal (tabulated-list-get-id) "orion/t2"))
      (herdr-core-agents-observe)
      (set-buffer herdr-core--agents-buffer-name)
      (herdr-core-agents-send "hello there")
      (herdr-core-agents-read)
      (set-buffer herdr-core--agents-buffer-name)
      (herdr-core-agents-show)
      (set-buffer herdr-core--agents-buffer-name)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Address")))
        (herdr-core-agents-sort))
      (goto-char (point-min))
      (should (equal (tabulated-list-get-id) "job-p-a1/t1"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) herdr-core--attention-sort-label)))
        (herdr-core-agents-sort))
      (goto-char (point-min))
      (should (equal (tabulated-list-get-id) "quasar/t4")))
    (should (equal (mapcar (lambda (o) (nth 2 o)) (reverse test-herdr-core--opened))
                   '(("attach" "--observe" "orion/t2") ("attach" "orion/t2"))))
    (should (test-herdr-core--wait (lambda () (string-match-p "^send orion/t2 hello there$" (test-herdr-core--log dir)))))
    (should (test-herdr-core--wait (lambda () (get-buffer "*hc read: tidepool · tidepool-pm*"))))
    ;; Changes arrive pushed: the list redraws.
    (apply #'test-herdr-core--feed (test-herdr-core--lines "changes.jsonl"))
    (herdr-core--refresh)
    (with-current-buffer herdr-core--agents-buffer-name
      (goto-char (point-min))
      (should (equal (tabulated-list-get-id) "orion/t2"))
      (should (string-match-p "job-q-a1" (buffer-string)))
      (should-not (string-match-p "job-p-a1" (buffer-string))))
    (kill-buffer herdr-core--agents-buffer-name)
    (should-not (process-live-p herdr-core--process))))

(ert-deftest herdr-core-mode-line-notify-and-reconnect ()
  "The mode adds its segment and starts the stream; a blocked agent
notifies; a dropped stream restarts."
  (test-herdr-core--with-fake dir
    (let ((herdr-core-reconnect-delay 0.1)
          (global-mode-string nil)
          (notified nil))
      (unwind-protect
          (let ((herdr-core-notify-function
                 (lambda (title body address) (push (list title body address) notified))))
            (setq herdr-core-status-change-functions '(herdr-core-notify))
            (herdr-core-mode 1)
            (should (member herdr-core--mode-line-string global-mode-string))
            (should (test-herdr-core--wait (lambda () herdr-core--snapshot-seen)))
            (should (equal (substring-no-properties (herdr-core--mode-line-segment)) " ● 1 "))
            (test-herdr-core--feed (car (test-herdr-core--lines "changes.jsonl")))
            (should (equal notified '(("tidepool · tidepool-pm: blocked (question)" "orion · Which branch?" "orion/t2"))))
            (should (string-match-p "emacsclient.*herdr-core-show-agent.*orion/t2" (herdr-core--show-agent-command "orion/t2")))
            (let ((first herdr-core--process))
              (delete-process first)
              (should (test-herdr-core--wait (lambda () (and (process-live-p herdr-core--process)
                                                        (not (eq herdr-core--process first))))))))
        (herdr-core-mode -1))
      (should-not (member herdr-core--mode-line-string global-mode-string))
      (should-not (process-live-p herdr-core--process))))
  (should (herdr-core--notify-transition-p "working" "idle"))
  (should-not (herdr-core--notify-transition-p "idle" "working")))

(ert-deftest herdr-core-menu-builds-its-suffixes ()
  "The transient menu: the repeat key first, then every command once."
  (let ((herdr-core--menu-repeat-key "C-9")
        (herdr-core-menu-repeat-command #'herdr-core-agents))
    (let ((keys (mapcar (lambda (s) (plist-get (if (vectorp s) (aref s 2) (nth 2 s)) :key))
                        (herdr-core--menu-children nil))))
      (should (equal (car keys) "C-9"))
      (dolist (k '("l" "o" "s" "a" "w" "v" "n" "b" "r" "R" "m"))
        (should (member k keys)))))
  (let ((herdr-core-menu-key nil))
    (customize-set-variable 'herdr-core-menu-key "C-c 9")
    (unwind-protect (should (eq (keymap-global-lookup "C-c 9") #'herdr-core-menu))
      (customize-set-variable 'herdr-core-menu-key nil))
    (should-not (eq (keymap-global-lookup "C-c 9") #'herdr-core-menu))))

;;;; Live, against a real herdr-core

(defun test-herdr-core--hc-bin ()
  "A real hc: HC_BIN, else hc on PATH, else nil."
  (let ((bin (or (getenv "HC_BIN") (executable-find "hc"))))
    (and bin (file-executable-p bin)
         (string-match-p "\\`hc 0\\." (car (process-lines bin "--version")))
         bin)))

(defun test-herdr-core--env (dir)
  "Environment lines for a scratch hc core in DIR."
  (make-directory (expand-file-name "run" dir) t)
  (set-file-modes (expand-file-name "run" dir) #o700)
  (mapcar (lambda (kv) (format "%s=%s" (car kv) (expand-file-name (cdr kv) dir)))
          '(("HOME" . ".") ("XDG_RUNTIME_DIR" . "run") ("XDG_STATE_HOME" . "state")
            ("XDG_CONFIG_HOME" . "cfg") ("XDG_DATA_HOME" . "data"))))

(ert-deftest herdr-core-live-pod-agent-in-the-list-and-picker ()
  "A real core: a labelled, blocked pod agent and a local one show in
herdr-core-agents and the picker when the pod's machine joins, and go when it leaves."
  (let ((bin (test-herdr-core--hc-bin)))
    (skip-unless bin)
    (let* (;; Short: a core's socket path must fit macOS's 104 bytes ($TMPDIR is /var/folders/...).
           (root (let ((temporary-file-directory (if (file-directory-p "/tmp/") "/tmp/" temporary-file-directory)))
                   (make-temp-file "hl" t)))
           ;; HOME changes below: a "~/..." default-directory would then not resolve
           ;; (here and in the hc helper, whose temp buffer inherits whichever buffer is current).
           (default-directory (file-name-as-directory root))
           (local (expand-file-name "local" root))
           (pod (expand-file-name "pod" root))
           (bindir (expand-file-name "bin" root))
           ;; The pod's core is started by hc's line, `herdr-core' looked up on PATH.
           (path (progn (make-directory bindir t)
                        (make-symbolic-link (file-truename bin) (expand-file-name "herdr-core" bindir))
                        (concat "PATH=" bindir ":/usr/bin:/bin"))) ; minimal: it is spliced into the --via line, which a PATH with spaces breaks
           (pod-env (cons path (test-herdr-core--env pod)))
           (process-environment (append (list path) (test-herdr-core--env local)
                                        (list "HC_SESSION" "HC_TERMINAL_ID")
                                        (seq-remove (lambda (e) (string-match-p "\\`HC_\\(SESSION\\|TERMINAL_ID\\)=" e))
                                                    process-environment)))
           (herdr-core-executable bin)
           (hc (lambda (env &rest args)
                 (let ((process-environment (append env process-environment))
                       (default-directory (file-name-as-directory root)))
                   (with-temp-buffer
                     (list (apply #'call-process bin nil t nil args) (buffer-string)))))))
      (test-herdr-core--fresh
        (unwind-protect
            (progn
              (funcall hc pod-env "server" "start" "--json")
              ;; The pod's agent, as agent-run.sh starts it: herdr labels, then OSC 7501 blocked.
              (funcall hc pod-env "run" "--bg" "--name" "agent" "--label" "herdr.space=juniper"
                       "--label" "herdr.tab=job-p" "--label" "session.title=juniper · job-p - Pod work"
                       "--" "sh" "-c"
                       "printf '\\033]7501;state=blocked:kind=permission:app=claude\\033\\\\'; sleep 600")
              (funcall hc nil "server" "start" "--json")
              (funcall hc nil "run" "--bg" "--no-parent" "--name" "here" "--" "sleep" "600")
              (herdr-core-agents)
              (let ((added (funcall hc nil "machine" "add" "--temp" "--name" "job-p-a1" "--via"
                                    (concat "env -u HC_SESSION " (string-join pod-env " ") " sh -c")
                                    "--json")))
                (should (equal (list 0 "") (list (car added) (if (zerop (car added)) "" (cadr added))))))
              (should (test-herdr-core--wait
                       (lambda () (equal (test-herdr-core--addresses) '("job-p-a1/t1" "t1"))) 20))
              (should (test-herdr-core--wait (lambda () (= (herdr-core-blocked-count) 1)) 10))
              (should (test-herdr-core--wait
                       (lambda () (with-current-buffer herdr-core--agents-buffer-name
                                    (string-match-p "blocked:permission +juniper · job-p - Pod work +claude +job-p-a1 +juniper +job-p-a1/t1"
                                                    (buffer-string))))
                       5))
              (let (seen)
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_p collection &rest _)
                             (setq seen (all-completions "" collection))
                             (car seen))))
                  (should (equal (alist-get 'address (herdr-core--pick-agent)) "job-p-a1/t1")))
                (should (string-match-p "\\`\\[blocked:permission\\] juniper · job-p - Pod work (claude) · job-p-a1"
                                        (car seen)))
                (should (string-match-p "here (sleep) · local" (cadr seen))))
              (let ((read (funcall hc nil "read" "job-p-a1/t1")))
                (should (zerop (car read))))
              (funcall hc nil "machine" "rm" "job-p-a1" "--json")
              (should (test-herdr-core--wait (lambda () (equal (test-herdr-core--addresses) '("t1"))) 20)))
          (when (get-buffer herdr-core--agents-buffer-name) (kill-buffer herdr-core--agents-buffer-name))
          (herdr-core-stop)
          (funcall hc nil "server" "stop")
          (funcall hc pod-env "server" "stop")
          (delete-directory root t))))))

(ert-deftest herdr-core-foreground-event-keeps-the-terminal-name ()
  "A foreground event fills `foreground' and leaves the terminal's name."
  (test-herdr-core--fresh
    (herdr-core-apply '((terminals ((address . "t1") (name . "here") (argv "sleep" "600") (state . "live")))))
    (herdr-core-apply '((method . "event")
                   (params (type . "foreground") (terminal . "t1")
                           (data (argv "sleep" "600") (cwd . "/tmp") (name . "sleep") (pid . 7)))))
    (let ((r (gethash "t1" herdr-core--terminals)))
      (should (equal (alist-get 'name r) "here"))
      (should (equal (alist-get 'name (alist-get 'foreground r)) "sleep")))))

(provide 'test-herdr-core)
;;; test-herdr-core.el ends here
