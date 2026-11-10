;;; test-herdr-core-features.el --- Tests for herdr-core's customizable features -*- lexical-binding: t; -*-

;;; Commentary:
;;   Every customization area, switching, side-by-side
;; layouts, rendering read output, the launcher template and the talk
;; commands, against the fake hc of test-herdr-core.el (its fixtures
;; plus machines.json and an optional read.txt), and a live suite
;; against a real scratch herdr-core when one is at hand.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'test-herdr-core)

(defun test-herdr-core--ids ()
  "The agent addresses of the list buffer's lines, top to bottom."
  (with-current-buffer herdr-core--agents-buffer-name
    (save-excursion
      (goto-char (point-min))
      (let (ids)
        (while (not (eobp))
          (when-let* ((id (herdr-core--list-address))) (push id ids))
          (forward-line 1))
        (nreverse ids)))))

(defun test-herdr-core--group-rows ()
  "The group rows of the list buffer, top to bottom, trimmed."
  (with-current-buffer herdr-core--agents-buffer-name
    (save-excursion
      (goto-char (point-min))
      (let (rows)
        (while (not (eobp))
          (let ((row (get-text-property (point) 'vuistack-table-row)))
            (when (and row (not (plist-get row :address)))
              (push (string-trim (replace-regexp-in-string
                                  "\\`\\[.\\] +" "" (test-herdr-core--line)))
                    rows)))
          (forward-line 1))
        (nreverse rows)))))

(defun test-herdr-core--headers ()
  "The headers of the list table's shown columns."
  (with-current-buffer herdr-core--agents-buffer-name
    (mapcar (lambda (c) (plist-get c :header))
            (plist-get (vuistack-table--table herdr-core--table-id) :display))))

(defun test-herdr-core--line ()
  "The current line's text."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defmacro test-herdr-core--with-machines (&rest body)
  "Run BODY with the machine fixture known."
  (declare (indent 0))
  `(progn
     (herdr-core--set-machines
      (alist-get 'machines (herdr-core--parse (test-herdr-core--fixture "machines.json"))))
     ,@body))

;;;; Customization and documentation

(defun test-herdr-core--defcustoms ()
  "Every herdr-core user option."
  (let (vars)
    (mapatoms (lambda (s)
                (when (and (custom-variable-p s)
                           (string-prefix-p "herdr-core-" (symbol-name s)))
                  (push s vars))))
    (sort vars (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(ert-deftest herdr-core-every-option-is-grouped-and-documented ()
  "Each defcustom is in one area group under `herdr-core' and README.org documents it."
  (let ((areas '(herdr-core-backend herdr-core-events herdr-core-agents herdr-core-list
                 herdr-core-picker herdr-core-display herdr-core-render herdr-core-launcher
                 herdr-core-notify herdr-core-keys herdr-core-faces herdr-core-control))
        (readme (with-temp-buffer
                  (insert-file-contents (expand-file-name "../README.org" test-herdr-core--dir))
                  (buffer-string)))
        (vars (test-herdr-core--defcustoms)))
    (should (> (length vars) 60))
    (dolist (area areas)
      (should (assq area (get 'herdr-core 'custom-group)))
      (should (string-match-p (regexp-quote (symbol-name area)) readme)))
    (dolist (var vars)
      (should (seq-some (lambda (area) (assq var (get area 'custom-group))) areas))
      (unless (string-match-p (concat "=" (regexp-quote (symbol-name var)) "=") readme)
        (ert-fail (format "README.org does not document %s" var))))
    ;; Every face is in the faces group.
    (dolist (face '(herdr-core-status-blocked herdr-core-status-error herdr-core-status-done
                    herdr-core-status-working herdr-core-status-idle herdr-core-status-unknown
                    herdr-core-group-header herdr-core-mode-line herdr-core-header-current
                    herdr-core-link))
      (should (assq face (get 'herdr-core-faces 'custom-group)))
      (should (string-match-p (concat "=" (symbol-name face) "=") readme)))))

;;;; Agents: filters, names, statuses

(ert-deftest herdr-core-filters-names-and-machine-kinds ()
  "Filters, name sources, buffer names, glyphs and machine kinds are options."
  (test-herdr-core--fresh
    (test-herdr-core--load-snapshot)
    (let ((herdr-core-hidden-machines '("orion")))
      (should-not (member "orion/t2" (test-herdr-core--addresses))))
    (let ((herdr-core-filter-functions (list (lambda (a) (equal (alist-get 'kind a) "claude")))))
      (should (equal (test-herdr-core--addresses) '("quasar/t4" "job-p-a1/t1" "t1"))))
    (let ((herdr-core-show-finished t))
      (should (member "t9" (test-herdr-core--addresses))))
    (let ((herdr-core-name-sources '(name)))
      (should (equal (herdr-core--agent-display-name (herdr-core--find-agent "t1")) "job-a")))
    (let ((herdr-core-name-sources '("herdr.tab")))
      (should (equal (herdr-core--agent-display-name (herdr-core--find-agent "t1")) "job-a"))
      (should (equal (herdr-core--agent-display-name (herdr-core--find-agent "t3")) "t3")))
    (let ((herdr-core-name-function (lambda (a) (upcase (alist-get 'address a)))))
      (should (equal (herdr-core--agent-display-name (herdr-core--find-agent "t1")) "T1")))
    (let ((herdr-core-title-label "herdr.tab"))
      (should (equal (herdr-core-buffer-name "t1") "*hc: job-a*")))
    (let ((herdr-core-buffer-name-function (lambda (kind title) (format "<%s|%s>" kind title))))
      (should (equal (herdr-core-buffer-name "t1" "read") "<read|juniper · job-a - Local work>")))
    (let ((herdr-core-status-glyphs '(("blocked" . "!!"))))
      (should (string-prefix-p " !! job-b" (herdr-core-header-label (herdr-core--find-agent "quasar/t4")))))
    (let ((herdr-core-status-priority '(("idle" . 0) ("blocked" . 1))))
      (should (equal (car (test-herdr-core--addresses)) "orion/t2")))
    (should (equal (herdr-core-machine-kind "local" nil) "local"))
    (should (equal (herdr-core-machine-kind "job-p-a1" nil) "pod"))
    (should (equal (herdr-core-machine-kind "fly" '((via . t))) "command"))
    (should (equal (herdr-core-machine-kind "orion" '((via))) "ssh"))
    (test-herdr-core--with-machines
      (let ((machines (herdr-core-machines)))
        (should (equal (mapcar (lambda (m) (alist-get 'name m)) machines)
                       '("local" "orion" "quasar" "job-p-a1" "fly")))
        (should (equal (mapcar (lambda (m) (alist-get 'kind m)) machines)
                       '("local" "ssh" "ssh" "pod" "command")))
        (should (equal (alist-get 'state (car (last machines))) "connecting"))
        (should (equal (mapcar (lambda (a) (alist-get 'address a)) (alist-get 'agents (nth 1 machines)))
                       '("orion/t2")))))
    (let ((herdr-core-machine-kind-function (lambda (name _r) (if (equal name "orion") "lab" "x"))))
      (should (equal (herdr-core--machine-kind-of "orion") "lab")))))

;;;; The list: columns, sorting, grouping, narrowing, marks

(ert-deftest herdr-core-list-columns-sorting-and-grouping ()
  "Columns, sort predicates, groups (machine, label, function) and their order."
  (test-herdr-core--with-fake dir
    (test-herdr-core--with-machines
      (test-herdr-core--load-snapshot)
      (let ((herdr-core-list-columns '(("" 2 herdr-core-column-glyph)
                                       ("Tab" 10 herdr-core-column-tab)
                                       ("Dir" 24 herdr-core-column-cwd)
                                       ("Msg" 20 herdr-core-column-message)
                                       ("Age" 5 herdr-core-column-age)
                                       ("Address" 0 herdr-core-column-address))))
        (herdr-core-agents)
        (with-current-buffer herdr-core--agents-buffer-name
          (should (equal (test-herdr-core--first-agent) "quasar/t4"))
          (should (string-match-p "● +job-b +~?/home/user/juniper +Run rm -rf build\\? +[0-9]+[smhd] +quasar/t4"
                                  (test-herdr-core--line)))
          (should (equal (test-herdr-core--headers) '("" "Tab" "Dir" "Msg" "Age" "Address")))))
      ;; Sort predicates.
      (let ((herdr-core-sort-predicate #'herdr-core-sort-by-name))
        (should (equal (test-herdr-core--addresses) '("t1" "quasar/t4" "job-p-a1/t1" "t3" "orion/t2"))))
      (let ((herdr-core-sort-predicate #'herdr-core-sort-by-machine))
        (should (equal (test-herdr-core--addresses) '("t1" "t3" "job-p-a1/t1" "orion/t2" "quasar/t4"))))
      (let ((herdr-core-sort-predicate #'herdr-core-sort-by-address))
        (should (equal (car (test-herdr-core--addresses)) "job-p-a1/t1")))
      (let ((herdr-core-sort-predicate #'herdr-core-sort-by-recent))
        (should (equal (car (test-herdr-core--addresses)) "t1")))
      ;; Grouped by machine: a table group row per machine, its kind, count
      ;; and attention.
      (let ((herdr-core-group-by 'machine))
        (herdr-core-agents)
        (with-current-buffer herdr-core--agents-buffer-name
          (should (equal (test-herdr-core--ids) '("quasar/t4" "job-p-a1/t1" "t1" "t3" "orion/t2")))
          (should (equal (mapcar (lambda (r) (replace-regexp-in-string "  +" " " r))
                                 (test-herdr-core--group-rows))
                         '("▾ quasar · ssh (1) ● 1" "▾ job-p-a1 · pod (1) ✓ 1"
                           "▾ local · local (2)" "▾ orion · ssh (1)")))
          (should (equal (car (test-herdr-core--headers)) "Machine"))
          (goto-char (point-min))
          (search-forward "quasar · ssh")
          (should (memq 'herdr-core-group-header
                        (ensure-list (get-text-property (match-beginning 0) 'face))))
          ;; n and p skip headers and group rows.
          (goto-char (point-min))
          (herdr-core-agents-next-line)
          (should (equal (herdr-core--list-address) "quasar/t4"))
          (herdr-core-agents-next-line)
          (should (equal (herdr-core--list-address) "job-p-a1/t1"))
          (herdr-core-agents-next-line)
          (should (equal (herdr-core--list-address) "t1"))
          (herdr-core-agents-previous-line)
          (should (equal (herdr-core--list-address) "job-p-a1/t1"))
          ;; Point stays on its agent across redraws.
          (herdr-core--refresh)
          (should (equal (herdr-core--list-address) "job-p-a1/t1"))
          ;; A column sort applies within each group: Session, then reversed.
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Session")))
            (herdr-core-agents-sort)
            (herdr-core-agents-sort))
          (should (equal (test-herdr-core--ids) '("quasar/t4" "job-p-a1/t1" "t3" "t1" "orion/t2")))
          (should (equal (herdr-core--table-get :sort) '((1 . t))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) herdr-core--attention-sort-label)))
            (herdr-core-agents-sort))
          ;; G regroups this buffer only.
          (herdr-core-agents-group 'space)
          (should (string-match-p "\\`▾ juniper (3) +● 1 +✓ 1\\'" (car (test-herdr-core--group-rows))))
          (herdr-core-agents-group nil)
          (should-not (test-herdr-core--group-rows))
          (should (equal (car (test-herdr-core--ids)) "quasar/t4"))))
      (let ((herdr-core-group-by '(label "herdr.space")) (herdr-core-group-order 'name))
        (should (equal (mapcar #'car (herdr-core-groups (herdr-core-list)))
                       '("(no herdr.space)" "juniper" "tidepool"))))
      (let ((herdr-core-group-by (lambda (a) (if (equal (alist-get 'kind a) "claude") "agents" "other")))
            (herdr-core-group-header-function (lambda (g agents) (format "== %s %d" g (length agents)))))
        (herdr-core--refresh)
        (with-current-buffer herdr-core--agents-buffer-name
          (should (string-prefix-p "▾ == agents 3 (3)" (car (test-herdr-core--group-rows))))
          (should (equal (car (test-herdr-core--headers)) "Group")))))))

(ert-deftest herdr-core-list-narrow-mark-and-keys ()
  "`/' narrows to a machine, space or tab; marks show agents side by side."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core-agents)
    (with-current-buffer herdr-core--agents-buffer-name
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "juniper")))
        (herdr-core-agents-narrow-space))
      (should (equal (test-herdr-core--ids) '("quasar/t4" "job-p-a1/t1" "t1")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "local")))
        (herdr-core-agents-narrow-machine))
      (should (equal (test-herdr-core--ids) '("t1" "t3")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "tidepool-pm")))
        (herdr-core-agents-narrow-tab))
      (should (equal (test-herdr-core--ids) '("orion/t2")))
      (should (string-match-p "\\[filter: tab: ==tidepool-pm\\]" (buffer-string)))
      (herdr-core-agents-widen)
      (should (= (length (test-herdr-core--ids)) 5))
      (should (eq (keymap-lookup herdr-core-agents-mode-map "/ m") #'herdr-core-agents-narrow-machine))
      (dolist (k '("RET" "a" "o" "r" "s" "b" "S" "G" "m" "u" "v" "i" "e" "k" "c" "?" "n" "p" "g"))
        (should (commandp (keymap-lookup herdr-core-agents-mode-map k))))
      (test-herdr-core--first-agent)
      (herdr-core-agents-mark)
      (herdr-core-agents-mark)
      (should (equal (herdr-core-agents-marked) '("quasar/t4" "job-p-a1/t1")))
      (should (string-prefix-p "[x]" (save-excursion (test-herdr-core--first-agent) (test-herdr-core--line))))
      (herdr-core--refresh)
      (should (string-prefix-p "[x]" (save-excursion (test-herdr-core--first-agent) (test-herdr-core--line))))
      (herdr-core-agents-unmark)
      (should (equal (herdr-core-agents-marked) '("quasar/t4" "job-p-a1/t1")))
      (forward-line -2)
      (herdr-core-agents-unmark)
      (should (equal (herdr-core-agents-marked) '("quasar/t4")))
      (test-herdr-core--first-agent)
      (forward-line 2)
      (herdr-core-agents-mark)
      (herdr-core-agents-show-marked)
      (should-not (herdr-core-agents-marked)))
    (should (= (length (window-list)) 2))
    (should (equal (mapcar (lambda (w) (buffer-local-value 'herdr-core--attached-address (window-buffer w)))
                           (window-list nil nil (frame-first-window)))
                   '("quasar/t4" "t1")))))

;;;; The list as a vuistack-table: grouping, filters, actions, live state

(defun test-herdr-core--goto-group (text)
  "Move to the list's group row whose text contains TEXT."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (let ((row (get-text-property (point) 'vuistack-table-row)))
        (if (and row (not (plist-get row :address))
                 (string-match-p (regexp-quote text) (test-herdr-core--line)))
            (setq found t)
          (forward-line 1))))
    (should found)))

(defun test-herdr-core--goto-column (header)
  "Move to the first agent's cell in the shown column HEADER."
  (test-herdr-core--first-agent)
  (let* ((table (vuistack-table--table herdr-core--table-id))
         (index (plist-get (seq-find (lambda (c) (equal (plist-get c :header) header))
                                     (plist-get table :display))
                           :vuistack-table--index)))
    (goto-char (text-property-any (line-beginning-position) (line-end-position)
                                  'vuistack-table-column-index index))
    (forward-char 1)))

(ert-deftest herdr-core-list-table-grouping-and-aggregation ()
  "Grouping is the table's: groups fold, nest under a column's own G, aggregate and page."
  (test-herdr-core--with-fake dir
    (test-herdr-core--with-machines
      (test-herdr-core--load-snapshot)
      (let ((herdr-core-group-by 'space))
        (herdr-core-agents)
        (with-current-buffer herdr-core--agents-buffer-name
          (should (equal (herdr-core--table-get :grouping) (list (herdr-core--group-index))))
          (should (eq (herdr-core--table-get :expanded) t))
          (should (equal (mapcar (lambda (r) (car (split-string r "  "))) (test-herdr-core--group-rows))
                         '("▾ juniper (3)" "▾ tidepool (1)" "▾ (no space) (1)")))
          ;; x folds the group at point, X unfolds them all.
          (test-herdr-core--goto-group "juniper")
          (vuistack-table-toggle-expand)
          (should (equal (test-herdr-core--ids) '("orion/t2" "t3")))
          (should (string-prefix-p "▸ juniper (3)" (car (test-herdr-core--group-rows))))
          (vuistack-table-toggle-expand-all)
          (should (= (length (test-herdr-core--ids)) 5))
          ;; The table's own G on a shown column nests a group under the space.
          (test-herdr-core--goto-column "Kind")
          (vuistack-table-group-by-column)
          (should (equal (herdr-core--table-get :grouping)
                         (list (herdr-core--group-index) 2)))
          (should (seq-some (lambda (r) (string-match-p "\\`▾ +claude (3)" r))
                            (test-herdr-core--group-rows)))
          (should (string-match-p "grouped by Space > Kind" (buffer-string)))
          ;; Dropping the herdr grouping keeps the table's own.
          (herdr-core-agents-group nil)
          (should (equal (herdr-core--table-get :grouping) nil))
          (herdr-core-agents-group 'machine)
          (should (equal (car (test-herdr-core--headers)) "Machine")))))
    ;; Paging.
    (kill-buffer herdr-core--agents-buffer-name)
    (let ((herdr-core-list-page-size 2))
      (herdr-core-agents)
      (with-current-buffer herdr-core--agents-buffer-name
        (should (equal (test-herdr-core--ids) '("quasar/t4" "job-p-a1/t1")))
        (should (string-match-p "Page 1 of 3 · 5 rows" (buffer-string)))
        (test-herdr-core--first-agent)
        (vuistack-table-next-page)
        (should (equal (test-herdr-core--ids) '("t1" "orion/t2")))))))

(ert-deftest herdr-core-list-table-filters ()
  "Global, fuzzy, per-column and faceted filters are the table's; narrowing stacks with them."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core-agents)
    (with-current-buffer herdr-core--agents-buffer-name
      (should (eq (keymap-lookup herdr-core-agents-mode-map "/ f") #'vuistack-table-filter))
      (test-herdr-core--first-agent)
      (vuistack-table-filter "juniper")
      (should (equal (test-herdr-core--ids) '("quasar/t4" "job-p-a1/t1" "t1")))
      (should (string-match-p "\\[filter: juniper\\]" (buffer-string)))
      (test-herdr-core--first-agent)
      (vuistack-table-filter "Kind: codex")
      (should (equal (test-herdr-core--ids) '("orion/t2")))
      (test-herdr-core--first-agent)
      (vuistack-table-filter "~tdpm")
      (should (equal (car (test-herdr-core--ids)) "orion/t2"))
      (test-herdr-core--first-agent)
      (vuistack-table-filter "")
      (should (= (length (test-herdr-core--ids)) 5))
      ;; f on a column completes its values with counts (faceting).
      (test-herdr-core--goto-column "Machine")
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p collection &rest _) (setq offered (all-completions "" collection))
                     "local")))
          (call-interactively #'vuistack-table-filter-column))
        (should (equal (sort offered #'string<) '("job-p-a1" "local" "orion" "quasar"))))
      (should (equal (test-herdr-core--ids) '("t1" "t3")))
      ;; A narrowing stacks with it; / / drops both.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "juniper")))
        (herdr-core-agents-narrow-space))
      (should (equal (test-herdr-core--ids) '("t1")))
      (herdr-core-agents-widen)
      (should (= (length (test-herdr-core--ids)) 5))
      (should-not (herdr-core--table-get :column-filters)))))

(ert-deftest herdr-core-list-table-row-actions ()
  "RET runs the table's row action (show the agent); the table's keys are the list's."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core-agents)
    (with-current-buffer herdr-core--agents-buffer-name
      (test-herdr-core--goto-column "Session")
      (should (functionp (get-text-property (point) 'vuistack-table-action)))
      (should (eq (plist-get (plist-get (vuistack-table--table herdr-core--table-id) :options) :action)
                  #'herdr-core--row-action))
      (dolist (k '(("x" . vuistack-table-toggle-expand) ("f" . vuistack-table-filter-column)
                   ("F" . vuistack-table-filter-reset) ("M" . vuistack-table-toggle-select-all)
                   ("V" . vuistack-table-toggle-column) ("|" . vuistack-table-pin-column)))
        (should (eq (keymap-lookup herdr-core-agents-mode-map (car k)) (cdr k))))
      (herdr-core-agents-show))
    (should (equal (nth 2 (car test-herdr-core--opened)) '("attach" "quasar/t4")))
    (with-current-buffer herdr-core--agents-buffer-name
      ;; Off a cell (the selection gutter), RET still shows the line's agent.
      (test-herdr-core--first-agent)
      (herdr-core-agents-next-line)
      (beginning-of-line)
      (should-not (get-text-property (point) 'vuistack-table-action))
      (herdr-core-agents-show))
    (should (equal (nth 2 (car test-herdr-core--opened)) '("attach" "job-p-a1/t1")))
    (with-current-buffer herdr-core--agents-buffer-name
      ;; The status line and headings are no agent.
      (goto-char (point-min))
      (should-error (herdr-core-agents-attach) :type 'user-error))))

(ert-deftest herdr-core-list-table-keeps-state-across-events ()
  "An hc event re-renders the mounted table keeping sort, filter, folds, selection and point."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core-agents)
    (with-current-buffer herdr-core--agents-buffer-name
      (let ((root herdr-core--agents-root))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Address")))
          (herdr-core-agents-sort))
        (test-herdr-core--first-agent)
        (search-forward "quasar/t4")
        (herdr-core-agents-mark)
        (herdr-core-agents-group 'space)
        (test-herdr-core--goto-group "juniper")
        (vuistack-table-toggle-expand)
        (should (equal (test-herdr-core--ids) '("orion/t2" "t3")))
        (test-herdr-core--first-agent)
        (should (equal (herdr-core--list-address) "orion/t2"))
        ;; orion/t2 blocks, t1 is renamed, a pod joins, another leaves, t3 ends.
        (apply #'test-herdr-core--feed (test-herdr-core--lines "changes.jsonl"))
        (herdr-core--refresh)
        (should (eq herdr-core--agents-root root))
        (should (equal (herdr-core--list-address) "orion/t2"))
        (should (equal (test-herdr-core--ids) '("orion/t2")))
        (should (seq-some (lambda (r) (string-prefix-p "▸ juniper (3)" r)) (test-herdr-core--group-rows)))
        (vuistack-table-toggle-expand-all)
        (should (equal (test-herdr-core--ids) '("orion/t2" "job-q-a1/t1" "quasar/t4" "t1")))
        (should (equal (herdr-core--table-get :sort) '((5))))
        (should (equal (herdr-core-agents-marked) '("quasar/t4")))
        (should (string-match-p "\\[x\\] .*juniper · job-b" (buffer-string)))
        ;; A filter survives a fresh snapshot too.
        (test-herdr-core--first-agent)
        (vuistack-table-filter "tidepool")
        (test-herdr-core--load-snapshot)
        (herdr-core--refresh)
        (should (equal (test-herdr-core--ids) '("orion/t2")))
        (should (string-match-p "\\[filter: tidepool\\]" (buffer-string)))))))

;;;; Picking and switching

(ert-deftest herdr-core-picker-groups-and-custom-reader ()
  "The picker keeps attention order, offers completion groups, and every part is pluggable."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let* (seen meta
           (herdr-core-group-by 'space)
           (herdr-core-candidate-function (lambda (a) (concat "<" (alist-get 'address a) ">")))
           (herdr-core-completing-read-function
            (lambda (_prompt collection &rest _)
              (setq seen (all-completions "" collection)
                    meta (funcall collection "" nil 'metadata))
              "<orion/t2>")))
      (should (equal (alist-get 'address (herdr-core--pick-agent)) "orion/t2"))
      (should (equal seen '("<quasar/t4>" "<job-p-a1/t1>" "<t1>" "<orion/t2>" "<t3>")))
      (let ((group (alist-get 'group-function (cdr meta))))
        (should (equal (funcall group "<orion/t2>" nil) "tidepool"))
        (should (equal (funcall group "<t3>" nil) "(no space)")))
      (let ((herdr-core-picker-group-by nil))
        (herdr-core--pick-agent)
        (should-not (alist-get 'group-function (cdr meta))))
      (let ((herdr-core-picker-group-by 'machine))
        (herdr-core--pick-agent)
        (should (equal (funcall (alist-get 'group-function (cdr meta)) "<t3>" nil) "local"))))))

(ert-deftest herdr-core-next-previous-attention-and-scope ()
  "next/previous cycle in list order, wrapping; scope narrows them; attention
and blocked jumps cycle through their statuses."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let ((where (lambda () (buffer-local-value 'herdr-core--attached-address
                                                (window-buffer (selected-window))))))
      (herdr-core-next-agent)
      (should (equal (funcall where) "quasar/t4"))
      (herdr-core-next-agent)
      (should (equal (funcall where) "job-p-a1/t1"))
      (herdr-core-previous-agent)
      (herdr-core-previous-agent)
      (should (equal (funcall where) "t3"))
      (herdr-core-next-agent)
      (should (equal (funcall where) "quasar/t4"))
      (let ((herdr-core-cycle-scope 'space))
        (herdr-core-next-agent)
        (should (equal (funcall where) "job-p-a1/t1"))
        (herdr-core-next-agent)
        (should (equal (funcall where) "t1"))
        (herdr-core-next-agent)
        (should (equal (funcall where) "quasar/t4")))
      (let ((herdr-core-cycle-scope 'machine))
        (herdr-core--attach-agent (herdr-core--find-agent "t1"))
        (herdr-core-next-agent)
        (should (equal (funcall where) "t3"))
        (herdr-core-next-agent)
        (should (equal (funcall where) "t1")))
      ;; blocked, error, done.
      (herdr-core-next-attention)
      (should (equal (funcall where) "quasar/t4"))
      (herdr-core-next-attention)
      (should (equal (funcall where) "job-p-a1/t1"))
      (herdr-core-next-attention)
      (should (equal (funcall where) "quasar/t4"))
      (let ((herdr-core-attention-statuses '("idle")))
        (herdr-core-next-attention)
        (should (equal (funcall where) "orion/t2")))
      ;; Two blocked: goto-blocked alternates.
      (test-herdr-core--feed (car (test-herdr-core--lines "changes.jsonl")))
      (should (equal (seq-take (test-herdr-core--addresses) 2) '("orion/t2" "quasar/t4")))
      (herdr-core-goto-blocked)
      (should (equal (funcall where) "quasar/t4"))
      (herdr-core-goto-blocked)
      (should (equal (funcall where) "orion/t2"))
      (with-temp-buffer
        (let ((herdr-core-attention-statuses '("nothing")))
          (should-error (herdr-core-next-attention) :type 'user-error))))))

(ert-deftest herdr-core-switch-machine-space-tab-and-any-terminal ()
  "Switch by machine, space or tab: one agent is shown, several are picked from;
any terminal (finished, or a typed address) attaches."
  (test-herdr-core--with-fake dir
    (test-herdr-core--with-machines
      (test-herdr-core--load-snapshot)
      (let (prompts offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (push prompt prompts)
                     (let ((all (all-completions "" collection)))
                       (push all offered)
                       (pcase prompt
                         ("Machine: " (seq-find (lambda (c) (string-prefix-p "quasar " c)) all))
                         ("Space: " (seq-find (lambda (c) (string-prefix-p "juniper " c)) all))
                         ("Tab: " (seq-find (lambda (c) (string-prefix-p "tidepool / " c)) all))
                         (_ (seq-find (lambda (c) (string-match-p "t1\\'" c)) all)))))))
          (call-interactively #'herdr-core-switch-machine)
          (should (equal herdr-core--attached-address "quasar/t4"))
          (should (equal (car offered)
                         '("quasar (ssh, connected) · 1 agent, 1 blocked"
                           "job-p-a1 (pod, connected) · 1 agent"
                           "local (local, connected) · 2 agents"
                           "orion (ssh, connected) · 1 agent")))
          (herdr-core-switch-space)
          (should (equal (car prompts) "Agent in juniper: "))
          (should (equal herdr-core--attached-address "job-p-a1/t1"))
          (herdr-core-switch-tab)
          (should (equal herdr-core--attached-address "orion/t2"))
          (should (seq-find (lambda (c) (string-prefix-p "- / scratch" c)) (car offered)))))
      ;; Any terminal: the finished t9, or an address typed in.
      (cl-letf (((symbol-function 'completing-read)
                 (test-herdr-core--pick "\\`t9 · finished")))
        (call-interactively #'herdr-core-attach-terminal))
      (should (equal herdr-core--attached-address "t9"))
      (herdr-core-attach-terminal "pod9/t4" t)
      (should (equal (nth 2 (car test-herdr-core--opened)) '("attach" "--observe" "pod9/t4"))))))

;;;; Attaching: layouts, history, hooks

(defun test-herdr-core--window-addresses ()
  "The attached address of each window, in window order, with its edges."
  (mapcar (lambda (w) (list (buffer-local-value 'herdr-core--attached-address (window-buffer w))
                            (window-edges w)))
          (window-list nil nil (frame-first-window))))

(ert-deftest herdr-core-side-by-side-layouts ()
  "Several agents side by side: columns, rows, a grid, at most the max, or a function."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let ((agents (herdr-core-list))
          (frame-width (frame-width)))
      (ignore frame-width)
      (let ((herdr-core-layout 'columns))
        (herdr-core-show-several (seq-take agents 3))
        (let ((wins (test-herdr-core--window-addresses)))
          (should (equal (mapcar #'car wins) '("quasar/t4" "job-p-a1/t1" "t1")))
          ;; Side by side: same top, growing left.
          (should (apply #'= (mapcar (lambda (w) (nth 1 (cadr w))) wins)))
          (should (apply #'< (mapcar (lambda (w) (nth 0 (cadr w))) wins))))
        (should (eq (window-buffer (selected-window))
                    (herdr-core--find-attach-buffer "quasar/t4"))))
      (let ((herdr-core-layout 'rows))
        (herdr-core-show-several (seq-take agents 2))
        (let ((wins (test-herdr-core--window-addresses)))
          (should (= (length wins) 2))
          (should (apply #'= (mapcar (lambda (w) (nth 0 (cadr w))) wins)))
          (should (apply #'< (mapcar (lambda (w) (nth 1 (cadr w))) wins)))))
      (let ((herdr-core-layout 'grid))
        (herdr-core-show-several agents)
        (let* ((wins (test-herdr-core--window-addresses))
               (lefts (delete-dups (mapcar (lambda (w) (nth 0 (cadr w))) wins))))
          ;; 4 of 5 (the max), two columns of two.
          (should (= (length wins) 4))
          (should (= (length lefts) 2))))
      (let ((herdr-core-layout 'columns) (herdr-core-layout-max 2))
        (herdr-core-show-several agents)
        (should (= (length (window-list)) 2)))
      (let ((herdr-core-layout 'columns) (herdr-core-layout-delete-other-windows nil))
        (delete-other-windows)
        (split-window-below)
        (herdr-core-show-several (seq-take agents 2))
        (should (= (length (window-list)) 3)))
      (let* (got (herdr-core-layout (lambda (as) (setq got as))))
        (herdr-core-show-several (seq-take agents 2))
        (should (equal (mapcar (lambda (a) (alist-get 'address a)) got) '("quasar/t4" "job-p-a1/t1"))))
      ;; Interactive picks end on empty input.
      (let ((herdr-core-layout 'columns) (answers '("" "orion/t2\\'" "· t1\\'")))
        (delete-other-windows)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p collection &rest _)
                     (let ((re (pop answers)) (all (all-completions "" collection)))
                       (if (string-empty-p re) "" (seq-find (lambda (c) (string-match-p re c)) all))))))
          (setq answers (reverse answers))
          (herdr-core-attach-several))
        (should (equal (mapcar #'car (test-herdr-core--window-addresses)) '("t1" "orion/t2"))))
      ;; A group side by side.
      (cl-letf (((symbol-function 'completing-read) (test-herdr-core--pick "\\`juniper")))
        (herdr-core-attach-group))
      (should (equal (mapcar #'car (test-herdr-core--window-addresses))
                     '("quasar/t4" "job-p-a1/t1" "t1"))))))

(ert-deftest herdr-core-attach-history-hook-and-display-actions ()
  "--history, the attach hook, header-line options and per-context display actions."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let* (hooked
           (herdr-core-attach-history 200)
           (herdr-core-attach-hook (list (lambda () (push herdr-core--attached-address hooked))))
           (herdr-core-attach-header-line nil)
           (herdr-core-display-actions `((attach . ((display-buffer-pop-up-window)))
                                         ,@herdr-core-display-actions)))
      (delete-other-windows)
      (herdr-core--attach-agent (herdr-core--find-agent "t1"))
      (should (equal hooked '("t1")))
      (should (equal (cdr (car test-herdr-core--opened))
                     (list herdr-core-executable '("attach" "--history" "200" "t1")
                           '((display-buffer-pop-up-window)))))
      (should (= (length (window-list)) 2))
      (with-current-buffer (herdr-core--find-attach-buffer "t1")
        (should-not header-line-format)))
    (let ((herdr-core-header-label-function (lambda (a) (concat "[" (alist-get 'address a) "]"))))
      (with-temp-buffer
        (should (string-prefix-p "[quasar/t4] [job-p-a1/t1]"
                                 (substring-no-properties (herdr-core--header-line))))))))

;;;; Read buffers and renderers

(defun test-herdr-core--buttons (prop)
  "Values of PROP on the current buffer's buttons."
  (let (vals)
    (save-excursion
      (goto-char (point-min))
      (while (forward-button 1 nil nil t)
        (when-let* ((v (button-get (button-at (point)) prop)))
          (push v vals))))
    (nreverse vals)))

(ert-deftest herdr-core-read-renders-links-files-images-and-diffs ()
  "A read buffer turns URLs, files, images and diffs the agent printed into
buttons, inline images and diff faces; the renderers are a list of functions."
  (test-herdr-core--with-fake dir
    (let* ((cwd (file-name-as-directory (expand-file-name "work" dir)))
           (browsed nil) (found nil))
      (make-directory (expand-file-name "src" cwd) t)
      (with-temp-file (expand-file-name "src/main.rs" cwd) (insert "fn main() {}\n"))
      (with-temp-file (expand-file-name "shot.png" cwd) (insert "PNG"))
      (with-temp-file (expand-file-name "read.txt" dir)
        (insert "Done: see https://gitlab.com/x/y/-/merge_requests/7.\n"
                "I edited src/main.rs:12 and wrote shot.png; missing/nothing.rs stays plain.\n"
                "\e[31mred\e[0m text\n"
                "diff --git a/src/main.rs b/src/main.rs\n"
                "index 1..2 100644\n"
                "--- a/src/main.rs\n"
                "+++ b/src/main.rs\n"
                "@@ -1 +1 @@\n"
                "-fn old() {}\n"
                "+fn main() {}\n"
                "\n"
                "end\n"))
      (herdr-core-apply `((terminals ((address . "t1") (state . "live") (cwd . ,cwd)
                                      (labels (session.title . "proj · render"))
                                      (status (state . "done")) (turn . 1)))))
      (cl-letf (((symbol-function 'herdr-core--create-image)
                 (lambda (file &optional _data) (list 'image :type 'png :file file))))
        (let ((herdr-core-browse-url-function (lambda (url) (setq browsed url)))
              (herdr-core-find-file-function (lambda (f) (setq found f))))
          (herdr-core--read "t1")
          (should (test-herdr-core--wait (lambda () (get-buffer "*hc read: proj · render*"))))
          (with-current-buffer "*hc read: proj · render*"
            (should (derived-mode-p 'herdr-core-read-mode))
            (should (equal (test-herdr-core--buttons 'herdr-core-url)
                           '("https://gitlab.com/x/y/-/merge_requests/7")))
            (should (equal (test-herdr-core--buttons 'herdr-core-file) '("src/main.rs" "shot.png")))
            ;; ANSI escapes became faces.
            (should-not (string-match-p "\e" (buffer-string)))
            (goto-char (point-min))
            (search-forward "red")
            (should (seq-some (lambda (o) (overlay-get o 'face)) (overlays-at (1- (point)))))
            ;; The URL and the file open through their functions.
            (goto-char (point-min))
            (search-forward "https://")
            (push-button)
            (should (equal browsed "https://gitlab.com/x/y/-/merge_requests/7"))
            (goto-char (point-min))
            (search-forward "src/main")
            (push-button)
            (should (equal found (expand-file-name "src/main.rs" cwd)))
            ;; The image shows below its line.
            (let ((ov (seq-find (lambda (o) (overlay-get o 'herdr-core-image)) (overlays-in (point-min) (point-max)))))
              (should ov)
              (should (equal (overlay-get ov 'herdr-core-image) "shot.png"))
              (should (equal (plist-get (cdr (get-text-property 1 'display (overlay-get ov 'after-string))) :file)
                             (expand-file-name "shot.png" cwd))))
            ;; The diff: colored, and d opens it in diff-mode.
            (goto-char (point-min))
            (search-forward "+fn main")
            (should (memq 'diff-added (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at (point)))))
            (search-backward "-fn old")
            (should (memq 'diff-removed (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at (point)))))
            (let ((diff (herdr-core-read-open-diff)))
              (with-current-buffer diff
                (should (derived-mode-p 'diff-mode))
                (should (string-prefix-p "diff --git" (buffer-string)))
                (should (string-suffix-p "+fn main() {}\n" (buffer-string)))))
            (goto-char (point-max))
            (should-error (herdr-core-read-open-diff) :type 'user-error))))
      ;; Renderers are pluggable.
      (let ((herdr-core-renderers (list (lambda (beg end _agent) (put-text-property beg end 'mine t)))))
        (test-herdr-core--reread "t1")
        (should (test-herdr-core--wait
                 (lambda () (with-current-buffer "*hc read: proj · render*"
                              (get-text-property (point-min) 'mine)))))
        (with-current-buffer "*hc read: proj · render*"
          (should-not (test-herdr-core--buttons 'herdr-core-url)))))))

(defun test-herdr-core--reread (address)
  "Refresh ADDRESS's read buffer (a test helper)."
  (with-current-buffer (herdr-core--find-read-buffer address)
    (herdr-core-read-refresh)))

(ert-deftest herdr-core-read-remote-files-and-images-go-through-hc ()
  "On another machine, absolute paths open through `hc file read --on' and
images come from `hc exec --on M base64' when clicked (or inline)."
  (test-herdr-core--with-fake dir
    (with-temp-file (expand-file-name "read.txt" dir)
      (insert "wrote /srv/out/report.txt and /srv/out/chart.png and rel/thing.txt\n"))
    (herdr-core-apply '((terminals ((address . "orion/t2") (state . "live") (cwd . "/srv")
                                    (labels (session.title . "tidepool · remote"))))))
    (let (images)
      (cl-letf (((symbol-function 'herdr-core--create-image)
                 (lambda (data &optional data-p) (push (list data data-p) images)
                   (list 'image :type 'png :data data))))
        (herdr-core--read "orion/t2")
        (should (test-herdr-core--wait (lambda () (get-buffer "*hc read: tidepool · remote*"))))
        (with-current-buffer "*hc read: tidepool · remote*"
          (should (equal (test-herdr-core--buttons 'herdr-core-file) '("/srv/out/report.txt")))
          (should (equal (test-herdr-core--buttons 'herdr-core-image) '("/srv/out/chart.png")))
          (should-not images)
          (goto-char (point-min))
          (search-forward "chart")
          (push-button)
          (should (equal images '(("PNGDATA" t))))
          (should (seq-find (lambda (o) (overlay-get o 'herdr-core-image)) (overlays-in (point-min) (point-max))))
          (goto-char (point-min))
          (search-forward "report")
          (let ((buf (button-activate (button-at (point)))))
            (ignore buf))
          (with-current-buffer "*hc file: orion:/srv/out/report.txt*"
            (should (string-match-p "remote file: file read --on orion --cwd /srv /srv/out/report.txt"
                                    (buffer-string)))))
        (setq images nil)
        (let ((herdr-core-render-remote-images 'inline))
          (test-herdr-core--reread "orion/t2")
          (should (test-herdr-core--wait (lambda () images))))
        (let ((herdr-core-render-remote-images nil))
          (test-herdr-core--reread "orion/t2")
          (should (test-herdr-core--wait
                   (lambda () (with-current-buffer "*hc read: tidepool · remote*"
                                (null (test-herdr-core--buttons 'herdr-core-image)))))))))
    (should (string-match-p "^exec --on orion base64 /srv/out/chart.png$" (test-herdr-core--log dir)))))

(ert-deftest herdr-core-read-follows-the-agent ()
  "A following read buffer reads again when its agent's turn or status changes."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (herdr-core--read "t1")
    (should (test-herdr-core--wait (lambda () (herdr-core--find-read-buffer "t1"))))
    (let ((count (lambda () (cl-count-if (lambda (l) (equal l "read --lines 500 t1"))
                                         (split-string (test-herdr-core--log dir) "\n")))))
      (should (= (funcall count) 1))
      (herdr-core--refresh)
      (should (= (funcall count) 1))
      ;; The turn event (changes.jsonl line 3).
      (test-herdr-core--feed (nth 2 (test-herdr-core--lines "changes.jsonl")))
      (herdr-core--refresh)
      (should (test-herdr-core--wait (lambda () (= (funcall count) 2))))
      (with-current-buffer (herdr-core--find-read-buffer "t1")
        (herdr-core-read-toggle-follow))
      (test-herdr-core--feed (test-herdr-core--json
                              '((jsonrpc . "2.0") (method . "event")
                                (params (type . "turn") (terminal . "t1") (data (turn . 3))))))
      (herdr-core--refresh)
      (sleep-for 0.2)
      (should (= (funcall count) 2)))))

;;;; Events and polling

(ert-deftest herdr-core-poll-policy-and-poll-fallback ()
  "`poll' never streams and keeps the state from hc ls; `events+poll' polls
while the stream is down; status changes run the hook either way."
  (test-herdr-core--with-fake dir
    (let ((herdr-core-refresh-policy 'poll) (herdr-core-poll-interval 60) changes)
      (setq herdr-core-status-change-functions
            (list (lambda (a old new) (push (list (alist-get 'address a) old new) changes))))
      (herdr-core-start)
      (should herdr-core--poll-timer)
      (should (test-herdr-core--wait (lambda () herdr-core--snapshot-seen)))
      (should-not (process-live-p herdr-core--process))
      (should (equal (test-herdr-core--addresses) '("quasar/t4" "job-p-a1/t1" "t1" "orion/t2" "t3")))
      (should-not (string-match-p "^events" (test-herdr-core--log dir)))
      ;; orion/t2 blocks: the next poll sees it.
      (with-temp-file (expand-file-name "get/orion_t2.json" dir)
        (insert (test-herdr-core--json '((address . "orion/t2") (state . "live")
                                         (status (state . "blocked") (kind . "auth"))))))
      (herdr-core-poll)
      (should (equal changes '(("orion/t2" "idle" "blocked"))))
      (should (equal (alist-get 'blocked-kind (herdr-core--find-agent "orion/t2")) "auth"))
      ;; Without details: one ls call, its status words.
      (let ((herdr-core-poll-details nil))
        (herdr-core-poll)
        (should (equal (alist-get 'status (herdr-core--find-agent "orion/t2")) "idle")))
      ;; hc down: the state stays.
      (with-temp-file (expand-file-name "down" dir))
      (should-not (herdr-core-poll))
      (should (= (length (herdr-core-list)) 5))
      ;; `herdr-core--fetch-agents-sync' uses the polled state.
      (should (= (length (herdr-core--fetch-agents-sync)) 5))
      (herdr-core-stop)
      (should-not herdr-core--poll-timer))
    (delete-file (expand-file-name "down" dir))
    (with-temp-file (expand-file-name "events-down" dir))
    (let ((herdr-core-refresh-policy 'events+poll) (herdr-core-poll-interval 60)
          (herdr-core-reconnect-delay 60) (global-mode-string nil))
      (unwind-protect
          (progn
            (herdr-core-mode 1)
            (should (test-herdr-core--wait (lambda () herdr-core--poll-timer)))
            (should (test-herdr-core--wait (lambda () (= (length (herdr-core-list)) 5)))))
        (herdr-core-mode -1)))))

(ert-deftest herdr-core-event-hooks-and-machine-refresh ()
  "Raw event functions see every line; the refresh hook runs after a redraw;
a machine leaving or a new machine's terminal asks hc machine ls again."
  (test-herdr-core--with-fake dir
    (let* (seen refreshed
           (herdr-core-event-functions (list (lambda (m) (push m seen))))
           (herdr-core-refresh-hook (list (lambda () (setq refreshed t)))))
      (herdr-core--set-machines nil)
      (apply #'test-herdr-core--feed (test-herdr-core--lines "events.jsonl"))
      (apply #'test-herdr-core--feed (test-herdr-core--lines "changes.jsonl"))
      (should (= (length seen) 8))
      (should (equal (nth 2 seen) '((removed . "job-p-a1"))))
      (herdr-core--refresh)
      (should refreshed)
      (should (test-herdr-core--wait (lambda () (consp herdr-core--machines))))
      (should (string-match-p "^machine ls --json$" (test-herdr-core--log dir))))))

;;;; Starting agents

(ert-deftest herdr-core-launcher-template-machine-and-hooks ()
  "The launcher command is a template (plain hc, --on a machine); the launch
function, naming, directory and after-launch hooks are pluggable."
  (test-herdr-core--with-fake dir
    (let* ((root (file-name-as-directory (expand-file-name "proj" dir)))
           (herdr-core-launch-directory-function (lambda () root))
           (launched nil)
           (herdr-core-after-launch-functions (list (lambda (address out) (push (list address out) launched)))))
      (make-directory root t)
      (let ((herdr-core-launcher-args herdr-core-hc-run-launcher-args))
        (should (equal (herdr-core-launcher-arguments
                        `((name . "proj-claude") (dir . ,root) (kind . "claude") (machine . "orion")
                          (space . "proj") (tab . "proj-claude")))
                       `("run" "--bg" "--keep" "--outlive-parent" "--no-parent" "--json"
                         "--name" "proj-claude" "--cwd" ,root "--on" "orion"
                         "--label" "herdr.space=proj" "--label" "herdr.tab=proj-claude"
                         "--label" "session.title=proj · proj-claude" "--" "claude")))
        (should (equal (member "--on" (herdr-core-launcher-arguments `((name . "n") (dir . ,root) (kind . "k")
                                                                     (machine . "local"))))
                       nil))
        (herdr-core-new-agent "codex" "orion")
        (should (test-herdr-core--wait (lambda () launched)))
        (should (string-match-p (format "^run --bg .* --name proj-codex --cwd %s --on orion .*-- codex$"
                                        (regexp-quote root))
                                (test-herdr-core--log dir "launch.log")))
        (should (equal (caar launched) "t7")))
      ;; A launcher template without --on cannot place a machine.
      (let ((herdr-core-launcher-args '("run" "--name" "%n" "--cwd" "%d" "--" "%k")))
        (should-error (herdr-core-new-agent "claude" "orion") :type 'user-error))
      ;; A Lisp launcher, its own names, no attach.
      (let* (spec-seen
             (herdr-core-launch-function (lambda (spec cb) (setq spec-seen spec) (funcall cb "orion/t9" "")))
             (herdr-core-agent-name-function (lambda (_dir kind) (concat "x-" kind)))
             (herdr-core-attach-after-launch nil)
             (opened (length test-herdr-core--opened)))
        (setq launched nil)
        (herdr-core-new-agent "pi" "orion")
        (should (equal (alist-get 'name spec-seen) "x-pi"))
        (should (equal (alist-get 'machine spec-seen) "orion"))
        (should (equal (alist-get 'space spec-seen) "proj"))
        (should (equal launched '(("orion/t9" ""))))
        (should (= (length test-herdr-core--opened) opened))))))

;;;; Talking to agents

(ert-deftest herdr-core-talk-commands-go-through-hc ()
  "keys, interrupt, put, wait, label (rename), kill and schema call hc; the
current buffer's agent is the target, else a pick."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let* ((agent (herdr-core--find-agent "orion/t2"))
           (notified nil)
           (herdr-core-notify-function (lambda (title body address) (push (list title body address) notified)))
           (file (expand-file-name "f.txt" dir)))
      (with-temp-file file (insert "hi"))
      (herdr-core-send-keys agent "esc ctrl+c")
      (herdr-core-interrupt-agent agent)
      (herdr-core-put-file agent file)
      (herdr-core-wait-agent agent)
      (let ((herdr-core-wait-timeout 5)) (herdr-core-wait-agent agent))
      (herdr-core-rename-agent agent "tidepool · renamed")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (herdr-core-kill-agent agent))
      (should (test-herdr-core--wait (lambda () (and notified (equal (car kill-ring) "/core/files/orion/t2/f.txt")))))
      (should (test-herdr-core--wait (lambda () (= (length notified) 2))))
      (should (equal (car notified) '("tidepool · tidepool-pm: stopped (done)" "done" "orion/t2")))
      (let ((log (test-herdr-core--log dir)))
        (dolist (want (list "^keys orion/t2 esc ctrl\\+c$" "^interrupt --json orion/t2$"
                            (concat "^put --json orion/t2 " (regexp-quote file) "$")
                            "^wait --json orion/t2$" "^wait --json --timeout 5 orion/t2$" "^label --json orion/t2 session.title=tidepool · renamed$"
                            "^kill --json orion/t2$"))
          (should (test-herdr-core--wait (lambda () (string-match-p want (test-herdr-core--log dir)))))))
      ;; From an attach buffer, the target is its agent: no pick.
      (herdr-core--attach-agent (herdr-core--find-agent "t1"))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Should not pick"))))
        (should (equal (alist-get 'address (herdr-core--target-agent "x")) "t1")))
      (herdr-core-schema)
      (with-current-buffer "*hc schema*"
        (should (equal (alist-get 'openrpc (herdr-core--parse (buffer-string))) "1.3.2")))
      ;; The region format is an option.
      (let ((herdr-core-region-format-function #'buffer-substring-no-properties))
        (cl-letf (((symbol-function 'completing-read) (test-herdr-core--pick "t3\\'")))
          (with-temp-buffer
            (insert "plain words")
            (herdr-core-send-region (point-min) (point-max)))))
      (should (test-herdr-core--wait (lambda () (string-match-p "^send t3 plain words$" (test-herdr-core--log dir))))))))

;;;; Backend

(ert-deftest herdr-core-backend-args-and-environment ()
  "`herdr-core-executable-args' go before every command and
`herdr-core-process-environment' reaches every hc process."
  (test-herdr-core--with-fake dir
    (let* ((hc herdr-core-executable)
           (herdr-core-executable "/bin/sh")
           (herdr-core-executable-args (list hc))
           (herdr-core-process-environment '("HERDR_TEST_ENV=from-emacs")))
      (should (equal (herdr-core--command "ls") (list "/bin/sh" hc "ls")))
      (herdr-core--call-sync "env")
      (herdr-core--hc '("env") #'ignore)
      (should (test-herdr-core--wait (lambda () (equal (test-herdr-core--log dir "env.log")
                                                       "from-emacs\nfrom-emacs\n"))))
      (should (= (length (herdr-core--fetch-agents-sync)) 5))
      (herdr-core--attach-agent (herdr-core--find-agent "t1"))
      (should (equal (cdr (butlast (car test-herdr-core--opened)))
                     (list "/bin/sh" (list hc "attach" "t1"))))
      (let ((herdr-core-start-core 'never))
        (herdr-core-stop)
        (with-temp-file (expand-file-name "events-down" dir))
        (with-temp-file (expand-file-name "down" dir))
        (should-error (herdr-core--fetch-agents-sync) :type 'user-error))
      (let ((herdr-core-start-core 'always))
        (should (= (length (herdr-core--fetch-agents-sync)) 5))))))

;;;; Notifications and the mode line

(ert-deftest herdr-core-notify-and-mode-line-options ()
  "Which statuses notify and count, the format, and notifying while visible."
  (test-herdr-core--with-fake dir
    (test-herdr-core--load-snapshot)
    (let (notified)
      (let ((herdr-core-mode-line-statuses '("blocked" "done"))
            (herdr-core-mode-line-format "[hc %d]"))
        (should (equal (substring-no-properties (herdr-core--mode-line-segment)) "[hc 2]"))
        (should (get-text-property 0 'local-map (herdr-core--mode-line-segment))))
      (let ((herdr-core-mode-line-statuses '("nothing")))
        (should (equal (herdr-core--mode-line-segment) "")))
      (let ((herdr-core-mode-line-function (lambda () "custom")))
        (should (equal (herdr-core--mode-line-segment) "custom")))
      (let ((herdr-core-notify-function (lambda (&rest args) (push args notified)))
            (herdr-core-notify-statuses '("error")))
        (herdr-core-notify (herdr-core--find-agent "t1") "working" "blocked")
        (should-not notified)
        (herdr-core-notify (herdr-core--find-agent "t1") "working" "error")
        (should (= (length notified) 1))
        ;; In view of a focused frame: quiet, unless notify-when-visible.
        (herdr-core--attach-agent (herdr-core--find-agent "t1"))
        (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t)))
          (herdr-core-notify (herdr-core--find-agent "t1") "working" "error")
          (should (= (length notified) 1))
          (let ((herdr-core-notify-when-visible t))
            (herdr-core-notify (herdr-core--find-agent "t1") "working" "error")
            (should (= (length notified) 2))))))
    (should (equal (herdr-core-message-notify "a" "b") "a: b"))))

;;;; Keys and the menu

(ert-deftest herdr-core-keymap-prefix-and-menu-suffixes ()
  "The command map goes under `herdr-core-keymap-prefix'; the menu is built from
`herdr-core-menu-suffixes', with function descriptions."
  (customize-set-variable 'herdr-core-keymap-prefix "C-c 8")
  (unwind-protect
      (progn
        (should (eq (keymap-global-lookup "C-c 8 ]") #'herdr-core-next-agent))
        (should (eq (keymap-global-lookup "C-c 8 M") #'herdr-core-switch-machine))
        (dolist (k '("l" "s" "a" "t" "A" "G" "w" "v" "SPC" "]" "[" "." "b" "M" "S" "T" "n" "r" "R" "k" "i" "p" "W" "e" "m"))
          (should (commandp (keymap-lookup herdr-core-command-map k)))))
    (customize-set-variable 'herdr-core-keymap-prefix nil))
  (should-not (keymapp (keymap-global-lookup "C-c 8")))
  (let ((herdr-core--menu-repeat-key nil)
        (herdr-core-menu-suffixes '(("x" "Mine" herdr-core-agents)
                                    ("y" (lambda () "Dynamic") herdr-core-next-agent))))
    (let ((suffixes (herdr-core--menu-children nil)))
      (should (= (length suffixes) 2))
      (should (equal (mapcar (lambda (s) (plist-get (if (vectorp s) (aref s 2) (nth 2 s)) :key)) suffixes)
                     '("x" "y")))))
  ;; Every default entry is a command.
  (dolist (s herdr-core-menu-suffixes)
    (should (commandp (nth 2 s)))))

;;;; Live, against a real herdr-core

(ert-deftest herdr-core-live-launch-read-render-send-and-machines ()
  "A real scratch core: launch through the plain-hc template (labels and keep
as the Herdr app needs), read and render what the terminal printed, send to
it, wait on it, and see a pod machine's kind from `hc machine ls'."
  (let ((bin (test-herdr-core--hc-bin)))
    (skip-unless bin)
    (let* ((root (let ((temporary-file-directory (if (file-directory-p "/tmp/") "/tmp/" temporary-file-directory)))
                   (make-temp-file "hf" t)))
           (default-directory (file-name-as-directory root))
           (local (expand-file-name "l" root))
           (pod (expand-file-name "p" root))
           (bindir (expand-file-name "bin" root))
           (path (progn (make-directory bindir t)
                        (make-symbolic-link (file-truename bin) (expand-file-name "herdr-core" bindir))
                        (concat "PATH=" bindir ":/usr/bin:/bin")))
           (pod-env (cons path (test-herdr-core--env pod)))
           (process-environment (append (list path) (test-herdr-core--env local)
                                        (list "HC_SESSION" "HC_TERMINAL_ID")
                                        (seq-remove (lambda (e) (string-match-p "\\`HC_\\(SESSION\\|TERMINAL_ID\\)=" e))
                                                    process-environment)))
           (herdr-core-executable bin)
           (herdr-core-launcher bin)
           (herdr-core-launcher-args herdr-core-hc-run-launcher-args)
           (herdr-core-terminal-function #'test-herdr-core--terminal)
           (test-herdr-core--opened nil)
           (project (expand-file-name "proj" root))
           (herdr-core-launch-directory-function (lambda () project))
           (hc (lambda (env &rest args)
                 (let ((process-environment (append env process-environment))
                       (default-directory (file-name-as-directory root)))
                   (with-temp-buffer
                     (list (apply #'call-process bin nil t nil args) (buffer-string)))))))
      (make-directory project t)
      (with-temp-file (expand-file-name "notes.md" project) (insert "notes\n"))
      (test-herdr-core--fresh
        (unwind-protect
            (progn
              (funcall hc nil "server" "start" "--json")
              (funcall hc pod-env "server" "start" "--json")
              (herdr-core-mode 1)
              (should (test-herdr-core--wait (lambda () herdr-core--snapshot-seen) 10))
              ;; Launch: a program that prints a URL and a file, then reads stdin.
              (herdr-core-new-agent "sh")
              (should (test-herdr-core--wait (lambda () (assoc "*hc: proj · proj-sh*" test-herdr-core--opened)) 15))
              (let* ((address (car (last (nth 2 (assoc "*hc: proj · proj-sh*" test-herdr-core--opened)))))
                     (got (herdr-core--call-json-sync "get" "--json" address)))
                (should (equal (alist-get 'keep got) t))
                (should (equal (herdr-core--label got "herdr.space") "proj"))
                (should (equal (herdr-core--label got "herdr.tab") "proj-sh"))
                (should (equal (herdr-core--label got "session.title") "proj · proj-sh"))
                (should (test-herdr-core--wait (lambda () (herdr-core--find-agent address)) 10))
                (should (equal (alist-get 'space (herdr-core--find-agent address)) "proj"))
                ;; Send a line to its shell; read it back rendered.
                (herdr-core--send (herdr-core--find-agent address)
                                  "echo see https://example.com/pr/1 and notes.md")
                (should (test-herdr-core--wait
                         (lambda () (string-match-p "see https://example.com/pr/1 and notes.md\n"
                                                    (cadr (funcall hc nil "read" address))))
                         15))
                (herdr-core--read address)
                (should (test-herdr-core--wait (lambda () (herdr-core--find-read-buffer address)) 10))
                (with-current-buffer (herdr-core--find-read-buffer address)
                  (should (member "https://example.com/pr/1" (test-herdr-core--buttons 'herdr-core-url)))
                  (should (member "notes.md" (test-herdr-core--buttons 'herdr-core-file))))
                ;; hc wait checks in after its timeout (a shell never reports), and notifies.
                (let* (notified
                       (herdr-core-wait-timeout 1)
                       (herdr-core-notify-function (lambda (&rest args) (push args notified))))
                  (herdr-core-wait-agent (herdr-core--find-agent address))
                  (should (test-herdr-core--wait (lambda () notified) 20))
                  (should (equal (caar notified) "proj · proj-sh: stopped (timeout)")))
                ;; Rename through hc label: the buffer name follows.
                (herdr-core-rename-agent (herdr-core--find-agent address) "proj · renamed")
                (should (test-herdr-core--wait (lambda () (equal (herdr-core-title address) "proj · renamed")) 10)))
              ;; A pod machine: its kind and its agents.
              (funcall hc pod-env "run" "--bg" "--name" "agent" "--label" "herdr.space=pod" "--" "sleep" "600")
              (let ((added (funcall hc nil "machine" "add" "--temp" "--name" "job-z-a1" "--via"
                                    (concat "env -u HC_SESSION " (string-join pod-env " ") " sh -c")
                                    "--json")))
                (should (zerop (car added))))
              (should (test-herdr-core--wait (lambda () (herdr-core--find-agent "job-z-a1/t1")) 20))
              (should (test-herdr-core--wait
                       (lambda () (assoc "job-z-a1" herdr-core--machine-names-cache)) 10))
              (let ((m (seq-find (lambda (m) (equal (alist-get 'name m) "job-z-a1")) (herdr-core-machines))))
                (should (equal (alist-get 'kind m) "pod"))
                (should (equal (mapcar (lambda (a) (alist-get 'address a)) (alist-get 'agents m))
                               '("job-z-a1/t1"))))
              ;; The OpenRPC schema.
              (let ((schema (herdr-core--call-json-sync "schema")))
                (should (alist-get 'openrpc schema))
                (should (seq-find (lambda (m) (equal (alist-get 'name m) "events.subscribe"))
                                  (alist-get 'methods schema))))
              (funcall hc nil "machine" "rm" "job-z-a1" "--json"))
          (herdr-core-mode -1)
          (herdr-core-stop)
          (dolist (b (buffer-list))
            (when (buffer-local-value 'herdr-core--attached-address b)
              (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
          (funcall hc nil "server" "stop")
          (funcall hc pod-env "server" "stop")
          (delete-directory root t))))))

(ert-deftest herdr-core-follow-shows-the-row-agent-in-the-follow-window ()
  "Follow mode shows the agent under the cursor in the follow window, once per row."
  (let ((shown nil) (address "t1")
        (agents '(((address . "t1") (name . "one")) ((address . "t2") (name . "two")))))
    (cl-letf (((symbol-function 'herdr-core--list-address) (lambda (&rest _) address))
              ((symbol-function 'herdr-core--find-agent)
               (lambda (a) (seq-find (lambda (x) (equal (alist-get 'address x) a)) agents)))
              ((symbol-function 'herdr-core-attach-in-window)
               (lambda (w agent &optional observe) (push (list w (alist-get 'name agent) observe) shown))))
      (save-window-excursion
        (delete-other-windows)
        (let ((follow (split-window nil nil 'right)))
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (setq herdr-core--follow-window follow)
            (herdr-core-follow-mode 1)
            (herdr-core-follow-update)
            (herdr-core-follow-update)
            (should (equal shown (list (list follow "one" nil))))
            (setq address "t2")
            (let ((herdr-core-control-follow 'observe)) (herdr-core-follow-update))
            (should (equal (car shown) (list follow "two" t)))
            (should (memq #'herdr-core--follow-schedule post-command-hook))
            (herdr-core-follow-mode -1)
            (should-not (memq #'herdr-core--follow-schedule post-command-hook))))))))

(ert-deftest herdr-core-follow-schedule-debounces ()
  "Motion only schedules: the follow window changes once the cursor rests."
  (let ((herdr-core-control-debounce 0.05) (calls 0))
    (cl-letf (((symbol-function 'herdr-core-follow-update) (lambda (&rest _) (cl-incf calls))))
      (with-temp-buffer
        (dotimes (_ 5) (herdr-core--follow-schedule))
        (should (= calls 0))
        (should (timerp herdr-core--follow-timer))
        (cancel-timer herdr-core--follow-timer)))))

(provide 'test-herdr-core-features)
;;; test-herdr-core-features.el ends here
