;;; yasnippet-bundle.el --- Yet another snippet extension (Auto compiled bundle)
;;; yasnippet.el --- Yet another snippet extension for Emacs.

;; Copyright 2008 pluskid

;; Author: pluskid <pluskid@gmail.com>
;; Created: 02 Mar 2008
;; Version: 0.5.10
;; Keywords: snippet, textmate
;; URL: http://code.google.com/p/yasnippet/
;; EmacsWiki: YaSnippetMode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Basic steps to setup:
;;   1. Place `yasnippet.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'yasnippet)
;;   3. Place the `snippets' directory somewhere.  E.g: ~/.emacs.d/snippets
;;   4. In your .emacs file
;;        (yas/initialize)
;;        (yas/load-directory "~/.emacs.d/snippets")
;;
;; For more information and detailed usage, refer to the project page:
;;      http://code.google.com/p/yasnippet/

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/dont-activate nil
  "If set to t, don't activate yas/minor-mode automatically.")
(make-variable-buffer-local 'yas/dont-activate)

(defvar yas/key-syntaxes (list "w" "w_" "w_." "w_.\\" "^ ")
  "A list of syntax of a key. This list is tried in the order
to try to find a key. For example, if the list is '(\"w\" \"w_\").
And in emacs-lisp-mode, where \"-\" has the syntax of \"_\":

foo-bar

will first try \"bar\", if not found, then \"foo-bar\" is tried.")

(defvar yas/root-directory nil
  "The (list of) root directory that stores the snippets for each
major modes.")

(defvar yas/indent-line t
  "Each (except the 1st) line of the snippet template is indented to
current column if this variable is non-`nil'.")
(make-variable-buffer-local 'yas/indent-line)

(defvar yas/trigger-key (kbd "TAB")
  "The key to bind as a trigger of snippet.")
(defvar yas/next-field-key (kbd "TAB")
  "The key to navigate to next field.")

(defvar yas/keymap (make-sparse-keymap)
  "The keymap of snippet.")
(define-key yas/keymap yas/next-field-key 'yas/next-field-group)
(define-key yas/keymap (kbd "S-TAB") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-iso-lefttab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<S-tab>") 'yas/prev-field-group)
(define-key yas/keymap (kbd "<backtab>") 'yas/prev-field-group)

(defvar yas/show-all-modes-in-menu nil
  "Currently yasnippet only all \"real modes\" to menubar. For
example, you define snippets for \"cc-mode\" and make it the
parent of `c-mode', `c++-mode' and `java-mode'. There's really
no such mode like \"cc-mode\". So we don't show it in the yasnippet
menu to avoid the menu becoming too big with strange modes. The
snippets defined for \"cc-mode\" can still be accessed from
menu-bar->c-mode->parent (or c++-mode, java-mode, all are ok).
However, if you really like to show all modes in the menu, set
this variable to t.")
(defvar yas/use-menu t
  "If this is set to `t', all snippet template of the current
mode will be listed under the menu \"yasnippet\".")
(defvar yas/trigger-symbol " =>"
  "The text that will be used in menu to represent the trigger.")

(defface yas/field-highlight-face
  '((((class color) (background light)) (:background "DarkSeaGreen2"))
    (t (:background "DimGrey")))
  "The face used to highlight a field of snippet.")
(defface yas/mirror-highlight-face
  '((((class color) (background light)) (:background "LightYellow2"))
    (t (:background "gray22")))
  "The face used to highlight mirror fields of a snippet.")

(defvar yas/window-system-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. This function
is called to let user select one of them. `yas/text-popup-function'
is used instead when not in a window system.")
(defvar yas/text-popup-function #'yas/dropdown-list-popup-for-template
  "When there's multiple candidate for a snippet key. If not in a
window system, this function is called to let user select one of
them. `yas/window-system-popup-function' is used instead when in
a window system.")

(defvar yas/extra-mode-hooks
  '()
  "A list of mode-hook that should be hooked to enable yas/minor-mode.
Most modes need no special consideration.  Some mode (like `ruby-mode')
doesn't call `after-change-major-mode-hook' need to be hooked explicitly.")
(mapc '(lambda (x)
         (add-to-list 'yas/extra-mode-hooks
                      x))
      '(ruby-mode-hook actionscript-mode-hook ox-mode-hook python-mode-hook))

(defvar yas/after-exit-snippet-hook
  '()
  "Hooks to run after a snippet exited.
The hooks will be run in an environment where some variables bound to
proper values:
 * yas/snippet-beg : The beginning of the region of the snippet.
 * yas/snippet-end : Similar to beg.")

(defvar yas/before-expand-snippet-hook
  '()
  "Hooks to run after a before expanding a snippet.")

(defvar yas/buffer-local-condition
  '(if (and (not (bobp))
            (or (equal 'font-lock-comment-face
                       (get-char-property (1- (point))
                                          'face))
                (equal 'font-lock-string-face
                       (get-char-property (1- (point))
                                          'face))))
       '(require-snippet-condition . force-in-comment)
     t)
  "Condition to yasnippet local to each buffer.

    * If yas/buffer-local-condition evaluate to nil, snippet
      won't be expanded.

    * If it evaluate to the a cons cell where the car is the
      symbol require-snippet-condition and the cdr is a
      symbol (let's call it requirement):
       * If the snippet has no condition, then it won't be
         expanded.
       * If the snippet has a condition but evaluate to nil or
         error occured during evaluation, it won't be expanded.
       * If the snippet has a condition that evaluate to
         non-nil (let's call it result):
          * If requirement is t, the snippet is ready to be
            expanded.
          * If requirement is eq to result, the snippet is ready
            to be expanded.
          * Otherwise the snippet won't be expanded.
    * If it evaluate to other non-nil value:
       * If the snippet has no condition, or has a condition that
         evaluate to non-nil, it is ready to be expanded.
       * Otherwise, it won't be expanded.

Here's an example:

 (add-hook 'python-mode-hook
           '(lambda ()
              (setq yas/buffer-local-condition
                    '(if (python-in-string/comment)
                         '(require-snippet-condition . force-in-comment)
                       t))))")
(eval-when-compile
  (make-variable-buffer-local 'yas/buffer-local-condition))

(defvar yas/fallback-behavior 'call-other-command
  "The fall back behavior of YASnippet when it can't find a snippet
to expand.

 * 'call-other-command means try to temporarily disable
    YASnippet and call other command bound to `yas/trigger-key'.
 * 'return-nil means return nil.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/substr (str pattern &optional group)
  "Search PATTERN in STR. If found, the content of group
  GROUP (default 0) is returned, or else the original STR will be
  returned."
  (let ((grp (or group 0)))
    (save-match-data
      (if (string-match pattern str)
          (match-string-no-properties grp str)
        str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/version "0.5.10")

(defvar yas/snippet-tables (make-hash-table)
  "A hash table of snippet tables corresponding to each major-mode.")
(defvar yas/menu-table (make-hash-table)
  "A hash table of menus of corresponding major-mode.")
(defvar yas/menu-keymap (make-sparse-keymap "YASnippet"))
;; empty menu will cause problems, so we insert some items
(define-key yas/menu-keymap [yas/about]
  '(menu-item "About" yas/about))
(define-key yas/menu-keymap [yas/reload]
  '(menu-item "Reload all snippets" yas/reload-all))
(define-key yas/menu-keymap [yas/load]
  '(menu-item "Load snippets..." yas/load-directory))
(define-key yas/menu-keymap [yas/separator]
  '(menu-item "--"))

(defvar yas/known-modes
  '(ruby-mode rst-mode markdown-mode)
  "A list of mode which is well known but not part of emacs.")
(defconst yas/escape-backslash
  (concat "YASESCAPE" "BACKSLASH" "PROTECTGUARD"))
(defconst yas/escape-dollar
  (concat "YASESCAPE" "DOLLAR" "PROTECTGUARD"))
(defconst yas/escape-backquote
  (concat "YASESCAPE" "BACKQUOTE" "PROTECTGUARD"))

(defconst yas/field-regexp
  (concat "$\\([0-9]+\\)" "\\|"
          "${\\(?:\\([0-9]+\\):\\)?\\([^}]*\\)}"))

(defvar yas/snippet-id-seed 0
  "Contains the next id for a snippet.")
(defun yas/snippet-next-id ()
  (let ((id yas/snippet-id-seed))
    (incf yas/snippet-id-seed)
    id))

(defvar yas/overlay-modification-hooks
  (list 'yas/overlay-modification-hook)
  "The list of hooks to the overlay modification event.")
(defvar yas/overlay-insert-in-front-hooks
  (list 'yas/overlay-insert-in-front-hook)
  "The list of hooks of the overlay inserted in front event.")
(defvar yas/keymap-overlay-modification-hooks
  (list 'yas/overlay-maybe-insert-behind-hook)
  "The list of hooks of the big keymap overlay modification event.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yas/minor-mode-map (make-sparse-keymap)
  "The keymap of yas/minor-mode")
(defvar yas/minor-mode-on-hook nil
  "Hook to call when yas/minor-mode is on.")
(defvar yas/minor-mode-off-hook nil
  "Hook to call when yas/minor-mode is off.")
(define-minor-mode yas/minor-mode
  "Toggle YASnippet mode.
With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When YASnippet mode is enabled, the TAB key
expands snippets of code depending on the mode.

You can customize the key through `yas/trigger-key'."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " yas"
  :group 'editing
  (define-key yas/minor-mode-map yas/trigger-key 'yas/expand))


(defun yas/minor-mode-auto-on ()
  "Turn on YASnippet minor mode unless `yas/dont-activate' is
set to t."
  (unless yas/dont-activate
    (yas/minor-mode-on)))
(defun yas/minor-mode-on ()
  "Turn on YASnippet minor mode."
  (interactive)
  (yas/minor-mode 1))
(defun yas/minor-mode-off ()
  "Turn off YASnippet minor mode."
  (interactive)
  (yas/minor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (yas/template (:constructor yas/make-template
                                       (content name condition)))
  "A template for a snippet."
  content
  name
  condition)
(defstruct (yas/snippet (:constructor yas/make-snippet ()))
  "A snippet.

Description of some fields:

`yas/snippet-saved-buffer-undo-list' saves the value of
`buffer-undo-list' just after the snippet has been expanded. This
is to be restored when the snippet is cleaned up. Thus the
snippet expansion can still be undone after
`yas/cleanup-snippet', even if field-level undo steps were
recorded.

`yas/snippet-end-marker' saves the actual end position of the
snippets main overlay, at the time the snippet was cleaned
up. Thus `yas/undo-expand-snippet' can clean it up properly.

TODO: describe the rest of the fields"
  (groups nil)
  (exit-marker nil)
  (id (yas/snippet-next-id) :read-only t)
  (overlay nil)
  (saved-buffer-undo-list nil)
  (end-marker nil))

(defstruct (yas/group (:constructor yas/make-group (primary-field snippet)))
  "A group contains a list of field with the same number."
  primary-field
  (fields (list primary-field))
  (next nil)
  (prev nil)
  snippet)
(defstruct (yas/field
            (:constructor yas/make-field (overlay number value transform)))
  "A field in a snippet."
  overlay
  number
  transform
  value)
(defstruct (yas/snippet-table (:constructor yas/make-snippet-table ()))
  "A table to store snippets for a perticular mode."
  (hash (make-hash-table :test 'equal))
  (parent nil))

(defun yas/snippet-valid? (snippet)
  "See if snippet is valid (ie. still alive)."
  (and (not (null snippet))
       (not (null (yas/snippet-overlay snippet)))
       (not (null (overlay-start (yas/snippet-overlay snippet))))))

(defun yas/snippet-add-field (snippet field)
  "Add FIELD to SNIPPET."
  (let ((group (find field
                     (yas/snippet-groups snippet)
                     :test
                     '(lambda (field group)
                        (and (not (null (yas/field-number field)))
                             (not (null (yas/group-number group)))
                             (= (yas/field-number field)
                                (yas/group-number group)))))))
    (if group
        (yas/group-add-field group field)
      (push (yas/make-group field snippet)
            (yas/snippet-groups snippet)))))

(defun yas/group-value (group)
  "Get the default value of the field group."
  (or (yas/field-value
       (yas/group-primary-field group))
      ""))
(defun yas/group-number (group)
  "Get the number of the field GROUP."
  (yas/field-number
   (yas/group-primary-field group)))
(defun yas/group-add-field (group field)
  "Add a FIELD to the field GROUP. If the value of the primary
field is nil and that of the field is not nil, the field is set
as the primary field of the group."
  (push field (yas/group-fields group))
  (when (and (null (yas/field-value (yas/group-primary-field group)))
             (yas/field-value field))
    (setf (yas/group-primary-field group) field)))

(defun yas/snippet-field-compare (field1 field2)
  "Compare two fields. The field with a number is sorted first.
If they both have a number, compare through the number. If neither
have, compare through the start point of the overlay."
  (let ((n1 (yas/field-number field1))
        (n2 (yas/field-number field2)))
    (if n1
        (if n2
            (< n1 n2)
          t)
      (if n2
          nil
        (< (overlay-start (yas/field-overlay field1))
           (overlay-start (yas/field-overlay field2)))))))

(defun yas/template-condition-predicate (condition)
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (eval condition))))
    (error (progn
             (message (format "[yas]error in condition evaluation: %s"
                              (error-message-string err)))
             nil))))

(defun yas/filter-templates-by-condition (templates)
  "Filter the templates using the condition. The rules are:

 * If the template has no condition, it is kept.
 * If the template's condition eval to non-nil, it is kept.
 * Otherwise (eval error or eval to nil) it is filtered."
  (remove-if-not '(lambda (pair)
                    (let ((condition (yas/template-condition (cdr pair))))
                      (if (null condition)
                          (if yas/require-template-condition
                              nil
                            t)
                        (let ((result
                               (yas/template-condition-predicate condition)))
                          (if yas/require-template-condition
                              (if (eq yas/require-template-condition t)
                                  result
                                (eq result yas/require-template-condition))
                            result)))))
                 templates))

(defun yas/snippet-table-fetch (table key)
  "Fetch a snippet binding to KEY from TABLE. If not found,
fetch from parent if any."
  (let ((templates (yas/filter-templates-by-condition
                    (gethash key (yas/snippet-table-hash table)))))
    (when (and (null templates)
               (not (null (yas/snippet-table-parent table))))
      (setq templates (yas/snippet-table-fetch
                       (yas/snippet-table-parent table)
                       key)))
    templates))
(defun yas/snippet-table-store (table full-key key template)
  "Store a snippet template in the table."
  (puthash key
           (yas/modify-alist (gethash key
                                      (yas/snippet-table-hash table))
                             full-key
                             template)
           (yas/snippet-table-hash table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/ensure-minor-mode-priority ()
  "Ensure that the key binding of yas/minor-mode takes priority."
  (unless (eq 'yas/minor-mode
              (caar minor-mode-map-alist))
    (setq minor-mode-map-alist
          (cons
           (cons 'yas/minor-mode yas/minor-mode-map)
           (assq-delete-all 'yas/minor-mode
                            minor-mode-map-alist)))))

(defun yas/real-mode? (mode)
  "Try to find out if MODE is a real mode. The MODE bound to
a function (like `c-mode') is considered real mode. Other well
known mode like `ruby-mode' which is not part of Emacs might
not bound to a function until it is loaded. So yasnippet keeps
a list of modes like this to help the judgement."
  (or (fboundp mode)
      (find mode yas/known-modes)))

(defun yas/eval-string (string)
  "Evaluate STRING and convert the result to string."
  (condition-case err
      (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (format "%s" (eval (read string))))))
    (error (format "(error in elisp evaluation: %s)"
                   (error-message-string err)))))
(defun yas/calculate-field-value (field value)
  "Calculate the value of the field. If there's a transform
for this field, apply it. Otherwise, the value is returned
unmodified."
  (let ((text value)
        (transform (yas/field-transform field)))
    (if transform
        (yas/eval-string transform)
      text)))
(defsubst yas/replace-all (from to)
  "Replace all occurance from FROM to TO."
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))

(defun yas/snippet-table (mode)
  "Get the snippet table corresponding to MODE."
  (let ((table (gethash mode yas/snippet-tables)))
    (unless table
      (setq table (yas/make-snippet-table))
      (puthash mode table yas/snippet-tables))
    table))
(defsubst yas/current-snippet-table ()
  "Get the snippet table for current major-mode."
  (yas/snippet-table major-mode))

(defun yas/menu-keymap-for-mode (mode)
  "Get the menu keymap correspondong to MODE."
  (let ((keymap (gethash mode yas/menu-table)))
    (unless keymap
      (setq keymap (make-sparse-keymap))
      (puthash mode keymap yas/menu-table))
    keymap))

(defun yas/current-key ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end (point))
        (syntaxes yas/key-syntaxes)
        syntax done templates)
    (while (and (not done) syntaxes)
      (setq syntax (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point)))
      (setq templates
            (yas/snippet-table-fetch
             (yas/current-snippet-table)
             (buffer-substring-no-properties start end)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))

(defun yas/synchronize-fields (field-group)
  "Update all fields' text according to the primary field."
  (when (yas/snippet-valid? (yas/group-snippet field-group))
    (save-excursion
      (let* ((inhibit-modification-hooks t)
             (primary (yas/group-primary-field field-group))
             (text (yas/current-field-text primary)))
        ;; For all fields except the primary, replace their text
        (yas/replace-fields-with-value (remove-if #'(lambda (field)
                                                      (equal field primary))
                                                  (yas/group-fields field-group))
                                       text)))))
(defun yas/current-field-text (field)
  (let ((primary-overlay (yas/field-overlay field)))
    (when primary-overlay
      (buffer-substring-no-properties (overlay-start primary-overlay)
                                      (overlay-end primary-overlay)))))


(defun yas/overlay-modification-hook (overlay after? beg end &optional length)
  "Modification hook for snippet field overlay."
  (when (and after? (not undo-in-progress))
    (yas/synchronize-fields (overlay-get overlay 'yas/group))))

(defun yas/overlay-insert-in-front-hook (overlay after? beg end &optional length)
  "Hook for snippet overlay when text is inserted in front of a snippet field."
  (when after?
    (let ((field-group (overlay-get overlay 'yas/group))
          (inhibit-modification-hooks t))
      (when (not (overlay-get overlay 'yas/modified?))
        (overlay-put overlay 'yas/modified? t)
        (when (> (overlay-end overlay) end)
          (save-excursion
            (goto-char end)
            (delete-char (- (overlay-end overlay) end)))))
      (yas/synchronize-fields field-group))))

(defun yas/overlay-maybe-insert-behind-hook (overlay after? beg end &optional length)
  "Insert behind hook sometimes doesn't get called. I don't know why.
So I add modification hook in the big overlay and try to detect `insert-behind'
event manually."
  (when after?
    (cond ((and (= beg end)
                (> length 0)
                (= (overlay-start overlay)
                   (overlay-end overlay)))
           (yas/exit-snippet (overlay-get overlay 'yas/snippet-reference)))
          ((and (= length 0)
                (> end beg)
                (null (yas/current-snippet-overlay beg))
                (not (bobp)))
           (let ((field-overlay (yas/current-snippet-overlay (1- beg))))
             (if field-overlay
                 (when (= beg (overlay-end field-overlay))
                   (move-overlay field-overlay
                                 (overlay-start field-overlay)
                                 end)
                   (yas/synchronize-fields (overlay-get field-overlay 'yas/group)))
               (let ((snippet (yas/snippet-of-current-keymap))
                     (done nil))
                 (if snippet
                     (do* ((groups (yas/snippet-groups snippet) (cdr groups))
                           (group (car groups) (car groups)))
                         ((or (null groups)
                              done))
                       (setq field-overlay (yas/field-overlay
                                            (yas/group-primary-field group)))
                       (when (and (= (overlay-start field-overlay)
                                     (overlay-end field-overlay))
                                  (= beg
                                     (overlay-start field-overlay)))
                         (move-overlay field-overlay beg end)
                         (yas/synchronize-fields group)
                         (setq done t)))))))))))

(defun yas/remove-recent-undo-from-history ()
  (let ((undo (car buffer-undo-list)))
    (while (null undo)
      (setq buffer-undo-list (cdr buffer-undo-list))
      (setq undo (car buffer-undo-list)))
    ;; Remove this undo operation record
    (setq buffer-undo-list (cdr buffer-undo-list))))

(defun yas/undo-expand-snippet (start key snippet)
  "Undo a snippet expansion. Delete the overlays. This undo can't be
redo-ed."
  (yas/remove-recent-undo-from-history)
  (let ((inhibit-modification-hooks t)
        (buffer-undo-list t))
    (yas/exit-snippet snippet)
    (goto-char start)
    (delete-char (- (yas/snippet-end-marker snippet)
                    start))
    (insert key)))

(defun yas/replace-fields-with-value (fields text)
  "In all of the fields of the snippet group GROUP fields, delete
whatever value (string) existed and insert TEXT instead.

The string to insert is calculated according to
`yas/calculate-field-value', which might insert different text
for each field."
  (dolist (field fields)
    (let* ((overlay (yas/field-overlay field))
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (length (- end start)))
      (goto-char start)
      (insert (yas/calculate-field-value field text))
      (if (eq length 0)
	  (move-overlay overlay start (point)))
      (delete-char length))))

(defun yas/expand-snippet (start end template)
  "Expand snippet at current point. Text between START and END
will be deleted before inserting template."
  (run-hooks 'yas/before-expand-snippet-hook)

  (goto-char start)

  (let ((key (buffer-substring-no-properties start end))
        (original-undo-list buffer-undo-list) ;; save previous undo information
        (inhibit-modification-hooks t)
        (length (- end start))
        (column (current-column)))
    (save-restriction
      (narrow-to-region start start)

      (setq buffer-undo-list t) ;; disable undo for a short while
      (insert template)

      ;; Step 1: do necessary indent
      (when yas/indent-line
        (let* ((indent (if indent-tabs-mode
                           (concat (make-string (/ column tab-width) ?\t)
                                   (make-string (% column tab-width) ?\ ))
                         (make-string column ?\ ))))
          (goto-char (point-min))
          (while (and (zerop (forward-line))
                      (= (current-column) 0))
            (insert indent))))

      ;; Step 2: protect backslash and backquote
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)

      ;; Step 3: evaluate all backquotes
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`]*\\)`" nil t)
        ;; go back so that (current-column) in elisp code evaluation
        ;; will calculate to a meaningful value
        (goto-char (match-beginning 0))
        (replace-match (yas/eval-string (match-string-no-properties 1))
                       t t))

      ;; Step 4: protect all escapes, including backslash and backquot
      ;; which may be produced in Step 3
      (yas/replace-all "\\\\" yas/escape-backslash)
      (yas/replace-all "\\`" yas/escape-backquote)
      (yas/replace-all "\\$" yas/escape-dollar)

      ;; Step 5: Create and register a brand new snippet in the local
      ;; `yas/registered-snippets' var. Create fields.
      (let ((snippet (yas/register-snippet (yas/make-snippet))))
        (goto-char (point-min))
        (while (re-search-forward yas/field-regexp nil t)
          (let ((number (or (match-string-no-properties 1)
                            (match-string-no-properties 2)))
                (transform nil)
                (value (match-string-no-properties 3)))
            (when (eq (elt value 0) ?\$)
              (setq transform (substring value 1))
              (setq value nil))
            (if (and number
                     (string= "0" number))
                (progn
                  (replace-match "")
                  (setf (yas/snippet-exit-marker snippet)
                        (copy-marker (point) t)))
              (yas/snippet-add-field
               snippet
               (yas/make-field
                (make-overlay (match-beginning 0) (match-end 0))
                (and number (string-to-number number))
                value
                transform)))))

        ;; Step 6: Sort and link each field group
        (setf (yas/snippet-groups snippet)
              (sort (yas/snippet-groups snippet)
                    '(lambda (group1 group2)
                       (yas/snippet-field-compare
                        (yas/group-primary-field group1)
                        (yas/group-primary-field group2)))))
        (let ((prev nil))
          (dolist (group (yas/snippet-groups snippet))
            (setf (yas/group-prev group) prev)
            (when prev
              (setf (yas/group-next prev) group))
            (setq prev group)))

        ;; Step 7: Create keymap overlay for snippet
        (let ((overlay (make-overlay (point-min)
                                     (point-max)
                                     nil
                                     nil
                                     t)))
          (overlay-put overlay
                       'modification-hooks
                       yas/keymap-overlay-modification-hooks)
          (overlay-put overlay
                       'insert-behind-hooks
                       yas/keymap-overlay-modification-hooks)
          (overlay-put overlay 'keymap yas/keymap)
          (overlay-put overlay 'yas/snippet-reference snippet)
          (setf (yas/snippet-overlay snippet) overlay)
          (setf (yas/snippet-end-marker snippet) (overlay-end overlay)))

        ;; Step 8: Replace fields with default values
        (dolist (group (yas/snippet-groups snippet))
          (yas/replace-fields-with-value (yas/group-fields group)
                                         (yas/group-value group)))

        ;; Step 9: restore all escape characters
        (yas/replace-all yas/escape-dollar "$")
        (yas/replace-all yas/escape-backquote "`")
        (yas/replace-all yas/escape-backslash "\\")

        ;; Step 10: Set up properties of overlays
        (dolist (group (yas/snippet-groups snippet))
          (let ((overlay (yas/field-overlay
                          (yas/group-primary-field group))))
            (overlay-put overlay 'yas/snippet snippet)
            (overlay-put overlay 'yas/group group)
            (overlay-put overlay 'yas/modified? nil)
            (overlay-put overlay 'modification-hooks yas/overlay-modification-hooks)
            (overlay-put overlay 'insert-in-front-hooks yas/overlay-insert-in-front-hooks)
            (overlay-put overlay 'face 'yas/field-highlight-face)
            (dolist (field (yas/group-fields group))
              (unless (equal overlay (yas/field-overlay field))
                (overlay-put (yas/field-overlay field)
                             'face
                             'yas/mirror-highlight-face)))))

        ;; Step 11: move to end and make sure exit-marker exist
        (goto-char (point-max))
        (unless (yas/snippet-exit-marker snippet)
          (setf (yas/snippet-exit-marker snippet) (copy-marker (point) t)))

        ;; Step 12: Construct undo information
        (unless (eq original-undo-list t)
          (add-to-list 'original-undo-list
                       `(apply yas/undo-expand-snippet
                               ,(point-min)
                               ,key
                               ,snippet)))

        ;; Step 13: remove the trigger key
        (widen)
        (delete-char length)

        ;; Step 14: Do necessary indenting
        (save-excursion
          (let ((ovst (overlay-start (yas/snippet-overlay snippet)))
                (oven (copy-marker
                       (1+ (overlay-end (yas/snippet-overlay snippet))))))
            (when (and ovst oven)
              (goto-char ovst)
              (while (re-search-forward "$>" oven t)
                (replace-match "")
                (indent-according-to-mode)))))

        ;; Step 15: Restore undo information, and also save it for future use.
        (setf (yas/snippet-saved-buffer-undo-list snippet) original-undo-list)
        (setq buffer-undo-list original-undo-list)

        ;; Step 16: place the cursor at a proper place
        (let ((groups (yas/snippet-groups snippet))
              (exit-marker (yas/snippet-exit-marker snippet)))
          (if groups
              (goto-char (overlay-start
                          (yas/field-overlay
                           (yas/group-primary-field
                            (car groups)))))
            ;; no need to call exit-snippet, since no overlay created.
            (yas/exit-snippet snippet)))

        ))))

(defun yas/current-snippet-overlay (&optional point)
  "Get the most proper overlay which is belongs to a snippet."
  (let ((point (or point (point)))
        (snippet-overlay nil))
    (dolist (overlay (overlays-at point))
      ;; appending and removing-duplicates fixes a bug when overlays
      ;; are not recognized because point is really at the end
      (when (overlay-get overlay 'yas/snippet)
        (if (null snippet-overlay)
            (setq snippet-overlay overlay)
          (when (> (yas/snippet-id (overlay-get overlay 'yas/snippet))
                   (yas/snippet-id (overlay-get snippet-overlay 'yas/snippet)))
            (setq snippet-overlay overlay)))))
    snippet-overlay))

(defun yas/snippet-of-current-keymap (&optional point)
  "Get the snippet holding the snippet keymap under POINT."
  (let ((point (or point (point)))
        (keymap-snippet nil)
        (snippet nil))
    (dolist (overlay (overlays-at point))
      (setq snippet (overlay-get overlay 'yas/snippet-reference))
      (when snippet
        (if (null keymap-snippet)
            (setq keymap-snippet snippet)
          (when (> (yas/snippet-id snippet)
                   (yas/snippet-id keymap-snippet))
            (setq keymap-snippet snippet)))))
    keymap-snippet))

(defun yas/current-overlay-for-navigation ()
  "Get current overlay for navigation. Might be overlay at current or previous point."
  (let ((overlay1 (yas/current-snippet-overlay))
        (overlay2 (if (bobp)
                      nil
                    (yas/current-snippet-overlay (- (point) 1)))))
    (if (null overlay1)
        overlay2
      (if (or (null overlay2)
              (eq (overlay-get overlay1 'yas/snippet)
                  (overlay-get overlay2 'yas/snippet)))
          overlay1
        (if (> (yas/snippet-id (overlay-get overlay2 'yas/snippet))
               (yas/snippet-id (overlay-get overlay1 'yas/snippet)))
            overlay2
          overlay1)))))

(defun yas/navigate-group (group next?)
  "Go to next of previous field group. Exit snippet if none."
  (let ((target (if next?
                    (yas/group-next group)
                  (yas/group-prev group))))
    (if target
        (goto-char (overlay-start
                    (yas/field-overlay
                     (yas/group-primary-field target))))
      (yas/exit-snippet (yas/group-snippet group)))))

(defun yas/parse-template (&optional file-name)
  "Parse the template in the current buffer.
If the buffer contains a line of \"# --\" then the contents
above this line are ignored. Variables can be set above this
line through the syntax:

#name : value

Here's a list of currently recognized variables:

 * name
 * contributor
 * condition
 * key
 * group

#name: #include \"...\"
# --
#include \"$1\""
  (goto-char (point-min))
  (let ((name file-name) template bound condition key group)
    (if (re-search-forward "^# --\n" nil t)
        (progn (setq template
                     (buffer-substring-no-properties (point)
                                                     (point-max)))
               (setq bound (point))
               (goto-char (point-min))
               (while (re-search-forward "^#\\([^ ]+?\\) *: *\\(.*\\)$" bound t)
                 (when (string= "name" (match-string-no-properties 1))
                   (setq name (match-string-no-properties 2)))
                 (when (string= "condition" (match-string-no-properties 1))
                   (setq condition (read (match-string-no-properties 2))))
                 (when (string= "group" (match-string-no-properties 1))
                   (setq group (match-string-no-properties 2)))
                 (when (string= "key" (match-string-no-properties 1))
                   (setq key (match-string-no-properties 2)))))
      (setq template
            (buffer-substring-no-properties (point-min) (point-max))))
    (list key template name condition group)))

(defun yas/directory-files (directory file?)
  "Return directory files or subdirectories in full path."
  (remove-if (lambda (file)
               (or (string-match "^\\."
                                 (file-name-nondirectory file))
                   (if file?
                       (file-directory-p file)
                     (not (file-directory-p file)))))
             (directory-files directory t)))

(defun yas/make-menu-binding (template)
  (lexical-let ((template template))
    (lambda ()
      (interactive)
      (yas/expand-snippet (point)
                          (point)
                          template))))

(defun yas/modify-alist (alist key value)
  "Modify ALIST to map KEY to VALUE. return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value)
              alist)
      (setcdr pair value)
      alist)))

(defun yas/fake-keymap-for-popup (templates)
  "Create a fake keymap for popup menu usage."
  (cons 'keymap
        (mapcar (lambda (pair)
                  (let* ((template (cdr pair))
                         (name (yas/template-name template))
                         (content (yas/template-content template)))
                    (list content 'menu-item name t)))
                templates)))

(defun yas/point-to-coord (&optional point)
  "Get the xoffset/yoffset information of POINT.
If POINT is not given, default is to current point.
If `posn-at-point' is not available (like in Emacs 21.3),
t is returned simply."
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (or point (point))))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))

(defun yas/x-popup-menu-for-template (templates)
  "Show a popup menu listing templates to let the user select one."
  (car (x-popup-menu (yas/point-to-coord)
                     (yas/fake-keymap-for-popup templates))))
(defun yas/text-popup-for-template (templates)
  "Can't display popup menu in text mode. Just select the first one."
  (yas/template-content (cdar templates)))
(defun yas/dropdown-list-popup-for-template (templates)
  "Use dropdown-list.el to popup for templates. Better than the
default \"select first\" behavior of `yas/text-popup-for-template'.
You can also use this in window-system.

NOTE: You need to download and install dropdown-list.el to use this."
  (if (fboundp 'dropdown-list)
      (let ((n (dropdown-list (mapcar (lambda (i)
                                        (yas/template-name
                                         (cdr i)))
                                      templates))))
        (if n
            (yas/template-content
             (cdr (nth n templates)))
          nil))
    (error "Please download and install dropdown-list.el to use this")))

(defun yas/popup-for-template (templates)
  (if window-system
      (funcall yas/window-system-popup-function templates)
    (funcall yas/text-popup-function templates)))

(defun yas/load-directory-1 (directory &optional parent)
  "Really do the job of loading snippets from a directory
hierarchy."
  (let ((mode-sym (intern (file-name-nondirectory directory)))
        (snippets nil))
    (with-temp-buffer
      (dolist (file (yas/directory-files directory t))
        (when (file-readable-p file)
          (insert-file-contents file nil nil nil t)
          (let* ((snip (yas/parse-template))
                 (key (or (car snip)
                          (file-name-nondirectory file)))
                 (snip (cdr snip)))
            (push (cons key snip) snippets)))))
    (yas/define-snippets mode-sym
                         snippets
                         parent)
    (dolist (subdir (yas/directory-files directory nil))
      (yas/load-directory-1 subdir mode-sym))))

(defun yas/quote-string (string)
  "Escape and quote STRING.
foo\"bar\\! -> \"foo\\\"bar\\\\!\""
  (concat "\""
          (replace-regexp-in-string "[\\\"]"
                                    "\\\\\\&"
                                    string
                                    t)
          "\""))

(defun yas/compile-bundle
  (&optional yasnippet yasnippet-bundle snippet-roots code)
  "Compile snippets in SNIPPET-ROOTS to a single bundle file.
SNIPPET-ROOTS is a list of root directories that contains the snippets
definition. YASNIPPET is the yasnippet.el file path. YASNIPPET-BUNDLE
is the output file of the compile result. CODE is the code you would
like to used to initialize yasnippet. Here's the default value for
all the parameters:

 (yas/compile-bundle \"yasnippet.el\"
                     \"./yasnippet-bundle.el\"
                     '(\"snippets\")
                     \"(yas/initialize)\")"
  (when (null yasnippet)
    (setq yasnippet "yasnippet.el"))
  (when (null yasnippet-bundle)
    (setq yasnippet-bundle "./yasnippet-bundle.el"))
  (when (null snippet-roots)
    (setq snippet-roots '("snippets")))
  (when (null code)
    (setq code (concat "(yas/initialize-bundle)"
           "\n;;;###autoload"               ; break through so that won't
           "(require 'yasnippet-bundle)"))) ; be treated as magic comment

  (let ((dirs (or (and (listp snippet-roots) snippet-roots)
                  (list snippet-roots)))
        (bundle-buffer nil))
    (with-temp-buffer
      (setq bundle-buffer (current-buffer))
      (insert ";;; yasnippet-bundle.el --- "
              "Yet another snippet extension (Auto compiled bundle)\n")
      (insert-file-contents yasnippet)
      (goto-char (point-max))
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert ";;;;      Auto-generated code         ;;;;\n")
      (insert ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
      (insert "(defun yas/initialize-bundle ()\n"
              "  \"Initialize YASnippet and load snippets in the bundle.\""
              "  (yas/initialize)\n")
      (flet ((yas/define-snippets
              (mode snippets &optional parent)
              (with-current-buffer bundle-buffer
                (insert ";;; snippets for " (symbol-name mode) "\n")
                (insert "(yas/define-snippets '" (symbol-name mode) "\n")
                (insert "'(\n")
                (dolist (snippet snippets)
                  (insert "  ("
                          (yas/quote-string (car snippet))
                          " "
                          (yas/quote-string (nth 1 snippet))
                          " "
                          (if (nth 2 snippet)
                              (yas/quote-string (nth 2 snippet))
                            "nil")
                          " "
                          (if (nth 3 snippet)
                              (format "'%s" (nth 3 snippet))
                            "nil")
                          " "
                          (if (nth 4 snippet)
                              (yas/quote-string (nth 4 snippet))
                            "nil")
                          ")\n"))
                (insert "  )\n")
                (insert (if parent
                            (concat "'" (symbol-name parent))
                          "nil")
                        ")\n\n"))))
        (dolist (dir dirs)
          (dolist (subdir (yas/directory-files dir nil))
            (yas/load-directory-1 subdir nil))))

      (insert ")\n\n" code "\n")
      (insert "(provide '"
              (file-name-nondirectory
               (file-name-sans-extension
                yasnippet-bundle))
              ")\n")
      (insert ";;; "
              (file-name-nondirectory yasnippet-bundle)
              " ends here\n")
      (setq buffer-file-name yasnippet-bundle)
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/about ()
  (interactive)
  (message (concat "yasnippet (version "
                   yas/version
                   ") -- pluskid <pluskid@gmail.com>")))
(defun yas/reload-all ()
  "Reload all snippets."
  (interactive)
  (if yas/root-directory
      (if (listp yas/root-directory)
          (dolist (directory yas/root-directory)
            (yas/load-directory directory))
        (yas/load-directory yas/root-directory))
    (call-interactively 'yas/load-directory))
  (message "done."))

(defun yas/load-directory (directory)
  "Load snippet definition from a directory hierarchy.
Below the top-level directory, each directory is a mode
name.  And under each subdirectory, each file is a definition
of a snippet.  The file name is the trigger key and the
content of the file is the template."
  (interactive "DSelect the root directory: ")
  (unless (file-directory-p directory)
    (error "Error %s not a directory" directory))
  (add-to-list 'yas/root-directory directory)
  (dolist (dir (yas/directory-files directory nil))
    (yas/load-directory-1 dir))
  (when (interactive-p)
    (message "done.")))

(defun yas/initialize ()
  "Do necessary initialization."
  (add-hook 'after-change-major-mode-hook
            'yas/minor-mode-auto-on)
  (dolist (hook yas/extra-mode-hooks)
    (add-hook hook
              'yas/minor-mode-auto-on))
  (add-hook 'yas/minor-mode-on-hook
            'yas/ensure-minor-mode-priority)
  (when yas/use-menu
    (define-key-after
      (lookup-key global-map [menu-bar])
      [yasnippet]
      (cons "YASnippet" yas/menu-keymap)
      'buffer)))

(defun yas/define-snippets (mode snippets &optional parent-mode)
  "Define snippets for MODE.  SNIPPETS is a list of
snippet definition, of the following form:

 (KEY TEMPLATE NAME CONDITION GROUP)

or the NAME, CONDITION or GROUP may be omitted.  The optional 3rd
parameter can be used to specify the parent mode of MODE.  That
is, when looking a snippet in MODE failed, it can refer to its
parent mode.  The PARENT-MODE may not need to be a real mode."
  (let ((snippet-table (yas/snippet-table mode))
        (parent-table (if parent-mode
                          (yas/snippet-table parent-mode)
                        nil))
        (keymap (if yas/use-menu
                    (yas/menu-keymap-for-mode mode)
                  nil)))
    (when parent-table
      (setf (yas/snippet-table-parent snippet-table)
            parent-table)
      (when yas/use-menu
        (define-key keymap (vector 'parent-mode)
          `(menu-item "parent mode"
                      ,(yas/menu-keymap-for-mode parent-mode)))))
    (when (and yas/use-menu
               (yas/real-mode? mode))
      (define-key yas/menu-keymap (vector mode)
        `(menu-item ,(symbol-name mode) ,keymap)))
    (dolist (snippet snippets)
      (let* ((full-key (car snippet))
             (key (file-name-sans-extension full-key))
             (name (or (nth 2 snippet) (file-name-extension full-key)))
             (condition (nth 3 snippet))
             (group (nth 4 snippet))
             (template (yas/make-template (nth 1 snippet)
                                          (or name key)
                                          condition)))
        (yas/snippet-table-store snippet-table
                                 full-key
                                 key
                                 template)
        (when yas/use-menu
          (let ((group-keymap keymap))
            (when (and (not (null group))
                       (not (string= "" group)))
              (dolist (subgroup (mapcar #'make-symbol
                                        (split-string group "\\.")))
                (let ((subgroup-keymap (lookup-key group-keymap 
                                                   (vector subgroup))))
                  (when (null subgroup-keymap)
                    (setq subgroup-keymap (make-sparse-keymap))
                    (define-key group-keymap (vector subgroup)
                      `(menu-item ,(symbol-name subgroup)
                                  ,subgroup-keymap)))
                  (setq group-keymap subgroup-keymap))))
            (define-key group-keymap (vector (make-symbol full-key))
              `(menu-item ,(yas/template-name template)
                          ,(yas/make-menu-binding (yas/template-content 
                                                   template))
                          :keys ,(concat key yas/trigger-symbol)))))))))

(defun yas/set-mode-parent (mode parent)
  "Set parent mode of MODE to PARENT."
  (setf (yas/snippet-table-parent
         (yas/snippet-table mode))
        (yas/snippet-table parent))
  (when yas/use-menu
    (define-key (yas/menu-keymap-for-mode mode) (vector 'parent-mode)
      `(menu-item "parent mode"
                  ,(yas/menu-keymap-for-mode parent)))))

(defun yas/define (mode key template &optional name condition group)
  "Define a snippet.  Expanding KEY into TEMPLATE.
NAME is a description to this template.  Also update
the menu if `yas/use-menu' is `t'.  CONDITION is the
condition attached to this snippet.  If you attach a
condition to a snippet, then it will only be expanded
when the condition evaluated to non-nil."
  (yas/define-snippets mode
                       (list (list key template name condition group))))


(defun yas/hippie-try-expand (first-time?)
  "Integrate with hippie expand.  Just put this function in
`hippie-expand-try-functions-list'."
  (if (not first-time?)
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand))
    (when (and (null (car buffer-undo-list))
               (eq 'apply
                   (car (cadr buffer-undo-list)))
               (eq 'yas/undo-expand-snippet
                   (cadr (cadr buffer-undo-list))))
      (undo 1))
    nil))

(defun yas/expand ()
  "Expand a snippet."
  (interactive)
  (let ((local-condition (yas/template-condition-predicate
                          yas/buffer-local-condition)))
    (if local-condition
        (let ((yas/require-template-condition
               (if (and (consp local-condition)
                        (eq 'require-snippet-condition (car local-condition))
                        (symbolp (cdr local-condition)))
                   (cdr local-condition)
                 nil)))
          (multiple-value-bind (templates start end) (yas/current-key)
            (if templates
                (let ((template (if (null (cdr templates)) ; only 1 template
                                    (yas/template-content (cdar templates))
                                  (yas/popup-for-template templates))))
                  (if template
                      (progn (yas/expand-snippet start end template)
                             'expanded) ; expanded successfully
                    'interrupted))      ; interrupted by user
              (if (eq yas/fallback-behavior 'return-nil)
                  nil                   ; return nil
                (let* ((yas/minor-mode nil)
                       (command (key-binding yas/trigger-key)))
                  (when (commandp command)
                    (call-interactively command))))))))))

(defun yas/next-field-group ()
  "Navigate to next field group.  If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
        (yas/navigate-group (overlay-get overlay 'yas/group) t)
      (let ((snippet (yas/snippet-of-current-keymap))
            (done nil))
        (if snippet
            (do* ((groups (yas/snippet-groups snippet) (cdr groups))
                  (group (car groups) (car groups)))
                ((or (null groups)
                     done)
                 (unless done
                   (let* ((overlay (yas/snippet-overlay snippet))
                          (keymap (overlay-get overlay 'keymap))
                          (command nil))
                     (overlay-put overlay 'keymap nil)
                     (overlay-put overlay 'yas/snippet-reference nil)
                     (setq command (key-binding yas/next-field-key))
                     (when (commandp command)
                       (call-interactively command))
                     (overlay-put overlay 'keymap keymap)
                     (overlay-put overlay 'yas/snippet-reference snippet))))
              (when (= (point)
                       (overlay-start
                        (yas/field-overlay
                         (yas/group-primary-field group))))
                (setq done t)
                (yas/navigate-group group t))))))))

(defun yas/prev-field-group ()
  "Navigate to prev field group.  If there's none, exit the snippet."
  (interactive)
  (let ((overlay (yas/current-overlay-for-navigation)))
    (if overlay
        (yas/navigate-group (overlay-get overlay 'yas/group) nil)
      (let ((snippet (yas/snippet-of-current-keymap))
            (done nil))
        (if snippet
            (do* ((groups (yas/snippet-groups snippet) (cdr groups))
                  (group (car groups) (car groups)))
                ((or (null groups)
                     done)
                 (unless done (message "Not in a snippet field.")))
              (when (= (point)
                       (overlay-start
                        (yas/field-overlay
                         (yas/group-primary-field group))))
                (setq done t)
                (yas/navigate-group group nil)))
          (message "Not in a snippet field."))))))

(defun yas/exit-snippet (snippet)
  "Goto exit-marker of SNIPPET and cleanup the snippe.  Cleaning
up the snippet does not delete it!"
  (interactive)
  (goto-char (yas/snippet-exit-marker snippet))
  (yas/cleanup-snippet snippet))

;; Snippet register and unregister routines.
;;
;; XXX: Commentary on this section by joaot.
;;
;; These routines, along with minor modifications upwards, allow some
;; management of currently active snippets.
;;
;; The idea is to temporarily set `post-command-hook' while locally
;; "registered" snippets last.  After each command,
;; `yas/check-cleanup-snippet' is run, checking for some condition and
;; possibly unregistering the snippet.  When no more snippets are
;; registered, the `post-command-hook' is cleared up.
;;
;; They were introduced to fix bug 28
;; "http://code.google.com/p/yasnippet/issues/detail?id=28".  Whenever
;; point exits a snippet or a snippet field, *all* snippets are
;; destroyed.
;;
;; Also, this scheme have been reused to fix bug 33
;; "http://code.google.com/p/yasnippet/issues/detail?id=33", which
;; deals with undoing changes when part of the snippet's field have
;; been filled out already.  See commentary on "Field-level undo" below
;;

(defvar yas/registered-snippets nil
  "A hash table holding all active snippets")
(eval-when-compile
  (make-variable-buffer-local 'yas/registered-snippets))
(defun yas/get-registered-snippets ()
  (when (null yas/registered-snippets)
    (setq yas/registered-snippets
	  (make-hash-table :test 'eq)))
  yas/registered-snippets)

(defun yas/register-snippet (snippet)
  "Register SNIPPET in the `yas/registered-snippets' table.  Add a
`yas/check-cleanup-snippet' function to the buffer-local
`post-command-hook' that should exist while at least one
registered snippet exists in the current buffer.  Return snippet"
  (puthash (yas/snippet-id snippet) snippet (yas/get-registered-snippets))
  (add-hook 'pre-command-hook  'yas/field-undo-before-hook 'append 'local)
  (add-hook 'post-command-hook 'yas/check-cleanup-snippet 'append 'local)
  (add-hook 'post-command-hook 'yas/field-undo-after-hook 'append 'local)
  snippet)

(defun yas/unregister-snippet (snippet)
  "Unregister snippet from the `yas/registered-snippets'
table.  Remove `yas/check-cleanup-snippet' from the buffer-local
`post-command-hook' if no more snippets registered in the
current buffer."
  (remhash (yas/snippet-id snippet) (yas/get-registered-snippets))
  (when (eq 0
            (hash-table-count (yas/get-registered-snippets)))
    (remove-hook 'pre-command-hook  'yas/field-undo-before-hook 'local)
    (remove-hook 'post-command-hook 'yas/field-undo-after-hook 'local)
    (remove-hook 'post-command-hook 'yas/check-cleanup-snippet 'local)))

(defun yas/exterminate-snippets ()
  "Remove all locally registered snippets and remove
  `yas/check-cleanup-snippet' from the `post-command-hook'"
  (interactive)
  (maphash #'(lambda (key snippet) (yas/cleanup-snippet snippet))
           (yas/get-registered-snippets)))

(defun yas/cleanup-snippet (snippet)
  "Cleanup SNIPPET, but leave point as it is.  This renders the
snippet as ordinary text"
  (let* ((overlay (yas/snippet-overlay snippet))
         (yas/snippet-beg (overlay-start overlay))
         (yas/snippet-end (overlay-end overlay)))
    ;; save the end of the moribund snippet in case we need to undo
    ;; its original expansion.  This is used by `yas/undo-expand-snippet'
    (when (and overlay
               (overlay-buffer overlay))
      (setf (yas/snippet-end-marker snippet) yas/snippet-end)
      (delete-overlay overlay))
    (dolist (group (yas/snippet-groups snippet))
      (dolist (field (yas/group-fields group))
        (delete-overlay (yas/field-overlay field))))
    (run-hooks 'yas/after-exit-snippet-hook))
  (yas/unregister-snippet snippet)
  (setq buffer-undo-list (yas/snippet-saved-buffer-undo-list snippet)))

(defun yas/check-cleanup-snippet ()
  "Checks if point exited any of the fields of the snippet, if so
clean it up.

This function is part of `post-command-hook' while
registered snippets last."
  (let ((snippet (yas/snippet-of-current-keymap)))
    (cond ( ;;
           ;; No snippet at point, cleanup *all* snippets
           ;;
           (null snippet)
           (yas/exterminate-snippets))
          ( ;;
           ;; A snippet exits at point, but point is out of any
           ;; primary snippet field.
           (and snippet
                (notany #'(lambda (group)
                            (let ((primary-overlay (yas/field-overlay (yas/group-primary-field group))))
                              (and (>= (point) (overlay-start primary-overlay))
                                   (<= (point) (overlay-end primary-overlay)))))
                        (yas/snippet-groups snippet)))
           (yas/cleanup-snippet snippet))
          ( ;;
           ;; Snippet at point, and point inside a snippet field,
           ;; everything is normal
           ;;
           t
           nil))))

;; Field-level undo functionality
;;
;; XXX: Commentary on this section by joaot.
;;
;; "Field-level undo" means undoing for bits of snippet fields that have
;; already been filled out.  Because this is kind of experimental, I
;; have called it "field-undo", to distinguish it from regular undo
;; like the one used by `yas/undo-expand-snippet' to undo the original
;; snippet expansion.
;;
;; Field level undo allows no redos.  Also, field level undo undoes any
;; change, even if it is only one character long.  This might be
;; implemented in the future.
;;
;; Field level undo cooperates with normal undo and seems transparet
;; to the `undo' command.  The basic idea is the same as with snippet
;; registration/unregistration.  The undo history is saved in
;; `yas/field-undo-original-history' before each command and rewritten
;; if appropriate at the end.
;;
;; This is done by registering `yas/field-undo-before-hook' and
;; `yas/field-undo-after-hook' in the `pre-command-hook' and
;; `post-command-hook', respectively.
;;
;; Also, the `value' slot of the primary field of each group is used
;; to keep track of the most recently inserted text of that snippet
;; field.  This could be seen as a hack, but that slot wasn't being
;; used anyway and its new meaning is actually quite reasonable.
;;
;; Another detail is that undo informatino shoulnd't be recorded for
;; some commands, most notably `undo' itself.  Therefore, a variable
;; `yas/field-undo-forbidden-commands' has been introduced, to be
;; tested agains `this-command'.
;;

(defvar yas/field-undo-history nil
  "Saves the value of `buffer-undo-list' when undo information is
to be recorded by `yas/field-undo-after-hook'.  A new piece of undo
is pushed into this variable and it then replaces
`buffer-undo-list' if appropriate.")

(defvar yas/field-undo-forbidden-commands '(undo aquamacs-undo redo aquamacs-redo)
  "A list of commands executed while a snippet is active that
should not trigger any undo-recording action")

(defun yas/field-undo-before-hook ()
  "Saves the field-level undo history, `buffer-undo-list' into a global
`yas/field-undo-history' variable just before a command is
performed.  It will come in handy in case the command is to be undone"
  (setq yas/field-undo-history buffer-undo-list))

(defun yas/field-undo-after-hook ()
  "Compares the value (a string) of the currently active snippet
group with a previously saved one.  If these are different, undo
information is added to `buffer-undo-list'

This function is added to the `post-command-hook' and should
be a part of that list while registered snippets last."
  (let* ((overlay (or (yas/current-snippet-overlay)
                      (yas/current-snippet-overlay (1- (point)))))
         (group (when overlay
                  (overlay-get overlay 'yas/group))))
    (when group
      (let ((new-text (yas/current-field-text (yas/group-primary-field group)))
            (old-text (yas/field-value (yas/group-primary-field group))))
        ;;
        ;; Unless extended undo forbids `this-command', or the old and
        ;; new field strings are the same, rewrite the undo history
        ;; with a call to `yas/field-undo-group-text-change'
        ;; instead of whatever was placed there by the currently
        ;; finishing `this-command' command. This call receives the id
        ;; of the currently active snippet, the group to be undone and
        ;; the old text.
        ;;
        (unless (or (memq this-command yas/field-undo-forbidden-commands)
                    (string= new-text
                             old-text))
          ;;
          ;; Push a separator onto the history list, if one wasn't
          ;; there first. Have no clue why sometimes one is and one
          ;; isn't.
          ;;
          (unless (null (car yas/field-undo-history))
            (push nil yas/field-undo-history))
          (push `(apply yas/field-undo-group-text-change
                        ,group
                        ,old-text)
                yas/field-undo-history)
          (setq buffer-undo-list yas/field-undo-history))
        ;;
        ;; Then, in any case, save the new text into the value slot of
        ;; the primary this is because some "forbidden" commands might
        ;; really have changed the field value, most notably `undo'
        ;; itself! This was a hard bug to track down!
        ;;
        (setf (yas/field-value (yas/group-primary-field group)) new-text)))))

(defun yas/field-undo-group-text-change (group old-text)
  "Undoes one step of field-level undo history, in the snippet
  field group GROUP, replacing its text with OLD-TEXT, but
  respecting any transforms."
  (yas/remove-recent-undo-from-history)
  (let ((inhibit-modification-hooks t)  ; otherwise an additional
                                        ; `yas/replace-fields-with-value'
                                        ; is called
        (buffer-undo-list t))
    (yas/replace-fields-with-value
     (yas/group-fields group)
     old-text)))

;; Debug functions.  Use (or change) at will whenever needed.

(defun yas/debug-some-vars ()
  (interactive)
  (with-output-to-temp-buffer "*YASnippet trace*"
    (princ "Interesting YASnippet vars: \n\n")
    (princ (format "Register hash-table: %s\n\n" (yas/get-registered-snippets)))
    (cond ((eq (hash-table-count (yas/get-registered-snippets)) 0)
           (princ "  No registered snippets\n"))
          (t
           (maphash #'(lambda (key snippet)
                        (princ (format "\t key %s for snippet %s with %s  groups\n"
                                       key
                                       (yas/snippet-id snippet)
                                       (length (yas/snippet-groups snippet))))
                        (dolist (group (yas/snippet-groups snippet))
                          (princ (format "\t   group with %s fields.  Primary field is value is \"%s\"\n"
                                         (length (yas/group-fields group))
                                         (yas/field-value (yas/group-primary-field group))))))
                    (yas/get-registered-snippets))))

    (princ (format "\nPost command hook: %s\n" post-command-hook))
    (princ (format "\nPre  command hook: %s\n" pre-command-hook))

    (princ (format "\nUndo is %s. Undolist has %s elements. First 10 elements follow:\n"
                   (if (eq buffer-undo-list t)
                       "DISABLED"
                     "ENABLED")
                   (length buffer-undo-list)))
    (let ((undo-list buffer-undo-list))
      (dotimes (i 10)
        (when undo-list
          (princ (format "%s:  %s\n" i (car undo-list)))
          (setq undo-list (cdr undo-list)))))))


(provide 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey patching for other functions that's causing
;; problems to yasnippet. For details on why I patch
;; those functions, refer to
;;   http://code.google.com/p/yasnippet/wiki/MonkeyPatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-neutralize-syntax-in-CPP
  (around yas-mp/c-neutralize-syntax-in-CPP activate)
  "Adviced `c-neutralize-syntax-in-CPP' to properly
handle the end-of-buffer error fired in it by calling
`forward-char' at the end of buffer."
  (condition-case err
      ad-do-it
    (error (message (error-message-string err)))))

;; disable c-electric-* serial command in YAS fields
(add-hook 'c-mode-common-hook
          '(lambda ()
	     (make-variable-buffer-local 'yas/keymap)
             (dolist (k '(":" ">" ";" "<" "{" "}"))
               (define-key yas/keymap
                 k 'self-insert-command))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contents of dropdown-list.el
;;
;; dropdown-list.el is used by yasnippet to select multiple
;; candidate snippets.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el --- Drop-down menu interface
;;
;; Filename: dropdown-list.el
;; Description: Drop-down menu interface
;; Author: Jaeyoun Chung [jay.chung@gmail.com]
;; Maintainer:
;; Copyright (C) 2008 Jaeyoun Chung
;; Created: Sun Mar 16 11:20:45 2008 (Pacific Daylight Time)
;; Version:
;; Last-Updated: Sun Mar 16 12:19:49 2008 (Pacific Daylight Time)
;;           By: dradams
;;     Update #: 43
;; URL: http://www.emacswiki.org/cgi-bin/wiki/dropdown-list.el
;; Keywords: convenience menu
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  According to Jaeyoun Chung, "overlay code stolen from company-mode.el."
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/03/16 dadams
;;     Clean-up - e.g. use char-to-string for control chars removed by email posting.
;;     Moved example usage code (define-key*, command-selector) inside the library.
;;     Require cl.el at byte-compile time.
;;     Added GPL statement.
;; 2008/01/06 Jaeyoun Chung
;;     Posted to gnu-emacs-sources@gnu.org at 9:10 p.m.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; decf, fourth, incf, loop, mapcar*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface dropdown-list-face
  '((t :inherit default :background "lightyellow" :foreground "black"))
  "*Bla." :group 'dropdown-list)

(defface dropdown-list-selection-face
  '((t :inherit dropdown-list-face :background "purple"))
  "*Bla." :group 'dropdown-list)

(defvar dropdown-list-overlays nil)

(defun dropdown-list-hide ()
  (while dropdown-list-overlays
    (delete-overlay (pop dropdown-list-overlays))))

(defun dropdown-list-put-overlay (beg end &optional prop value prop2 value2)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'window t)
    (when prop
      (overlay-put ov prop value)
      (when prop2 (overlay-put ov prop2 value2)))
    ov))

(defun dropdown-list-line (start replacement &optional no-insert)
  ;; start might be in the middle of a tab, which means we need to hide the
  ;; tab and add spaces
  (let ((end (+ start (length replacement)))
        beg-point end-point
        before-string after-string)
    (goto-char (point-at-eol))
    (if (< (current-column) start)
        (progn (setq before-string (make-string (- start (current-column)) ? ))
               (setq beg-point (point)))
      (goto-char (point-at-bol)) ;; Emacs bug, move-to-column is wrong otherwise
      (move-to-column start)
      (setq beg-point (point))
      (when (> (current-column) start)
        (goto-char (1- (point)))
        (setq beg-point (point))
        (setq before-string (make-string (- start (current-column)) ? ))))
    (move-to-column end)
    (setq end-point (point))
    (let ((end-offset (- (current-column) end)))
      (when (> end-offset 0) (setq after-string (make-string end-offset ?b))))
    (when no-insert
      ;; prevent inheriting of faces
      (setq before-string (when before-string (propertize before-string 'face 'default)))
      (setq after-string (when after-string (propertize after-string 'face 'default))))
    (let ((string (concat before-string replacement after-string)))
      (if no-insert
          string
        (push (dropdown-list-put-overlay beg-point end-point 'invisible t
                                         'after-string string)
              dropdown-list-overlays)))))

(defun dropdown-list-start-column (display-width)
  (let ((column (mod (current-column) (window-width)))
        (width (window-width)))
    (cond ((<= (+ column display-width) width) column)
          ((> column display-width) (- column display-width))
          ((>= width display-width) (- width display-width))
          (t nil))))

(defun dropdown-list-move-to-start-line (candidate-count)
  (decf candidate-count)
  (let ((above-line-count (save-excursion (- (vertical-motion (- candidate-count)))))
        (below-line-count (save-excursion (vertical-motion candidate-count))))
    (cond ((= below-line-count candidate-count)
           t)
          ((= above-line-count candidate-count)
           (vertical-motion (- candidate-count))
           t)
          ((>= (+ below-line-count above-line-count) candidate-count)
           (vertical-motion (- (- candidate-count below-line-count)))
           t)
          (t nil))))

(defun dropdown-list-at-point (candidates &optional selidx)
  (dropdown-list-hide)
  (let* ((lengths (mapcar #'length candidates))
         (max-length (apply #'max lengths))
         (start (dropdown-list-start-column (+ max-length 3)))
         (i -1)
         (candidates (mapcar* (lambda (candidate length)
                                (let ((diff (- max-length length)))
                                  (propertize
                                   (concat (if (> diff 0)
                                               (concat candidate (make-string diff ? ))
                                             (substring candidate 0 max-length))
                                           (format "%3d" (+ 2 i)))
                                   'face (if (eql (incf i) selidx)
                                             'dropdown-list-selection-face
                                           'dropdown-list-face))))
                              candidates
                              lengths)))
    (save-excursion
      (and start
           (dropdown-list-move-to-start-line (length candidates))
           (loop initially (vertical-motion 0)
                 for candidate in candidates
                 do (dropdown-list-line (+ (current-column) start) candidate)
                 while (/= (vertical-motion 1) 0)
                 finally return t)))))

(defun dropdown-list (candidates)
  (let ((selection)
        (temp-buffer))
    (save-window-excursion
      (unwind-protect
          (let ((candidate-count (length candidates))
                done key selidx)
            (while (not done)
              (unless (dropdown-list-at-point candidates selidx)
                (switch-to-buffer (setq temp-buffer (get-buffer-create "*selection*"))
                                  'norecord)
                (delete-other-windows)
                (delete-region (point-min) (point-max))
                (insert (make-string (length candidates) ?\n))
                (goto-char (point-min))
                (dropdown-list-at-point candidates selidx))
              (setq key (read-key-sequence ""))
              (cond ((and (stringp key)
                          (>= (aref key 0) ?1)
                          (<= (aref key 0) (+ ?0 (min 9 candidate-count))))
                     (setq selection (- (aref key 0) ?1)
                           done      t))
                    ((member key `(,(char-to-string ?\C-p) [up]))
                     (setq selidx (mod (+ candidate-count (1- (or selidx 0)))
                                       candidate-count)))
                    ((member key `(,(char-to-string ?\C-n) [down]))
                     (setq selidx (mod (1+ (or selidx -1)) candidate-count)))
                    ((member key `(,(char-to-string ?\f))))
                    ((member key `(,(char-to-string ?\r) [return]))
                     (setq selection selidx
                           done      t))
                    (t (setq done t)))))
        (dropdown-list-hide)
        (and temp-buffer (kill-buffer temp-buffer)))
      ;;     (when selection
      ;;       (message "your selection => %d: %s" selection (nth selection candidates))
      ;;       (sit-for 1))
      selection)))

(defun define-key* (keymap key command)
  "Add COMMAND to the multiple-command binding of KEY in KEYMAP.
Use multiple times to bind different COMMANDs to the same KEY."
  (define-key keymap key (combine-command command (lookup-key keymap key))))

(defun combine-command (command defs)
  "$$$$$ FIXME - no doc string"
  (cond ((null defs) command)
        ((and (listp defs)
              (eq 'lambda (car defs))
              (= (length defs) 4)
              (listp (fourth defs))
              (eq 'command-selector (car (fourth defs))))
         (unless (member `',command (cdr (fourth defs)))
           (setcdr (fourth defs) (nconc (cdr (fourth defs)) `(',command))))
         defs)
        (t
         `(lambda () (interactive) (command-selector ',defs ',command)))))

(defvar command-selector-last-command nil "$$$$$ FIXME - no doc string")

(defun command-selector (&rest candidates)
  "$$$$$ FIXME - no doc string"
  (if (and (eq last-command this-command) command-selector-last-command)
      (call-interactively command-selector-last-command)
    (let* ((candidate-strings
            (mapcar (lambda (candidate)
                      (format "%s" (if (symbolp candidate)
                                       candidate
                                     (let ((s (format "%s" candidate)))
                                       (if (>= (length s) 7)
                                           (concat (substring s 0 7) "...")
                                         s)))))
                    candidates))
           (selection (dropdown-list candidate-strings)))
      (when selection
        (let ((cmd (nth selection candidates)))
          (call-interactively cmd)
          (setq command-selector-last-command cmd))))))

;;;;;;;;;;;;;;;;;;;;

(provide 'dropdown-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dropdown-list.el ends here

;;; yasnippet.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/initialize-bundle ()
  "Initialize YASnippet and load snippets in the bundle."  (yas/initialize)
;;; snippets for text-mode
(yas/define-snippets 'text-mode
'(
  ("time" "`(current-time-string)`" "(current time)" nil nil)
  ("email" "`user-mail-address`" "(user's email)" nil nil)
  )
nil)

;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
'(
  ("struct" "struct ${1:name}
{
    $0
};" "struct ... { ... }" nil nil)
  ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil)
  ("main" "int main(int argc, char *argv[])
{
    $0
    return 0;
}
" "int main(argc, argv) { ... }" nil nil)
  ("inc.1" "#include <$1>
" "#include <...>" nil nil)
  ("inc" "#include \"$1\"
" "#include \"...\"" nil nil)
  ("if" "if (${1:condition})
{
    $0
}" "if (...) { ... }" nil nil)
  ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }" nil nil)
  ("do" "do
{
    $0
} while (${1:condition});" "do { ... } while (...)" nil nil)
  )
'text-mode)

;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
'(
  ("using" "using namespace ${std};
$0" "using namespace ... " nil nil)
  ("template" "template <typename ${T}>" "template <typename ...>" nil nil)
  ("ns" "namespace " "namespace ..." nil nil)
  ("class" "class ${1:Name}
{
public:
    ${1:$(yas/substr text \"[^: ]*\")}($2);
    virtual ~${1:$(yas/substr text \"[^: ]*\")}();
};" "class ... { ... }" nil nil)
  ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil)
  )
'cc-mode)

;;; snippets for c-mode
(yas/define-snippets 'c-mode
'(
  ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");
" "FILE *fp = fopen(..., ...);" nil nil)
  )
'cc-mode)

;;; snippets for csharp-mode
(yas/define-snippets 'csharp-mode
'(
  ("using.2" "using System.$1;
" "using System....;" nil nil)
  ("using.1" "using System;
" "using System;" nil nil)
  ("using" "using $1;
" "using ...;" nil nil)
  ("region" "#region $1
$0
#endregion
" "#region ... #endregion" nil nil)
  ("prop" "/// <summary>
/// $5
/// </summary>
/// <value>$6</value>
$1 $2 $3
{
    get {
        return this.$4;
    }
    set {
        this.$4 = value;
    }
}
" "property ... ... { ... }" nil nil)
  ("namespace" "namespace $1
{
$0
}
" "namespace .. { ... }" nil nil)
  ("method" "/// <summary>
/// ${5:Description}
/// </summary>${2:$(if (string= (upcase text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" text \"</c></returns>\"))}
${1:public} ${2:void} ${3:MethodName}($4)
{
$0
}
" "public void Method { ... }" nil nil)
  ("comment.3" "/// <exception cref=\"$1\">$2</exception>
" "/// <exception cref=\"...\"> ... </exception>" nil nil)
  ("comment.2" "/// <returns>$1</returns>
" "/// <param name=\"...\"> ... </param>" nil nil)
  ("comment.1" "/// <param name=\"$1\">$2</param>
" "/// <param name=\"...\"> ... </param>" nil nil)
  ("comment" "/// <summary>
/// $1
/// </summary>
" "/// <summary> ... </summary>" nil nil)
  ("class" "${5:public} class ${1:Name}
{
    #region Ctor & Destructor
    /// <summary>
    /// ${3:Standard Constructor}
    /// </summary>
    public $1($2)
    {
    }

    /// <summary>
    /// ${4:Default Destructor}
    /// </summary>    
    public ~$1()
    {
    }
    #endregion
}
" "class ... { ... }" nil nil)
  ("attrib.2" "/// <summary>
/// $3
/// </summary>
private $1 ${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};

/// <summary>
/// ${3:Description}
/// </summary>
/// <value><c>$1</c></value>
public ${1:Type} ${2:Name}
{
    get {
        return this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")};
    }
    set {
        this.${2:$(if (> (length text) 0) (format \"_%s%s\" (downcase (substring text 0 1)) (substring text 1 (length text))) \"\")} = value;
    }
}
" "private _attribute ....; public Property ... ... { ... }" nil nil)
  ("attrib.1" "/// <summary>
/// $3
/// </summary>
private $1 $2;

/// <summary>
/// $4
/// </summary>
/// <value>$5</value>
public $1 $2
{
    get {
        return this.$2;
    }
    set {
        this.$2 = value;
    }
}
" "private attribute ....; public property ... ... { ... }" nil nil)
  ("attrib" "/// <summary>
/// $3
/// </summary>
private $1 $2;
" "private attribute ....;" nil nil)
  )
'cc-mode)

;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
'(
  ("prop" "- (${1:id})${2:foo}
{
    return $2;
}

- (void)set${2:$(capitalize text)}:($1)aValue
{
    [$2 autorelease];
    $2 = [aValue retain];
}
$0" "foo { ... } ; setFoo { ... }" nil nil)
  )
'cc-mode)

;;; snippets for css-mode
(yas/define-snippets 'css-mode
'(
  ("pad.top" "padding-top: $1;
" "padding-top: ..." nil nil)
  ("pad.right" "padding-right: $1;
" "padding-right: ..." nil nil)
  ("pad.padding" "padding: ${top} ${right} ${bottom} ${left};
" "padding: top right bottom left" nil nil)
  ("pad.pad" "padding: $1;
" "padding: ..." nil nil)
  ("pad.left" "padding-left: $1;
" "padding-left: ..." nil nil)
  ("pad.bottom" "padding-bottom: $1;
" "padding-bottom: ..." nil nil)
  ("mar.top" "margin-top: $1;
" "margin-top: ..." nil nil)
  ("mar.right" "margin-right: $1;
" "margin-right: ..." nil nil)
  ("mar.margin" "margin: ${top} ${right} ${bottom} ${left};
" "margin top right bottom left" nil nil)
  ("mar.mar" "margin: $1;
" "margin: ..." nil nil)
  ("mar.left" "margin-left: $1;
" "margin-left: ..." nil nil)
  ("mar.bottom" "margin-bottom: $1;
" "margin-bottom: ..." nil nil)
  ("fs" "font-size: ${12px};
" "font-size: ..." nil nil)
  ("ff" "font-family: $1;
" "font-family: ..." nil nil)
  ("disp.none" "dislpay: none;
" "display: none" nil nil)
  ("disp.inline" "dislpay: inline;
" "display: inline" nil nil)
  ("disp.block" "dislpay: block;
" "display: block" nil nil)
  ("cl" "clear: $1;
" "clear: ..." nil nil)
  ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil)
  ("bg.1" "background-image: url($1);" "background-image: ..." nil nil)
  ("bg" "background-color: #${1:DDD};" "background-color: ..." nil nil)
  )
'text-mode)

;;; snippets for erlang-mode
(yas/define-snippets 'erlang-mode
'(
  ("undef" "-undef($1).
$0
" "-undef(...)." nil nil)
  ("try" "try $1 of
    $0
catch
after
end
" "try ... of ... catch after end" nil nil)
  ("rec" "-record($1,{$2}).
$0
" "-record(...,{...})." nil nil)
  ("rcv.after" "receive
after
    $1 -> $0
end
" "receive after ... -> ... end" nil nil)
  ("rcv" "receive
    $1 -> $0
end
" "receive ... -> ... end" nil nil)
  ("mod" "-module(${1:$(file-name-nondirectory 
               (file-name-sans-extension (buffer-file-name)))}).
$0

" "-module()." nil nil)
  ("loop" "${1:loop}($2) ->
    receive
	${3:_} ->
	    $1($2)
    end.
$0
" "loop(...) -> receive _ -> loop(...) end." nil nil)
  ("inc.lib" "-include_lib(\"$1\").
$0
" "-include_lib(\"...\")." nil nil)
  ("inc" "-include(\"$1\").
$0
" "-include(\"...\")." nil nil)
  ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).
$0
" "-import([])." nil nil)
  ("ifndef" "-ifndef($1).
$0
-endif.
" "-ifndef(...). ... -endif." nil nil)
  ("ifdef" "-ifdef($1).
$0
-endif.
" "-ifdef(...). ... -endif." nil nil)
  ("if" "if
    $1 -> $2;
    true -> $0
end
" "if ... -> ... ; true -> ... end" nil nil)
  ("fun" "fun ($1) -> $0 end
" "fun (...) -> ... end" nil nil)
  ("exp" "-export([${1:start/0}]).
$0
" "-export([])." nil nil)
  ("def" "-define($1,$2).
$0
" "-define(...,...)." nil nil)
  ("compile" "-compile([${1:export_all}]).
$0
" "-compile(...)." nil nil)
  ("case" "case $1 of
    $0
end
" "case ... of ... end" nil nil)
  ("beh" "-behaviour(${1:gen_server}).
$0
" "-behaviour(...)." nil nil)
  ("begin" "begin
    $0
end
" "begin ... end" nil nil)
  ("after" "after
    $1 -> $0
" "after ... ->" nil nil)
  )
'text-mode)

;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
'(
  ("wr" "write (${1:*},${2:*}) $0
" "write (*,*)" nil nil)
  ("su" "subroutine $0
" "subroutine" nil nil)
  ("st" "structure $0
" "structure" nil nil)
  ("re" "read (${1:*},${2:*}) $0
" "read (*,*)" nil nil)
  ("pr" "program ${1:name}
  $0
end program ${1:name}
" "program ... end program ..." nil nil)
  ("pa" "parameter $0
" "parameter" nil nil)
  ("l" "logical $0
" "logical" nil nil)
  ("ir" "implicit real $0
" "implicit real" nil nil)
  ("intr" "intrinsic $0
" "intrinsic" nil nil)
  ("inc" "include $0
" "include" nil nil)
  ("in" "implicit none
" "implicit none" nil nil)
  ("il" "implicit logical $0
" "implicit logical" nil nil)
  ("ii" "implicit integer $0
" "implicit integer " nil nil)
  ("if" "if ( ${1:condition} ) then
   $0
end if
" "if then end if" nil nil)
  ("ich" "implicit character $0
" "implicit character" nil nil)
  ("ic" "implicit complex $0
" "implicit complex" nil nil)
  ("ib" "implicit byte $0
" "implicit byte" nil nil)
  ("eq" "equivalence $0
" "equivalence" nil nil)
  ("dp" "double precision $0
" "double precision" nil nil)
  ("do" "do while (${1:condition})
   $0
end do
" "do while (...) end do" nil nil)
  ("dc" "double complex $0
" "double complex" nil nil)
  ("cx" "complex $0
" "complex" nil nil)
  ("ch" "character $0
" "character" nil nil)
  ("c" "continue $0
" "continue" nil nil)
  ("bd" "block data $0
" "block data" nil nil)
  ("au" "automatic $0 
" "automatic" nil nil)
  )
'text-mode)

;;; snippets for html-mode
(yas/define-snippets 'html-mode
'(
  ("ul.id" "<ul id=\"$1\">
  $0
</ul>" "<ul id=\"...\">...</ul>" nil "list")
  ("ul.class" "<ul class=\"$1\">
  $0
</ul>" "<ul class=\"...\">...</ul>" nil "list")
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil "list")
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil "table")
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil "table")
  ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil "table")
  ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">
  $0
</table>" "<table ...>...</table>" nil "table")
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil)
  ("span.class" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("script.javascript-src" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil)
  ("script.javascript" "<script type=\"text/javascript\">
  $0
</script>" "<script type=\"text/javascript\">...</script> " nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol.id" "<ol id=\"$1\">
  $0
</ol>" "<ol id=\"...\">...</ol>" nil "list")
  ("ol.class" "<ol class=\"$1\">
  $0
</ol>" "<ol class=\"...\">...</ol>" nil "list")
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil "list")
  ("meta.http-equiv" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil "meta")
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta")
  ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil)
  ("link.stylesheet-ie" "<!--[if IE]>
<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />
<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil)
  ("link.stylesheet" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li.class" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil "list")
  ("li" "<li>$1</li>" "<li>...</li>" nil "list")
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil)
  ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("html" "<html>
  $0
</html>
" "<html>...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header")
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header")
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header")
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header")
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header")
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header")
  ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">
  $0
</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil)
  ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil "list")
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta")
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta")
  ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta")
  ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil "meta")
  ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil "meta")
  ("dl.id" "<dl id=\"$1\">
    $0
</dl>" "<dl> ... </dl>" nil "list")
  ("dl" "<dl>
    $0
</dl>
" "<dl> ... </dl>" nil "list")
  ("div.id-class" "<div id=\"$1\" class=\"$2\">
  $0
</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil)
  ("div.id" "<div id=\"$1\">
  $0
</div>" "<div id=\"...\">...</div>" nil nil)
  ("div.class" "<div class=\"$1\">
  $0
</div>" "<div class=\"...\">...</div>" nil nil)
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil)
  ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil "list")
  ("code.class" "<code class=\"$1\">
  $0
</code>" "<code class=\"...\">...</code>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for latex-mode
(yas/define-snippets 'latex-mode
'(
  ("begin" "
\\begin{${1:environment}}
$0
\\end{$1}
" "\\begin{environment} ... \\end{environment}" nil nil)
  )
'text-mode)

;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
'(
  ("rlink" "[${1:Link Text}][$2] $0
" "Reference Link" nil nil)
  ("rlb" "[${1:Reference}]: ${2:URL} $3
$0
" "Reference Label" nil nil)
  ("rimg" "![${1:Alt Text}][$2] $0
" "Referenced Image" nil nil)
  ("ol" "${1:1}. ${2:Text}
${1:$(number-to-string (1+ (string-to-number text)))}. $0
" "Ordered List" nil nil)
  ("link" "[${1:Link Text}](${2:URL} $3) $0
" "Link" nil nil)
  ("img" "![${1:Alt Text}](${2:URL} $3) $0
" "Image" nil nil)
  ("hr.2" "
*******

$0
" "Horizontal Rule (*)" nil nil)
  ("hr.1" "
----------

$0
" "Horizontal Rule (-)" nil nil)
  ("h6" "###### ${1:Header 6} ######

$0
" "Header 6" nil nil)
  ("h5" "##### ${1:Header 5} #####

$0
" "Header 5" nil nil)
  ("h4" "#### ${1:Header 4} ####

$0
" "Header 4" nil nil)
  ("h3" "### ${1:Header 3} ###

$0
" "Header 3" nil nil)
  ("h2.2" "${1:Header 2}
${1:$(make-string (string-width text) ?\\-)}

$0
" "Header 2 (-)" nil nil)
  ("h2.1" "## ${1:Header 1} ##

$0
" "Header 2 (##)" nil nil)
  ("h1.2" "${1:Header 1}
${1:$(make-string (string-width text) ?\\=)}

$0
" "Header 1 (=)" nil nil)
  ("h1.1" "# ${1:Header 1} #

$0
" "Header 1 (#)" nil nil)
  ("`" "\\`${1:Code}\\` $0
" "Inline Code" nil nil)
  ("__" "**${1:Text}** $0
" "Strong" nil nil)
  ("_" "_${1:Text}_ $0
" "Emphasis" nil nil)
  ("-" "- ${1:Text}
-$0
" "Unordered List" nil nil)
  ("+" "+ ${1:Text}
+$0
" "Unordered List" nil nil)
  )
'text-mode)

;;; snippets for nxml-mode
(yas/define-snippets 'nxml-mode
'(
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil nil)
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil nil)
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil nil)
  ("tag.2l" "<${1:tag}>
  $2
</$1>$0" "<tag> \\n...\\n</tag>" nil nil)
  ("tag.1l" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil)
  ("table" "<table>
  $0
</table>" "<table>...</table>" nil nil)
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil nil)
  ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil)
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil "meta")
  ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li" "<li>$1</li>" "<li>...</li>" nil nil)
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil)
  ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil "header")
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil "header")
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil "header")
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil "header")
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil "header")
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil "header")
  ("form" "<form method=\"$1\" action=\"$2\">
  $0
</form>" "<form method=\"...\" action=\"...\"></form>" nil nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil "meta")
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil "meta")
  ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil "meta")
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
'(
  ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil)
  ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil)
  ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil)
  ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil)
  ("while" "while ($1) {
    $0
}" "while (...) { ... }" nil nil)
  ("unless" "unless ($1) {
    $0
}" "unless (...) { ... }" nil nil)
  ("sub" "sub ${1:function_name} {
    $0
}" "sub ... { ... }" nil nil)
  ("ifee" "if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}" "if, elsif, else ..." nil nil)
  ("ife" "if ($1) {
    $2
} else {
    $3
}" "if (...) { ... } else { ... }" nil nil)
  ("if" "if ($1) {
    $0
}" "if (...) { ... }" nil nil)
  ("fore" "foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}" "foreach ... { ... }" nil nil)
  ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}" "for (...) { ... }" nil nil)
  ("eval" "eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}" "eval { ... } if ($@) { ... }" nil nil)
  )
'text-mode)

;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode
'(
  )
'perl-mode)

;;; snippets for python-mode
(yas/define-snippets 'python-mode
'(
  ("while" "while ${condition}:
    $0" "while ... : ..." nil nil)
  ("propsg" "def _set_${1:foo}(self, value):
    self._$1 = value

def _get_$1(self):
    return self._$1

$1 = property(_get_$1, _set_$1)

$0
" "_get_foo ... _set_foo ... foo=property(...)" nil nil)
  ("propg" "def _get_${1:foo}(self):
    return self._$1

$1 = property(_get_$1)

$0
" "_get_foo ... foo=property(...)" nil nil)
  ("ifmain" "if __name__ == '__main__':
    $0" "if __name__ == '__main__': ..." nil nil)
  ("for" "for ${var} in ${collection}:
    $0" "for ... in ... : ..." nil nil)
  ("defm" "def ${1:name}(self, $2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" nil nil nil)
  ("def" "def ${1:name}($2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" nil nil nil)
  ("class" "class ${1:ClassName}(${2:object}):
    \"\"\"$3
    \"\"\"

    def __init__(self, $4):
        \"\"\"$5
        ${4:$
        (let* ((indent
                (concat \"\\n\" (make-string (current-column) 32)))
               (args
                (mapconcat
                 '(lambda (x)
                    (if (not (string= (nth 0 x) \"\"))
                        (concat \"- \" (char-to-string 96) (nth 0 x)
                                (char-to-string 96) \":\")))
                 (mapcar
                  '(lambda (x)
                     (mapcar
                      (lambda (x)
                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))
                  (mapcar '(lambda (x) (split-string x \"=\"))
                          (split-string text \",\")))
                 indent)))
          (if (string= args \"\")
              (make-string 3 34)
            (mapconcat
             'identity
             (list \"\" \"Arguments:\" args (make-string 3 34))
             indent)))
        }
        ${4:$
        (let* ((indent (concat \"\\n\" (make-string (current-column) 32)))
              (self-vars (mapconcat
                   '(lambda (x)
                      (if (not (string= (nth 0 x) \"\"))
                          (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))
                   (mapcar
                    '(lambda (x)
                       (mapcar
                        '(lambda (x)
                           (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                                                     (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                        x))
                    (mapcar '(lambda (x) (split-string x \"=\"))
                            (split-string text \",\")))
                   (concat indent))))
  (if (string= self-vars \"\")
      indent
    self-vars))        
        }
        $0
" nil nil nil)
  ("__" "__${init}__" "__...__" nil nil)
  )
'text-mode)

;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
'(
  ("tit" "${1:$(make-string (string-width text) ?\\=)}
${1:Title}
${1:$(make-string (string-width text) ?\\=)}

$0" "Document title" nil nil)
  ("sec" "${1:Section}
${1:$(make-string (string-width text) ?\\-)}

$0" "Section title" nil nil)
  ("chap" "${1:Chapter}
${1:$(make-string (string-width text) ?\\=)}

$0" "Chapter title" nil nil)
  )
'text-mode)

;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
'(
  ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil "collections")
  ("y" ":yields: $0" ":yields: arguments (rdoc)" nil "general")
  ("while" "while ${condition}
  $0
end" "while ... end" nil "control structure")
  ("when" "when ${condition}
  $0
end" "when ... end" nil "control structure")
  ("w" "attr_writer :" "attr_writer ..." nil "definitions")
  ("upt" "upto(${n}) { |${i}|
  $0
}" "upto(...) { |n| ... }" nil "control structure")
  ("until" "until ${condition}
  $0
end" "until ... end" nil "control structure")
  ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil "control structure")
  ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil "collections")
  ("rw" "attr_accessor :" "attr_accessor ..." nil "definitions")
  ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil "general")
  ("req" "require \"$0\"" "require \"...\"" nil "general")
  ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil "collections")
  ("rb" "#!/usr/bin/ruby -wKU
" "/usr/bin/ruby -wKU" nil "general")
  ("r" "attr_reader :" "attr_reader ..." nil "definitions")
  ("mm" "def method_missing(method, *args)
  $0
end" "def method_missing ... end" nil "definitions")
  ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil "collections")
  ("ife" "if ${1:condition}
  $2
else
  $3
end" "if ... else ... end" nil "control structure")
  ("if" "if ${1:condition}
  $0
end" "if ... end" nil "control structure")
  ("forin" "for ${1:element} in ${2:collection}
  $0
end" "for ... in ...; ... end" nil "control structure")
  ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil "collections")
  ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil "collections")
  ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil "collections")
  ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil "collections")
  ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil "collections")
  ("dow" "downto(${0}) { |${n}|
  $0
}" "downto(...) { |n| ... }" nil "control structure")
  ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil "collections")
  ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil "collections")
  ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil "general")
  ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil "collections")
  ("cls" "class ${1:$
         (let ((fn (capitalize (file-name-nondirectory
                                 (file-name-sans-extension
                                   (buffer-file-name))))))
           (cond
             ((string-match \"_\" fn) (replace-match \"\" nil nil fn))
              (t fn)))}
  $0
end
" "class ... end" nil "definitions")
  ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil "collections")
  ("cla" "class << ${self}
  $0
end" "class << self ... end" nil "definitions")
  ("case" "case ${1:object}
when ${2:condition}
  $0
end" "case ... end" nil "general")
  ("bm" "Benchmark.bmbm(${1:10}) do |x|
  $0
end" "Benchmark.bmbm(...) do ... end" nil "general")
  ("app" "if __FILE__ == $PROGRAM_NAME
  $0
end" "if __FILE__ == $PROGRAM_NAME ... end" nil "general")
  ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil "collections")
  ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil "definitions")
  ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil "collections")
  ("Comp" "include Comparable

def <=> other
  $0
end" "include Comparable; def <=> ... end" nil "definitions")
  ("=b" "=begin rdoc
  $0
=end" "=begin rdoc ... =end" nil "general")
  ("#" "# => " "# =>" nil "general")
  )
'text-mode)

;;; snippets for scala-mode
(yas/define-snippets 'scala-mode
'(
  ("with" "with $0" "with T" nil nil)
  ("whi" "while (${1:condition}) {
  $0
}" "while(cond) { .. }" nil nil)
  ("var.ret" "var ${1:name}: ${2:T} = ${3:obj} $0
" "var name: T = .." nil nil)
  ("var.new" "var ${1:name} = new ${2:obj} $0
" "var name = new .." nil nil)
  ("var" "var ${1:name} = ${2:obj} $0
" "var name = .." nil nil)
  ("val.ret" "val ${1:name}: ${2:T} = ${3:obj} $0
" "val name: T = .." nil nil)
  ("val.new" "val ${1:name} = new ${2:obj} $0" "val name = new .." nil nil)
  ("val" "val ${1:name} = ${2:obj} $0" "val name = .." nil nil)
  ("tup.paren" "(${1:element1}, ${2:element2}) $0" "(element1, element2)" nil nil)
  ("tup.arrow" "${1:element1} -> ${2:element2} $0" "element1 -> element2" nil nil)
  ("try.finally" "try {

} finally {
  $0
}" "try { .. } finally { .. }" nil nil)
  ("try.catch-finally" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
} finally {

}" "try { .. } catch { case e => ..} finally { ..}" nil nil)
  ("try" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
}" "try { .. } catch { case e => ..}" nil nil)
  ("tr.with" "trait ${1:name} with ${2:trait} {
  $0
}" "trait T1 with T2 { .. }" nil nil)
  ("tr.ext-with" "trait ${1:name} extends ${2:class} with ${3:trait} {
  $0
}" "trait T1 extends C with T2 { .. }" nil nil)
  ("tr.ext" "trait ${1:name} extends ${2:class} {
  $0
}" "trait T extends C { .. }" nil nil)
  ("tr" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("throw" "throw new ${1:Exception}(${2:msg}) $0" "throw new Exception" nil nil)
  ("test" "//@Test
def test${1:name} = {
  $0
}" "@Test def testX = ..." nil nil)
  ("suite" "import org.scalatest._

class ${1:name} extends Suite {
  $0
}" "class T extends Suite { .. }" nil nil)
  ("pro.param" "protected[${1:this}] $0" "protected[this]" nil nil)
  ("pro" "protected $0" "protected" nil nil)
  ("pri.param" "private[${1:this}] $0" "private[this]" nil nil)
  ("pri" "private $0" "private" nil nil)
  ("pr.trace" "println(\"${1:obj}: \" + ${1:obj}) $0" "println(\"obj: \" + obj)" nil nil)
  ("pr.string" "println(\"${1:msg}\") $0" "println(\"..\")" nil nil)
  ("pr.simple" "print(${1:obj}) $0" "print(..)" nil nil)
  ("pr.newline" "println(${1:obj}) $0" "println(..)" nil nil)
  ("pac" "package $0" "package .." nil nil)
  ("ob" "object ${1:name} extends ${2:type} $0" "object name extends T" nil nil)
  ("mix" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("match.option" "${1:option} match {
  case None => $0
  case Some(res) => 

}" "option match { case None => .. }" nil nil)
  ("match.can" "${1:option} match {
  case Full(res) => $0

  case Empty => 

  case Failure(msg, _, _) => 

}" "can match { case Full(res) => .. }" nil nil)
  ("match" "${1:cc} match {
  case ${2:pattern} => $0
}" "cc match { .. }" nil nil)
  ("map.new" "Map(${1:key} -> ${2:value}) $0" "Map(key -> value)" nil nil)
  ("map" "map(${1:x} => ${2:body}) $0" "map(x => ..)" nil nil)
  ("main" "def main(args: Array[String]) = {
  $0
}" "def main(args: Array[String]) = { ... }" nil nil)
  ("ls.val-new" "val ${1:l} = List(${2:args}, ${3:args}) $0" "val l = List(..)" nil nil)
  ("ls.new" "List(${1:args}, ${2:args}) $0" "List(..)" nil nil)
  ("isof" "isInstanceOf[${1:type}] $0" "isInstanceOf[T] " nil nil)
  ("intercept" "intercept(classOf[${1:Exception]}) {
  $0
}" "intercept(classOf[T]) { ..}" nil nil)
  ("imp" "import $0" "import .." nil nil)
  ("if.else" "if (${1:condition}) {
  $2
} else {
  $0
}" "if (cond) { .. } else { .. }" nil nil)
  ("if" "if (${1:condition}) {
  $0
}" "if (cond) { .. }" nil nil)
  ("hset.val-new" "val ${1:m} = new HashSet[${2:key}] $0" "val m = new HashSet[K]" nil nil)
  ("hset.new" "new HashSet[${1:key}] $0
" "new HashSet[K]" nil nil)
  ("hmap.val-new" "val ${1:m} = new HashMap[${2:key}, ${3:value}] $0" "val m = new HashMap[K, V]" nil nil)
  ("hmap.new" "new HashMap[${1:key}, ${2:value}] $0" "new HashMap[K, V]" nil nil)
  ("foreach" "foreach(${1:x} => ${2:body}) $0" "foreach(x => ..)" nil nil)
  ("for.multi" "for {
  ${1:x} <- ${2:xs}
  ${3:x} <- ${4:xs}
} {
  yield $0
}" "for {x <- xs \\ y <- ys} { yield }" nil nil)
  ("for.loop" "for (${1:x} <- ${2:xs}) {
  $0
}" "for (x <- xs) { ... }" nil nil)
  ("for.if" "for (${1:x} <- ${2:xs} if ${3:guard}) {
  $0
}" "for (x <- xs if guard) { ... }" nil nil)
  ("for.extract" "${1:x} <- ${2:xs}" "x <- xs" nil nil)
  ("ext" "extends $0" "extends T" nil nil)
  ("expect" "expect(${1:reply}) {
  $0
}" "expect(value) { ..}" nil nil)
  ("doc.scaladoc" "/**
 * ${1:description}
 * $0
 */" "/** ... */" nil nil)
  ("doc.file-scala-api" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")`, LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala api file */" nil nil)
  ("doc.file-scala" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala $3                               **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")` , LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala file */" nil nil)
  ("doc.file" "/**
 * `(scala-mode-file-doc)`
 * $0
 * @author ${1:name}
 * @version ${2:0.1} 
 */" "/** file name */" nil nil)
  ("doc.def" "/** 
 * `(scala-mode-def-and-args-doc)`
 */ " "/** method name */" nil nil)
  ("doc.class" "/** 
 * `(scala-mode-find-clstrtobj-name-doc)`
 * ${1:description}
 * $0  
 */" "/** cls/trt/obj name */" nil nil)
  ("def.simple" "def ${1:name} = $0" "def f = ..." nil nil)
  ("def.ret-body" "def ${1:name}: ${3:Unit} = {
  $0
}" "def f: R = {...}" nil nil)
  ("def.ret" "def ${1:name}: ${2:Unit} = $0" "def f: R = ..." nil nil)
  ("def.body" "def ${1:name} = {
  $0
}" "def f = {...}" nil nil)
  ("def.arg-ret-body" "def ${1:name}(${2:args}): ${3:Unit} = {
  $0
}" "def f(arg: T): R = {...}" nil nil)
  ("def.arg-ret" "def ${1:name}(${2:args}): ${3:Unit} = $0" "def f(arg: T): R = ..." nil nil)
  ("def.arg-body" "def ${1:name}(${2:args}) = {
  $0
}" "def f(arg: T) = {...}" nil nil)
  ("def.arg" "def ${1:name}(${2:args}) = $0" "def f(arg: T) = ..." nil nil)
  ("cons.nil" "${1:element1} :: Nil $0
" "element1 :: Nil" nil nil)
  ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil)
  ("co" "case object ${1:name} $0" "case object T" nil nil)
  ("clof" "classOf[${1:type}] $0" "classOf[T] " nil nil)
  ("cl.arg" "class ${1:name}(${2:args}) {
  $0
}" "class T(args) { .. }" nil nil)
  ("cl.abs-arg" "abstract class ${1:name}(${2:args}) {
  $0
}" "abstract class T(args) { .. }" nil nil)
  ("cl.abs" "abstract class ${1:name} {
  $0
}" "abstract class T { .. }" nil nil)
  ("cl" "class ${1:name} {
  $0
}" "class T { .. }" nil nil)
  ("cc" "case class ${1:name}(${2:arg}: ${3:type}) $0" "case class T(arg: A)" nil nil)
  ("cast" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("case.match-all" "case _ => $0" "case _ => " nil nil)
  ("case" "case ${1:pattern} => $0" "case pattern => " nil nil)
  ("bang" "${1:actor} ! ${2:message} $0" "actor ! message" nil nil)
  ("at.version" "@version ${1:0.1} $0" "@version number" nil nil)
  ("at.return" "@return ${1:description} $0" "@return description" nil nil)
  ("at.param" "@param ${1:name} ${2:description} $0" "@param name description" nil nil)
  ("at.author" "@author ${1:name} $0" "@author name" nil nil)
  ("ass.true" "assert(true) $0" "assert(true)" nil nil)
  ("ass" "assert(${1:x} === ${2:y}) $0" "assert(x === y)" nil nil)
  ("asof" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("arr.val-new" "val ${1:arr} = Array[${2:value}](${3:args}) $0" "val a = Array[T](..)" nil nil)
  ("arr.new" "Array[${1:value}](${2:args}) $0" "Array[T](..)" nil nil)
  ("app" "object ${1:name} extends Application {
  $0
}" "object name extends Application" nil nil)
  ("ano" "($1) => ${2:body} $0" "(args) => ..." nil nil)
  ("actor" "val a = actor {
  loop {
    react {
      $0
    }
  }
}" "val a = actor { ..}" nil nil)
  ("act.arg" "def act(${1:arg}: ${2:type}) = {
  loop {
    react {
      $0
    }
  }
}" "def act(arg: T) = { ..}" nil nil)
  ("act" "def act = {
  loop {
    react {
      $0
    }
  }
}" "def act = { ..}" nil nil)
  )
'text-mode)

;;; snippets for sql-mode
(yas/define-snippets 'sql-mode
'(
  ("references" "REFERENCES ${1:TableName}([${2:ColumnName}])
" "REFERENCES ..." nil nil)
  ("create.1" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] 
(
		$3		$4		= ${5:NULL}		${6:OUTPUT}
)
AS
BEGIN
$0
END
GO
" "create procedure ..." nil nil)
  ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] 
(
		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}
$0
	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) 
)
GO
" "create table ..." nil nil)
  ("constraint.1" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] FOREIGN KEY ..." nil nil)
  ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] PRIMARY KEY ..." nil nil)
  ("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}
" ", ColumnName ColumnType NOT NULL..." nil nil)
  )
'text-mode)

)

(yas/initialize-bundle)
;;;###autoload(require 'yasnippet-bundle)
(provide 'yasnippet-bundle)
;;; yasnippet-bundle.el ends here
