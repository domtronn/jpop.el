;;; projectable.el --- Lightweight project cacheing and navigation framework -*- lexical-binding: t -*-

;; Copyright © 2015  Dom Charlesworth <dgc336@gmail.com>

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; URL: https://github.com/domtronn/projectable
;; Version: 1.0.0
;; Package-Version: 20150724-1604
;; Keywords: project, convenience
;; Package-Requires: ((emacs "24") (dash "2.11.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package creates an associative list of files
;; based on project keys for easier navigation.

;; Packages are defined by json files which can be loaded in.

;; This is perhaps not the best data structure for projects but
;; works well for require.js with name setups

;;; Code:

(require 'json)
(require 'ido)
(require 'dash)

(defconst projectable-dir (file-name-directory load-file-name))

;;; Group Definitions
(defgroup projectable nil
  "Manage how to read and create project caches."
  :prefix "projectable-"
  :group 'tools
  :group 'convenience)

(defcustom projectable-project-directory (expand-file-name (getenv "HOME"))
  "The directory where project json files are kept.

By default it looks in the the HOME folder as defined by `getenv`"
  :group 'projectable
  :type 'string)

(defcustom projectable-keymap-prefix (kbd "C-x p")
  "Projectable keymap prefix."
  :group 'projectable
  :type 'key-sequence)

(defcustom projectable-use-gitignore t
  "Whether to use gitignore for your regexp filters."
  :group 'projectable
  :type 'boolean)

(defcustom projectable-constrain-reformat t
  "Whether to constraint `projectable-reformat-file` to project files.

If nil, you can call `projectable-reformat-file` on any file which will
reformat that file to use the projects format settings."
  :group 'projectable
  :type 'boolean)

(defcustom projectable-completion-func 'cadr
    "The format of the file names when caling `completing-read`.

i.e.  Full     => /path/to/file.ext
      Basename => file.ext"
    :type '(radio
          (const :tag "Display the full file name" cadr)
          (const :tag "Display just the base name" car))
  :group 'projectable)

;;; Customisation Option Definitions
(defcustom projectable-alist-cmd (concat projectable-dir "create-file-alist.py")
  "Specify the command that to produce an associative list.

The SHELL-COMMAND, when run with a directory and a list of filter regexps,
should return an associative list in the following form as json for now.

\((file1 (dir1 dir2 dir3)) (file2 (dir1 dir2)))

By default, it uses the python script provided with this package."
  :group 'projectable
  :type 'string)

(defvar projectable-find-cmd-format
  "find %s -type f | grep -E \"%s\" | grep -vE \"%s\" | xargs %s"
  "Command format of find command used to pass to tags cmd.

The formats should be replaced, in order, by
- directory
- string of regexp language extensions e.g.  \\.cpp
  see `projectable-ctags-supported-languages`
  and `projectable-get-ctags-supported-languages`
- string of regexp filters
  and `projectable-get-filter-regexps`")

(defvar projectable-toggle-test-fallback-hook nil
  "A list of hooks to run if the `projectable-toggle-open-test` function fails.
If `projectable-toggle-open-test` does not guess a correct test file, it will
run this function, the idea being to prompt the user out of all the
tests.")

(defcustom projectable-auto-visit-tags t
  "Whether to visit the tags file upon creation of a tags file."
  :group 'projectable
  :type 'boolean)
(defcustom projectable-tags-file ".tags"
  "The name of the tags file to create."
  :group 'projectable
  :type 'string)
(defcustom projectable-ctags-cmd-format
  (format "ctags -f %s/%s -e" "%s" projectable-tags-file)
  "Specify the ctags command to pipe a list of files into.

-e is required to create an Emacs style tags file."
  :group 'projectable
  :type 'string)
(defcustom projectable-ctags-supported-languages
  '((asp . ("\\.asp$"))
    (c .  ("\\.c$"))
    (c++ . ("\\.cpp$" "\\.cc$" "\\.c\\+\\+"))
    (cs . ("\\.cs$"))
    (cobol . ("\\.cbl$" "\\.cob$" "\\.cpy$"))
    (erlang . ("\\.erl$" "\\.hrl$"))
    (fortran . ("\\.f$" "\\.for$" "\\.f90" "\\.f95"))
    (html . ("\\.html$" "\\.htm$"))
    (java . ("\\.java$" "\\.class$"))
    (javascript .  ("\\.js$"))
    (lisp . ("\\.lisp$" "\\.lsp$" "\\.l$" "\\.cl$"))
    (lua .  ("\\.lua$"))
    (matlab . ("\\.m$"))
    (perl . ("\\.pl$" "\\.pm$"))
    (php . ("\\.php$" "\\.php[3-5]$" "\\.phps$"))
    (python . ("\\.py$" "\\.pyc$" "\\.pyd$" "\\.pyo$" "\\.pyw$"))
    (ruby . ("\\.rb$"))
    (coffeescript . ("\\.coffee$")))
  "Select the languages you want to create ctags from."
  :group 'projectable
  :type '(alist :key-type symbol :value-type
                (repeat :tag "Regexp Extensions" regexp)))

(defcustom projectable-filter-tests t
  "Whether to filter out test files.

If t then the `projectable-file-alist` will not contain test files that
match `projectable-test-filter-regexp`, so that you only open directly
to source files.  If nil then `projectable-file-alist` will contain all
files."
  :group 'projectable
  :type 'booelan)

(defcustom projectable-test-filter-regexps
  (quote ("[-_]*[tT]est" "Spec" "\.spec" "\/script-tests\/.*"))
  "Specify a list of regexps to filter out test files.

This is a priority ordered list, so more likely matches should be first."
  :group 'projectable
  :type '(repeat regexp))

(defcustom projectable-filter-regexps
  (quote ("/node_modules/" "/tmp/"))
  "Specify a list of regexps to filter out extra files."
  :group 'projectable
  :type '(repeat string))

(defcustom projectable-use-caching t
  "Whether or not to locally cache the projects on `projectable-change`.

When this is non-nil, changing projects will cache the current
state of the current project, and when changing back to this
project, will restore the previous settings instantly rather than
perform OS walks.  When set to nil, it will not do this and cause
the file caches for the directories to be re generated every time
you change to it."
  :group 'projectable
  :type 'boolean)

(defcustom projectable-verbose nil
  "Toggle verbose printing.
Mainly for debugging of the package."
  :group 'projectable
  :type 'boolean)

;;; Variable Definitions
(defvar projectable-current-project-path nil)
(defvar projectable-project-alist nil)
(defvar projectable-project-hash nil)
(defvar projectable-file-alist nil)
(defvar projectable-all-alist nil)
(defvar projectable-test-alist nil)
(defvar projectable-id nil)
(defvar projectable-mode-line " [P>x]")

(defvar projectable-cache-alist nil)

(defvar projectable-indent-level
  2 "The level of indentation to be used.")
(defvar projectable-indent-object
  (list :tabs "	" "  ") "Definiton of indentation type with the indent character.")

;;; Function Definitions
(defun projectable-change (arg)
  "Change project path to ARG and refresh the cache."
  (interactive (list (ido-read-file-name "Enter path to Project file: "
                                         projectable-project-directory)))
  ;; Cache the old project
  (when (and projectable-id projectable-use-caching)
    (projectable-cache-current-project))

  ;; Set the current project path to new directory with removing trailing slash
  (setq projectable-current-project-path (replace-regexp-in-string "/$" "" arg))
  ;; Reset project specific variables
  (setq tags-table-list nil)
  (setq projectable-project-hash nil)
  (setq projectable-project-alist (make-hash-table :test 'equal))
  (setq projectable-file-alist (make-hash-table :test 'equal))
  (setq projectable-test-alist (make-hash-table :test 'equal))

  (if (and projectable-use-caching
           (assoc projectable-current-project-path projectable-cache-alist))
      (projectable-restore-cache projectable-current-project-path)
    (projectable-refresh)))

(defun projectable-restore-cache (cache-id)
  "Reset all of the projectable variables for CACHE-ID."
  (let ((project-cache (cdr (assoc cache-id projectable-cache-alist))))
    (setq projectable-current-project-path (cdr (assoc 'cpp project-cache)))
    (setq projectable-project-hash (cdr (assoc 'pph project-cache)))
    (setq projectable-project-alist (cdr (assoc 'ppa project-cache)))
    (setq projectable-id (cdr (assoc 'id project-cache)))
    (setq projectable-all-alist (cdr (assoc 'pa project-cache)))
    (setq projectable-file-alist (cdr (assoc 'pf project-cache)))
    (setq projectable-test-alist (cdr (assoc 'pt project-cache)))
    (projectable-message (format "Restored project from cache [%s]" projectable-current-project-path) t)))

(defun projectable-cache-current-project ()
  "Append or ammend a cache (for a session) for the current project."
  (let ((cache-id projectable-current-project-path)
        (cache-alist `((cpp . ,projectable-current-project-path)
                       (pph . ,projectable-project-hash)
                       (ppa . ,projectable-project-alist)
                       (id . ,projectable-id)
                       (pa . ,projectable-all-alist)
                       (pf . ,projectable-file-alist)
                       (pt . ,projectable-test-alist))))
    (when (assoc cache-id projectable-cache-alist)
      (setf (cdr (assoc cache-id projectable-cache-alist)) cache-alist))
    (add-to-list 'projectable-cache-alist (cons cache-id cache-alist))))

(defun projectable-refresh ()
  "Parse a json project file to create a cache for that project.

If the supplied file is not a file but a directory, it just adds
this directory to the file cache"
  (interactive)
  (when projectable-current-project-path
    (if (not (file-directory-p projectable-current-project-path))
        ;; Json file so load from json
        (progn (projectable-load-from-json)
               (projectable-message (format "New project is [%s]" projectable-current-project-path) t))
      ;; A directory so load form directory
      (progn
        (projectable-message
         (format "Interpreting as directory - [%s] is not a file" projectable-current-project-path) t)
        (projectable-load-from-path)))))

(defun projectable-load-from-json ()
  "Set the project based on a path.
This will just cache all of the files contained in that directory."
  (let* ((json-object-type 'hash-table)
         (json-hash (json-read-file projectable-current-project-path)))
    (setq projectable-project-hash json-hash)

    ;; Set project ID
    (let ((id (gethash "id" json-hash)))
      (setq projectable-id id)
      (setq projectable-mode-line (format " [P>%s]" id))
      (projectable-message (format "Project ID: [%s]" id)))

    ;; Create tags
    (projectable-create-tags (list (gethash "dirs" json-hash) (gethash "libs" json-hash)))

    (when (gethash "style" json-hash)
      (projectable-set-styling (gethash "style" json-hash)))

    (let* ((gitignore-from-hash (gethash "gitignore" json-hash))
           (use-gitignore (if gitignore-from-hash
                              (not (eq :json-false gitignore-from-hash))
                            projectable-use-gitignore)))

      (projectable-set-project-alist
       (when (and use-gitignore projectable-use-gitignore)
         (projectable-get-all-gitignore-filter (gethash "dirs" json-hash))))
      (projectable-set-test-alist
       (when (and use-gitignore projectable-use-gitignore)
         (projectable-get-all-gitignore-filter (gethash "dirs" json-hash))))))
t)

(defun projectable-get-all-gitignore-filter (project-list)
  "Get a distinct list of regexps to gitignore in the PROJECT-LIST files."
  (let ((gitignore-filter-regexp (list)))
    (mapc (lambda (x)
            (let ((location (locate-dominating-file (concat (gethash "dir" x) "/") ".gitignore")))
              (when location
                (setq gitignore-filter-regexp
                      (-distinct
                       (append gitignore-filter-regexp (projectable-get-gitignore-filter location)))))))
          project-list)
    gitignore-filter-regexp))

(defun projectable-create-tags (hash-list)
  "Create tags in the root projects based on a HASH-LIST of directories and flags."
  (mapc (lambda (hash)
          (mapc (lambda (elt)
                  (let* ((dir (concat (gethash "dir" elt) "/"))
                         (create-tags-p (not (eq :json-false (gethash "create-tags" elt)))))
                    (when create-tags-p
                      (projectable-message (format "Creating tags for [%s]" dir))
                      (projectable-create-tags-in-directory dir)
                      (when projectable-auto-visit-tags
                        (let ((tags-file (format "%s%s" (file-truename dir) projectable-tags-file)))
                          (when (not (member tags-file tags-table-list))
                            (setq tags-table-list (append tags-table-list (list tags-file)))))
                        )))
                  ) hash)) hash-list))

(defun projectable-create-tags-in-directory (dir)
  "Build and run the create tags command in DIR."
  (let* ((dir (shell-quote-argument (expand-file-name dir)))
         (cmd
          (format projectable-find-cmd-format
                  dir
                  (projectable-get-ctags-supported-languages)
                  (projectable-get-filter-regexps)
                  (format projectable-ctags-cmd-format dir)))
         (name (format "[projectable] Creating tags for [%s]" dir))
         (buffer-name (format "*create-tags*<%s>" dir)))
    (projectable-message cmd)
    (start-process-shell-command name buffer-name cmd)))

(defun projectable-set-styling (style-hash)
  "Set up variables associated with the styling from a STYLE-HASH."
  ;; Set the indent level
  (when (gethash "indent" style-hash)
    (setq projectable-indent-level (gethash "indent" style-hash)))
  ;; Set the tabs/spaces indent type
  (when (gethash "tabs" style-hash)
    (projectable-set-indent-object (eq :json-false (gethash "tabs" style-hash)))))

(defun projectable-load-from-path ()
  "Load a project from a given directory."
  ;; Set project ID
  (let ((id (file-name-nondirectory projectable-current-project-path)))
    (setq projectable-id id)
    (setq projectable-mode-line (format " [P>%s]" id))
    (projectable-message (format "Project ID: [%s]" id)))

  (let ((gitignore-filter-regexps (projectable-get-gitignore-filter
                                   (locate-dominating-file (concat projectable-current-project-path "/") ".gitignore"))))
    (projectable-set-project-alist (and projectable-use-gitignore gitignore-filter-regexps))
    (projectable-set-test-alist (and projectable-use-gitignore gitignore-filter-regexps)))
  t)

(defun projectable-set-test-alist (&optional gitignore-filter-regexps)
    "Set `projectable-test-alist` by using `projectable-alist-cmd`.

Can be passed a list GITIGNORE-FILTER-REGEXPS of regexps to append to the filter
string."
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
         (cmd (format "%s -i \"%s\" %s \"%s\""
                (shell-quote-argument projectable-alist-cmd)
                (mapconcat 'identity (mapcar (lambda (r) (concat r "\(\\.[a-z]+$\)")) projectable-test-filter-regexps) ",")
                (shell-quote-argument (expand-file-name projectable-current-project-path))
                (mapconcat 'identity (append (split-string (projectable-get-filter-regexps) "|") gitignore-filter-regexps) ",")))
         (result (json-read-from-string (shell-command-to-string cmd))))
            (projectable-message cmd)
            (setq projectable-test-alist (-reduce (lambda (a b) (append (cdr a) (cdr b))) result))
    t))

(defun projectable-set-project-alist (&optional gitignore-filter-regexps)
  "Set `projectable-project-alist` by usings `projectable-alist-cmd`.

Can be passed a list GITIGNORE-FILTER-REGEXPS to append to
the filter string set in the customisations."
  (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
         (cmd (format "%s %s \"%s\""
               (shell-quote-argument projectable-alist-cmd)
               (shell-quote-argument (expand-file-name projectable-current-project-path))
               (mapconcat 'identity (append
                                     (split-string (projectable-get-filter-regexps) "|")
                                     gitignore-filter-regexps
                                     (and projectable-filter-tests
                                          (mapcar (lambda (r) (concat r "\(\\.[a-z]+$\)")) projectable-test-filter-regexps)))
                          ",")))
         (result (json-read-from-string (shell-command-to-string cmd))))
    (projectable-message cmd)
    (setq projectable-project-alist result)
    (setq projectable-file-alist (cdr (assoc projectable-id result)))
    (setq projectable-all-alist (-reduce (lambda (a b) (append (cdr a) (cdr b))) result))
    t))

(defun projectable-get-gitignore-filter (gitignore-dir)
  "Produce regexps filters by based on a .gitignore files found in GITIGNORE-DIR."
  (when gitignore-dir
    (with-temp-buffer
     (insert-file-contents (concat gitignore-dir ".gitignore"))
     (goto-char (point-min))
     (flush-lines "^[#]")
     (flush-lines "^$")
     (while (search-forward "*" nil t) (replace-match ""))
     (goto-char (point-min))
     (mapcar 'regexp-quote (split-string (buffer-string) "\n" t)))))

(defun projectable-set-indent-object (use-spaces)
  "Set the indent type on whether we USE-SPACES.
t => spaces nil => tabs"
  (if use-spaces
      (progn
        (projectable-message (format "Using spaces for project [%s]" projectable-id))
        (setq projectable-indent-object (list :spaces (projectable-build-space-string) "	")))
    (progn
      (projectable-message (format "Using tabs for project [%s]" projectable-id))
      (setq projectable-indent-object (list :tabs "	" (projectable-build-space-string)))))
  t)

(defun projectable-stylise (indent &optional use-spaces)
  "Allow use to interactively set styling easily.

Sets the indent level to INDENT and if USE-SPACES is provided,
will use tabs vs spaces.  Otherwise they will be prompted."
  (interactive "sIndent Level : ")
  (let ((use-spaces (or use-spaces (y-or-n-p "Use spaces (n for Tabs)? "))))
    (setq projectable-indent-level (string-to-number indent))
    (projectable-set-indent-object use-spaces)
    (projectable-set-local-styles)))

(defun projectable-set-local-styles ()
  "Set the indent level and indent type."
  (setq-local indent-tabs-mode (eq :tabs (car projectable-indent-object)))
  (setq-local c-basic-offset projectable-indent-level)
  (setq-local css-indent-offset projectable-indent-level)
  (setq-local js-indent-level projectable-indent-level)
  (setq-local basic-offset projectable-indent-level)
  (setq tab-width projectable-indent-level)
  (when (fboundp 'js2-mode) (setq-local js2-basic-offset projectable-indent-level))
  (when (fboundp 'web-mode)
    (setq-local web-mode-markup-indent-offset projectable-indent-level)
    (setq-local web-mode-css-indent-offset projectable-indent-level)
    (setq-local web-mode-code-indent-offset projectable-indent-level))
  (projectable-message (format "Setting indent level to %s" projectable-indent-level))
  t)
;; Set a hook to set up the local project styles for project buffers
(add-hook 'find-file-hook
          '(lambda () (when (projectable-project-contains (buffer-file-name))
                   (projectable-set-local-styles))))

;; Utility functions
(defun projectable-message (string &optional override)
  "Prints debug message STRING for the package.
If called with boolean OVERRIDE, this will override the verbose setting."
  (when (or projectable-verbose override)
    (message (format "[projectable] %s" string))))

;;; Utility Functions
;;  A bunch of functions to help with project navigation and set up.

(defun projectable-switch ()
  "Switch between cached projects quickly."
  (interactive)
  (if (and projectable-id projectable-use-caching)
    (projectable-cache-current-project))
  (if (> (length projectable-cache-alist) 0)
      (let* ((projects (-map
                        (lambda (elt) (cons (file-name-nondirectory (car elt)) (car elt)))
                        projectable-cache-alist))
             (project (completing-read "Switch to Cached Project: " projects)))
        (projectable-restore-cache (cdr (assoc project projects))))
    (call-interactively 'projectable-change)))

(defun projectable-switch-buffer (&optional f)
  "Using `completing-read`, interactively switch between project buffers.

Optionally called F as the function used to switch the buffer."
  (interactive)
  (let ((project-buffers (-map 'buffer-name (projectable-get-project-buffers))))
        (funcall (or f 'switch-to-buffer) (completing-read
                (format "[%s] Switch to buffer: " projectable-id)
                project-buffers))))

(defun projectable-switch-buffer-other-window ()
  "Using `completing-read`, interactively switch buffers in other window for project buffers."
  (interactive)
  (projectable-switch-buffer 'switch-to-buffer-other-window))


(defun projectable-find-file ()
  "Call `projectable--find-file` for FILE with `find-file` as function call."
  (interactive)
  (projectable--find-file projectable-file-alist 'find-file))

(defun projectable-find-test ()
  "Call `projectable--find-file` for TEST-ALIST with `find-file` as function call."
  (interactive)
  (projectable--find-file (--map (cons (file-name-nondirectory (cadr it)) (cdr it)) (cdr projectable-test-alist)) 'find-file))

(defun projectable-extended-find-file (file-alist-id)
  "Call `projectable--find-file` after prompting user to narrow down the alist using FILE-ALIST-ID."
  (interactive (list (completing-read "Library: " (mapcar 'car projectable-project-alist))))
  (projectable--find-file
     (cdr (assoc file-alist-id projectable-project-alist)) 'find-file))

(defun projectable-find-file-other-window ()
  "Call `projectable--find-file` for FILE with `find-file-other-window` as function call."
  (interactive)
  (projectable--find-file projectable-file-alist 'find-file-other-window))

(defun projectable-find-test-other-window ()
  "Call `projectable--find-file` for TEST-ALIST with `find-file-other-window` as function call."
  (interactive)
  (projectable--find-file (--map (cons (file-name-nondirectory (cadr it)) (cdr it)) (cdr projectable-test-alist)) 'find-file))

(defun projectable-extended-find-file-other-window (file-alist-id)
  "Call `projectable--find-file` after prompting user to narrow down the alist using FILE-ALIST-ID."
  (interactive (list (completing-read "Library: " (mapcar 'car projectable-project-alist))))
  (projectable--find-file
     (cdr (assoc file-alist-id projectable-project-alist)) 'find-file-other-window))

(defun projectable--find-file (file-alist find-f)
  "Interactively find a file in your project.

Select a file matched using `completing-read` against the contents
of FILE-ALIST.  Options are displayed using READ-F.  If the file exists
in more than one directory, select directory.  Lastly the file is opened using FIND-F."
  (let* ((file (completing-read "File: " (mapcar projectable-completion-func file-alist)))
         (record (assoc (file-name-nondirectory file) file-alist)))
    (funcall find-f
      (if (= (length record) 2)
          (cadr record)
        (completing-read
         (format "Find %s in dir:" file) (cdr record))))))

(defun projectable-toggle-open-test-other-window ()
  "Open associated test class if it exists in the other window."
  (interactive)
  (projectable-toggle-open-test 'find-file-other-window))

(defun projectable-toggle-open-test (&optional f)
  "Open associated test class if it exists using F."
  (interactive)
  (let* ((find-f (or f 'find-file))
         (file-name (buffer-file-name))
         (filters (mapcar (lambda (r) (concat r "\\(\\.[a-z]+$\\)")) projectable-test-filter-regexps))
         (test-p (car (-non-nil (mapcar (lambda (r) (when (string-match r file-name) r)) filters))))
         (neutral-file-name (replace-regexp-in-string (or test-p "") "\\1" (file-name-nondirectory file-name)))
         (result (assoc neutral-file-name (if test-p projectable-all-alist projectable-test-alist))))
    (cond
     ((= 2 (length result)) (funcall find-f (cadr result)))
     ((> (length result) 2) (funcall find-f
                                     (completing-read
                                      (format "[%s] Open test/src file: " projectable-id) (cdr result))))
     (t (if projectable-toggle-test-fallback-hook
            (run-hooks 'projectable-toggle-test-fallback-hook)
          (projectable-message (format "Could not find the test/src file for [%s]" file-name) t))))))

(defun projectable-reformat-file (&optional force)
  "Reformat tabs/spaces into correct format for current file.

If FORCE is non-nil then ignore the constrain effect."
  (interactive)
  (if (or (projectable-project-contains (buffer-file-name))
          (or force (not projectable-constrain-reformat)))
      (with-current-buffer (buffer-name)
        (save-excursion
          (goto-char (point-min))
          (while (search-forward (caddr projectable-indent-object) (point-max) t)
            (replace-match (cadr projectable-indent-object))))
        (indent-region (point-min) (point-max))
        (projectable-message
         (format "Reformatted file to use [%s]" (car projectable-indent-object)) t))
    (projectable-message
     (format "Reformat aborted - [%s] is not part of project" (file-name-nondirectory (buffer-file-name))) t)))

(defun projectable-build-space-string ()
  "Build the indent string of spaces.
i.e.  If indent level was 4, the indent string would be '    '."
  (make-string projectable-indent-level ? ))

(defun projectable-kill-project-buffers ()
  "Kill all open buffers in the current project."
  (interactive)
  (let* ((project-buffers (projectable-get-project-buffers)))
    (if project-buffers
        (let ((kill (yes-or-no-p
                     (format "[%s] Kill %d buffers (%s)? " projectable-id (length project-buffers) (mapconcat 'buffer-name project-buffers ", ")))))
          (when kill (mapc (lambda (buf) (kill-buffer buf)) project-buffers)))
      (projectable-message "You currently have no buffers open associated with this project" t))))

(defun projectable-get-project-buffers ()
  "Get a list of buffers within the current project."
  (-filter (lambda (buffer) (let* ((bufname (buffer-file-name buffer)))
                         (and bufname (projectable-project-contains bufname)))) (buffer-list)))

(defun projectable-project-contains (file)
  "Check whether FILE is contained within any of the projects directory paths."
  (-any? (lambda (r) (string-match (replace-regexp-in-string "~" "" r) file))
         (if projectable-project-hash
             (mapcar (lambda (elt) (gethash "dir" elt)) (gethash "dirs" projectable-project-hash))
           (and projectable-current-project-path (list projectable-current-project-path)))))

(defun projectable-visit-project-file ()
  "Open the project file currently being used."
  (interactive)
  (when projectable-current-project-path
    (if (not (file-directory-p projectable-current-project-path))
        (find-file projectable-current-project-path)
      (projectable-message
       (format "Current project is an anonymous path, not a project file [%s]" projectable-current-project-path) t))))

(defun projectable-get-ctags-supported-languages ()
  "Flatten and concatenate all supported languages for find command."
  (mapconcat 'format (-flatten (mapcar 'cdr projectable-ctags-supported-languages)) "|"))

(defun projectable-get-filter-regexps (&optional separator)
  "Flatten and concatenate all filter regexps for find command with SEPARATOR."
  (let ((local-completion-extensions (--mapcat (list (if (string-match "[a-z]$" it) (format "%s$" it) it)) completion-ignored-extensions)))
    (mapconcat 'regexp-quote (append local-completion-extensions projectable-filter-regexps) (or separator "|"))))

;;; Projectable Mode
;;  Set up for the projectable minor-mode.
(defvar projectable-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'projectable-change)
    (define-key map (kbd "C") 'projectable-switch)
    (define-key map (kbd "r") 'projectable-refresh)
    (define-key map (kbd "e") 'projectable-extended-find-file)
    (define-key map (kbd "E") 'projectable-extended-find-file-other-window)
    (define-key map (kbd "f") 'projectable-find-file)
    (define-key map (kbd "F") 'projectable-find-file-other-window)
    (define-key map (kbd "t") 'projectable-toggle-open-test)
    (define-key map (kbd "T") 'projectable-toggle-open-test-other-window)
    (define-key map (kbd "l") 'projectable-reformat-file)
    (define-key map (kbd "L") '(lambda (interactive) (projectable-reformat-file t)))
    (define-key map (kbd "p") 'projectable-visit-project-file)
    (define-key map (kbd "b") 'projectable-switch-buffer)
    (define-key map (kbd "B") 'projectable-switch-buffer-other-window)
    (define-key map (kbd "k") 'projectable-kill-project-buffers)
    (define-key map (kbd "s") 'projectable-stylise)
    map)
  "Keymap for Projectable commands after `projectable-keymap-prefix'.")
(fset 'projectable-command-map projectable-command-map)
(defvar projectable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectable-keymap-prefix 'projectable-command-map)
    map)
  "Keymap for Projectile mode.")

;;;###autoload
(define-minor-mode projectable-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `projectable-mode'.  With prefix
ARG, enable `projectable-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectable-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectable-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
  :lighter  projectable-mode-line
  :keymap projectable-mode-map
  :group 'projectable
  :require 'projectable)

;;;###autoload
(define-globalized-minor-mode projectable-global-mode
  projectable-mode
  projectable-mode)

(provide 'projectable)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; projectable.el ends here
