;;; jpop.el --- Lightweight project cacheing and navigation framework -*- lexical-binding: t -*-

;; Copyright © 2015  Dom Charlesworth <dgc336@gmail.com>

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; URL: https://github.com/domtronn/jpop
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

(defconst jpop-dir (unless (boundp 'jpop-dir) (file-name-directory load-file-name)))

;;; Group Definitions
(defgroup jpop nil
  "Manage how to read and create project caches."
  :prefix "jpop-"
  :group 'tools
  :group 'convenience)

(defcustom jpop-project-directory (expand-file-name (getenv "HOME"))
  "The directory where project json files are kept.

By default it looks in the the HOME folder as defined by `getenv`"
  :group 'jpop
  :type 'string)

(defcustom jpop-keymap-prefix (kbd "C-x p")
  "Jpop keymap prefix."
  :group 'jpop
  :type 'key-sequence)

(defcustom jpop-use-gitignore t
  "Whether to use gitignore for your regexp filters."
  :group 'jpop
  :type 'boolean)

(defcustom jpop-constrain-reformat t
  "Whether to constraint `jpop-reformat-file` to project files.

If nil, you can call `jpop-reformat-file` on any file which will
reformat that file to use the projects format settings."
  :group 'jpop
  :type 'boolean)

(defcustom jpop-completion-func 'cadr
    "The format of the file names when caling `completing-read`.

i.e.  Full     => /path/to/file.ext
      Basename => file.ext"
    :type '(radio
          (const :tag "Display the full file name" cadr)
          (const :tag "Display just the base name" car))
  :group 'jpop)

;;; Customisation Option Definitions
(defcustom jpop-alist-cmd (concat jpop-dir "create-file-alist.py")
  "Specify the command that to produce an associative list.

The SHELL-COMMAND, when run with a directory and a list of filter regexps,
should return an associative list in the following form as json for now.

\((file1 (dir1 dir2 dir3)) (file2 (dir1 dir2)))

By default, it uses the python script provided with this package."
  :group 'jpop
  :type 'string)

(defcustom jpop-offset-vars
  '(c-basic-offset css-indent-offset js2-basic-offset tab-width js-indent-level
    web-mode-css-indent-offset web-mode-code-indent-offset
    web-mode-markup-indent-offset)
	 "A list of variables that define the indent level of a project to be set locally."
	 :group 'jpop
	 :type 'list)

(defvar jpop-find-cmd-format
  "find %s -type f  -not -size +64k | grep -E \"%s\" | grep -vE \"%s\" | xargs %s"
  "Command format of find command used to pass to tags cmd.

The formats should be replaced, in order, by
- directory
- string of regexp language extensions e.g.  \\.cpp
  see `jpop-ctags-supported-languages`
  and `jpop-get-ctags-supported-languages`
- string of regexp filters
  and `jpop-get-filter-regexps`")

(defvar jpop-toggle-test-fallback-hook nil
  "A list of hooks to run if the `jpop-toggle-open-test` function fails.
If `jpop-toggle-open-test` does not guess a correct test file, it will
run this function, the idea being to prompt the user out of all the
tests.")

(defvar jpop-stylised nil "Whether or not the current file is stylised.
This variable allows you to stylise a buffer without
the `find-file` hook overriding the settings.")

(defcustom jpop-auto-visit-tags t
  "Whether to visit the tags file upon creation of a tags file."
  :group 'jpop
  :type 'boolean)
(defcustom jpop-tags-file ".tags"
  "The name of the tags file to create."
  :group 'jpop
  :type 'string)
(defcustom jpop-ctags-cmd-format
  (format "ctags -f %s/%s -e" "%s" jpop-tags-file)
  "Specify the ctags command to pipe a list of files into.

-e is required to create an Emacs style tags file."
  :group 'jpop
  :type 'string)
(defcustom jpop-ctags-supported-languages
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
  :group 'jpop
  :type '(alist :key-type symbol :value-type
                (repeat :tag "Regexp Extensions" regexp)))

(defcustom jpop-filter-tests t
  "Whether to filter out test files.

If t then the `jpop-file-alist` will not contain test files that
match `jpop-test-filter-regexp`, so that you only open directly
to source files.  If nil then `jpop-file-alist` will contain all
files."
  :group 'jpop
  :type 'booelan)

(defcustom jpop-test-filter-regexps
  (quote ("[-_]*[tT]est" "[-_]*[sS]pec" "Spec" "\.spec" "\/script-tests\/.*"))
  "Specify a list of regexps to filter out test files.

This is a priority ordered list, so more likely matches should be first."
  :group 'jpop
  :type '(repeat regexp))

(defcustom jpop-filter-regexps
  (quote ("/node_modules/" "/tmp/"))
  "Specify a list of regexps to filter out extra files."
  :group 'jpop
  :type '(repeat string))

(defcustom jpop-use-caching t
  "Whether or not to locally cache the projects on `jpop-change`.

When this is non-nil, changing projects will cache the current
state of the current project, and when changing back to this
project, will restore the previous settings instantly rather than
perform OS walks.  When set to nil, it will not do this and cause
the file caches for the directories to be re generated every time
you change to it."
  :group 'jpop
  :type 'boolean)

(defcustom jpop-verbose nil
  "Toggle verbose printing.
Mainly for debugging of the package."
  :group 'jpop
  :type 'boolean)

;;; Variable Definitions
(defvar jpop-current-project-path nil)
(defvar jpop-project-alist nil)
(defvar jpop-project-plist nil)
(defvar jpop-file-alist nil)
(defvar jpop-all-alist nil)
(defvar jpop-test-alist nil)
(defvar jpop-id nil)
(defvar jpop-mode-line " [P>x]")

(defvar jpop-cache-alist nil)

(defvar jpop-indent-level
  4 "The level of indentation to be used.")
(defvar jpop-indent-object
  (list :tabs "	" "  ") "Definiton of indentation type with the indent character.")

;;; Function Definitions

;;;###autoload
(defun jpop-change (arg)
  "Change project path to ARG and refresh the cache."
  (interactive (list (ido-read-file-name "Enter path to Project file: "
                                         jpop-project-directory)))

  ;; Set the current project path to new directory with removing trailing slash
  (setq jpop-current-project-path (replace-regexp-in-string "/$" "" arg))
  ;; Reset project specific variables
  (setq tags-table-list nil)
  (setq jpop-project-plist nil)
  (setq jpop-project-alist nil)
  (setq jpop-file-alist nil)
  (setq jpop-test-alist nil)

  (if (and jpop-use-caching
           (assoc jpop-current-project-path jpop-cache-alist))
      (jpop-restore-cache jpop-current-project-path)
    (jpop-refresh)
    (when (and jpop-id jpop-use-caching)
      (jpop-cache-current-project))))

;; Project auto changing

(defun jpop-invalidate-cache ()
  "Invalidate the entire project cache."
  (interactive)
  (setq jpop-cache-alist nil))

(defun jpop--cache-plist-contains (file cache)
  "Check whether FILE is contained within any of the projects directory paths of CACHE."
  (when file
    (let ((p-plist (cdr (assoc 'plist (cdr cache)))))
      (-any? (lambda (r) (string-match (expand-file-name r) file))
             (-concat (mapcar (lambda (elt) (plist-get elt :dir)) (plist-get p-plist :libs))
                      (mapcar (lambda (elt) (plist-get elt :dir)) (plist-get p-plist :dirs)))))))

(defun jpop--cache-contains (file cache)
  "Check whether FILE is contained within CACHE.

If this a json/plist cache, then it will call
  `jpop--cache-plist-contains`, otherwise, revert to
  checking whether file is contained within a path."
  (let ((plist (cdr (assoc 'plist cache)))
        (path (cdr (assoc 'path cache))))
    (when file
      (if plist (jpop--cache-plist-contains file cache)
        (string-match (expand-file-name path) file)))))

(defun jpop-change-to-project-intelligently ()
  "Hook function to restor project intelligently based on the file you've opened."
  (if (and (bound-and-true-p jpop-use-caching)
           (bound-and-true-p jpop-cache-alist))

      (let* ((file-name (file-truename (buffer-file-name)))
             (project-contains-file (apply-partially 'jpop--cache-plist-contains file-name))
             (project-caches  (-non-nil (mapcar #'(lambda (cache) (when (cdr (assoc 'plist (cdr cache))) cache))
                                 jpop-cache-alist)))
             (project-cache-ids (-map 'car project-caches))
             (containing-file (-non-nil (-zip-with (lambda (a b) (and a b)) (mapcar project-contains-file project-caches) project-cache-ids))))

        (when (and (not (eq nil containing-file))
                   (not (-contains? containing-file jpop-current-project-path)))
          (jpop-cache-current-project)
          (jpop-restore-cache (car containing-file))))

    (error "[jpop] You need to have caching enabled and have projects in your cache to use this feature")))

(defun jpop-enable-auto-change-project () "Add auto changing of projects." (interactive)
       (if jpop-use-caching
           (add-hook 'buffer-list-update-hook 'jpop--auto-change-project-hook)
         (error "[jpop] You need to have caching enabled to use this feature")))

(defun jpop-disable-auto-change-project () "Add auto changing of projects." (interactive)
       (if jpop-use-caching
           (remove-hook 'buffer-list-update-hook 'jpop--auto-change-project-hook)
         (error "[jpop] You need to have caching enabled to use this feature")))

(defvar jpop-last-buffer nil)
(defun jpop--auto-change-project-hook ()
  "Hook function to automatically change to the project."
  (when (and (buffer-file-name)                                                      ;; Buffer visiting a file
             (not (minibuffer-window-active-p (get-buffer-window (current-buffer)))) ;; Buffer not in the minibuffer
             (not (eq jpop-last-buffer (current-buffer))))                               ;; Buffer has changed
    (setq jpop-last-buffer (current-buffer))
    (jpop-change-to-project-intelligently)))

;;;###autoload
(defun jpop-change-and-find-file ()
  "Switch project before calling `jpop-find-file`."
  (interactive)
  (call-interactively 'jpop-change)
  (call-interactively 'jpop-find-file))

(defun jpop-restore-cache (cache-id)
  "Reset all of the jpop variables for CACHE-ID."
  (let ((project-cache (cdr (assoc cache-id jpop-cache-alist))))
    (setq jpop-current-project-path (cdr (assoc 'path project-cache)))
    (setq jpop-project-plist (cdr (assoc 'plist project-cache)))
    (setq jpop-project-alist (cdr (assoc 'alist project-cache)))
    (setq jpop-id (cdr (assoc 'id project-cache)))
    (setq jpop-all-alist (cdr (assoc 'all project-cache)))
    (setq jpop-file-alist (cdr (assoc 'files project-cache)))
    (setq jpop-test-alist (cdr (assoc 'tests project-cache)))

    (when (json-plist-p jpop-project-plist)
      (let* ((f (lambda (it) (when (plist-get it :tags)
                          (format "%s/%s" (plist-get it :dir) jpop-tags-file))))
             (tags (-concat (mapcar f (plist-get jpop-project-plist :dirs))
                            (mapcar f (plist-get jpop-project-plist :libs)))))
        (setq tags-table-list tags)))

    (jpop-message (format "Restored project from cache [%s]" jpop-current-project-path) t)))

(defun jpop-cache-current-project ()
  "Append or ammend a cache (for a session) for the current project."
  (let ((cache-id jpop-current-project-path)
        (cache-alist `((path  . ,jpop-current-project-path)  ;; Current Project Path
                       (plist . ,jpop-project-plist)         ;; Project plist
                       (alist . ,jpop-project-alist)         ;; Project Alist
                       (id    . ,jpop-id)                    ;; Project ID
                       (all   . ,jpop-all-alist)             ;; All Files Alist
                       (files . ,jpop-file-alist)            ;; Main File Alist
                       (tests . ,jpop-test-alist))))         ;; Test File Alist

    (when (assoc cache-id jpop-cache-alist)
      (setf (cdr (assoc cache-id jpop-cache-alist)) cache-alist))
    (add-to-list 'jpop-cache-alist (cons cache-id cache-alist))))

(defun jpop-refresh ()
  "Parse a json project file to create a cache for that project.

If the supplied file is not a file but a directory, it just adds
this directory to the file cache"
  (interactive)
  (when jpop-current-project-path
    (if (not (file-directory-p jpop-current-project-path))
        ;; Json file so load from json
        (progn (jpop-load-from-json jpop-current-project-path)
               (jpop-message (format "New project is [%s]" jpop-current-project-path) t))
      ;; A directory so load form directory
      (progn
        (jpop-message
         (format "Interpreting as directory - [%s] is not a file" jpop-current-project-path) t)
        (jpop-load-from-path jpop-current-project-path)))))

(defun jpop--get-project-dir-configs ()
  "Return the flattened list of directories and libraries defined in a project json file."
  (-flatten-n 1 (list
                 (append (plist-get jpop-project-plist :dirs) nil)      ;; Convert vetor to list
                 (append (plist-get jpop-project-plist :libs) nil))))

(defun jpop-load-from-json (path)
  "Set the project by loading the json file on PATH.
This will just cache all of the files contained in that directory."
  (let* ((json-object-type 'plist)
         (json-plist (json-read-file path)))
    (setq jpop-project-plist json-plist)

    ;; Set project ID
    (let ((id (plist-get json-plist :id)))
      (setq jpop-id id)
      (setq jpop-mode-line (format " [P>%s]" id))
      (jpop-message (format "Project ID: [%s]" id)))

    ;; Create tags
    (jpop-create-tags (jpop--get-project-dir-configs))
    (when (plist-get json-plist :style)
      (jpop-set-styling (plist-get json-plist :style)))

    (let* ((gitignore-from-json (plist-get json-plist :gitignore))
           (use-gitignore (if gitignore-from-json
                              (not (eq :json-false gitignore-from-json))
                            jpop-use-gitignore)))

      (jpop-set-project-alist
       (when (and use-gitignore jpop-use-gitignore)
         (jpop-get-all-gitignore-filter (plist-get json-plist :dirs))))
      (jpop-set-test-alist
       (when (and use-gitignore jpop-use-gitignore)
         (jpop-get-all-gitignore-filter (plist-get json-plist :dirs))))))
t)

(defun jpop-get-all-gitignore-filter (project-list)
  "Get a distinct list of regexps to gitignore in the PROJECT-LIST files."
  (let ((gitignore-filter-regexp (list)))
    (mapc (lambda (x)
            (let ((location (locate-dominating-file (concat (plist-get x :dir) "/") ".gitignore")))
              (when location
                (setq gitignore-filter-regexp
                      (-distinct
                       (append gitignore-filter-regexp (jpop-get-gitignore-filter location)))))))
          project-list)
    gitignore-filter-regexp))

(defun jpop-create-tags (list)
  "Create tags in the root projects based on a LIST of directories and flags."
  (mapc (lambda (elt)
          (let* ((dir (concat (plist-get elt :dir) "/"))
                 (create-tags-p (not (eq :json-false (plist-get elt :tags)))))

            (when create-tags-p
              (jpop-message (format "Creating tags for [%s]" dir))
              (jpop-create-tags-in-directory dir)
              (when jpop-auto-visit-tags
                (let ((tags-file (format "%s%s" (file-truename dir) jpop-tags-file)))
                  (when (not (member tags-file tags-table-list))
                    (setq tags-table-list (append tags-table-list (list tags-file)))))))))

        list))

(defun jpop-create-tags-in-directory (dir)
  "Build and run the create tags command in DIR."
  (let* ((dir (shell-quote-argument (expand-file-name dir)))
         (cmd
          (format jpop-find-cmd-format
                  dir
                  (jpop-get-ctags-supported-languages)
                  (jpop-get-filter-regexps)
                  (format jpop-ctags-cmd-format dir)))
         (name (format "[jpop] Creating tags for [%s]" dir))
         (buffer-name (format "*create-tags*<%s>" dir)))
    (jpop-message cmd)
    (start-process-shell-command name buffer-name cmd)))

(defun jpop-set-styling (style-plist)
  "Set up variables associated with the styling from a STYLE-PLIST."
  ;; Set the indent level
  (when (plist-get style-plist :indent)
    (setq jpop-indent-level (plist-get style-plist :indent)))
  ;; Set the tabs/spaces indent type
  (when (plist-get style-plist :tabs)
    (jpop-set-indent-object (eq :json-false (plist-get style-plist :tabs)))))

(defun jpop-load-from-path (path)
  "Load a project from a given PATH directory."
  ;; Set project ID
  (let ((id (file-name-nondirectory path)))
    (setq jpop-id id)
    (setq jpop-mode-line (format " [P>%s]" id))
    (jpop-message (format "Project ID: [%s]" id)))

  (let ((gitignore-filter-regexps (jpop-get-gitignore-filter
                                   (locate-dominating-file (concat jpop-current-project-path "/") ".gitignore"))))
    (jpop-set-project-alist (and jpop-use-gitignore gitignore-filter-regexps))
    (jpop-set-test-alist (and jpop-use-gitignore gitignore-filter-regexps)))
  t)

(defun jpop-set-test-alist (&optional gitignore-filter-regexps)
    "Set `jpop-test-alist` by using `jpop-alist-cmd`.

Can be passed a list GITIGNORE-FILTER-REGEXPS of regexps to append to the filter
string."
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
           (cmd (format "%s -i \"%s\" %s \"%s\""
                  (shell-quote-argument jpop-alist-cmd)
                  (mapconcat 'identity (mapcar (lambda (r) (concat r "\(\\.[a-z]+$\)")) jpop-test-filter-regexps) ",")
                  (shell-quote-argument (expand-file-name jpop-current-project-path))
                  (mapconcat 'identity (append (split-string (jpop-get-filter-regexps) "|") gitignore-filter-regexps) ",")))
           (result (json-read-from-string (shell-command-to-string cmd))))
      (jpop-message (format "jpop-set-test-alist: %s" cmd))
      (setq jpop-test-alist (-filter 'consp (--reduce (append (cdr acc) (cdr it)) result)))
      t))

(defun jpop-set-project-alist (&optional gitignore-filter-regexps)
  "Set `jpop-project-alist` by usings `jpop-alist-cmd`.

Can be passed a list GITIGNORE-FILTER-REGEXPS to append to
the filter string set in the customisations."
  (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
         (cmd (format "%s %s \"%s\""
                (shell-quote-argument jpop-alist-cmd)
                (shell-quote-argument (expand-file-name jpop-current-project-path))
                (mapconcat 'identity (append
                                      (split-string (jpop-get-filter-regexps) "|")
                                      gitignore-filter-regexps
                                      (and jpop-filter-tests
                                           (mapcar (lambda (r) (concat r "\(\\.[a-z]+$\)")) jpop-test-filter-regexps)))
                           ",")))
         (result (json-read-from-string (shell-command-to-string cmd))))
    (jpop-message (format "jpop-set-project-alist: %s" cmd))
    (setq jpop-project-alist result)
    (setq jpop-file-alist (cdr (assoc jpop-id result)))
    (setq jpop-all-alist (-filter 'consp (--reduce (append (cdr acc) (cdr it)) result)))
    t))

(defun jpop-get-gitignore-filter (gitignore-dir)
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

(defun jpop-set-indent-object (use-spaces)
  "Set the indent type on whether we USE-SPACES.
t => spaces nil => tabs"
  (if use-spaces
      (progn
        (jpop-message (format "Using spaces for project [%s]" jpop-id))
        (setq jpop-indent-object (list :spaces (jpop-build-space-string) "	")))
    (progn
      (jpop-message (format "Using tabs for project [%s]" jpop-id))
      (setq jpop-indent-object (list :tabs "	" (jpop-build-space-string)))))
  t)

(defun jpop-stylise (indent &optional use-spaces)
  "Allow use to interactively set styling easily.

Sets the indent level to INDENT and if USE-SPACES is provided,
will use tabs vs spaces.  Otherwise they will be prompted."
  (interactive
   (list (read-string "sIndent Level : ")
         (y-or-n-p "Use spaces (n for Tabs)? ")))
  (let ((indent (if (stringp indent) (string-to-number indent) indent)))
    (setq-local jpop-stylised t)
    (jpop-set-local-styles indent use-spaces)))

(defun jpop-set-local-styles (indent-level use-spaces)
  "Set the INDENT-LEVEL and whether to USE-SPACES."
  (dolist (it jpop-offset-vars) (eval `(setq-local ,it ,indent-level)))
  (setq-local indent-tabs-mode (not use-spaces))
  (jpop-message (format "Setting indent level to %s" jpop-indent-level)))

;; Set a hook to set up the local project styles for project buffers
(add-hook 'find-file-hook
          '(lambda ()
             (unless jpop-stylised
               (when (jpop-project-contains (buffer-file-name))
                 (jpop-set-local-styles jpop-indent-level (eq (car jpop-indent-object) :spaces))))))

;; Utility functions
(defun jpop-message (string &optional override)
  "Prints debug message STRING for the package.
If called with boolean OVERRIDE, this will override the verbose setting."
  (when (or jpop-verbose override)
    (message (format "[jpop] %s" string))))

;;; Utility Functions
;;  A bunch of functions to help with project navigation and set up.

(defun jpop-switch ()
  "Switch between cached projects quickly."
  (interactive)
  (if (and jpop-id jpop-use-caching)
    (jpop-cache-current-project))
  (if (> (length jpop-cache-alist) 0)
      (let* ((projects (-map
                        (lambda (elt) (cons (file-name-nondirectory (car elt)) (car elt)))
                        jpop-cache-alist))
             (project (completing-read "Switch to Cached Project: " projects)))
        (jpop-restore-cache (cdr (assoc project projects))))
    (call-interactively 'jpop-change)))

(defun jpop-switch-and-find-file ()
  "Switch to a cached project and find a file within."
  (interactive)
  (call-interactively 'jpop-switch)
  (call-interactively 'jpop-find-file))

(defun jpop-switch-buffer (&optional f)
  "Using `completing-read`, interactively switch between project buffers.

Optionally called F as the function used to switch the buffer."
  (interactive)
  (let ((project-buffers (-map 'buffer-name (jpop-get-project-buffers))))
        (funcall (or f 'switch-to-buffer) (completing-read
                (format "[%s] Switch to buffer: " jpop-id)
                project-buffers))))

(defun jpop-switch-buffer-other-window ()
  "Using `completing-read`, interactively switch buffers in other window for project buffers."
  (interactive)
  (jpop-switch-buffer 'switch-to-buffer-other-window))

(defun jpop-find-file ()
  "Call `jpop--find-file` for FILE with `find-file` as function call."
  (interactive)
  (jpop--find-file jpop-file-alist 'find-file))

(defun jpop-find-test ()
  "Call `jpop--find-file` for TEST-ALIST with `find-file` as function call."
  (interactive)
  (jpop--find-file (--map (cons (file-name-nondirectory (cadr it)) (cdr it)) jpop-test-alist) 'find-file))

(defun jpop-extended-find-file (file-alist-id)
  "Call `jpop--find-file` after prompting user to narrow down the alist using FILE-ALIST-ID."
  (interactive (list (completing-read "Library: " (mapcar 'car jpop-project-alist))))
  (jpop--find-file
     (cdr (assoc file-alist-id jpop-project-alist)) 'find-file))

(defun jpop-find-file-other-window ()
  "Call `jpop--find-file` for FILE with `find-file-other-window` as function call."
  (interactive)
  (jpop--find-file jpop-file-alist 'find-file-other-window))

(defun jpop-find-test-other-window ()
  "Call `jpop--find-file` for TEST-ALIST with `find-file-other-window` as function call."
  (interactive)
  (jpop--find-file (--map (cons (file-name-nondirectory (cadr it)) (cdr it)) jpop-test-alist) 'find-file))

(defun jpop-extended-find-file-other-window (file-alist-id)
  "Call `jpop--find-file` after prompting user to narrow down the alist using FILE-ALIST-ID."
  (interactive (list (completing-read "Library: " (mapcar 'car jpop-project-alist))))
  (jpop--find-file
     (cdr (assoc file-alist-id jpop-project-alist)) 'find-file-other-window))

(defun jpop--find-file (file-alist find-f)
  "Interactively find a file in your project.

Select a file matched using `completing-read` against the contents
of FILE-ALIST.  Options are displayed using READ-F.  If the file exists
in more than one directory, select directory.  Lastly the file is opened using FIND-F."

  (unless jpop-id
    (error "[jpop] You have not set a project yet - Set one by calling `jpop-find-file`"))

  (let* ((file (completing-read "File: " (mapcar jpop-completion-func file-alist)))
         (record (assoc (file-name-nondirectory file) file-alist)))
    (funcall find-f
      (if (= (length record) 2)
          (cadr record)
        (completing-read
         (format "Find %s in dir:" file) (cdr record))))))

(defun jpop--get-all-project-files (f)
  "Gets all files across cached projects and creats an alist in form (F . filepath)."
  (-reduce 'append
           (--map (let ((id (cdr (assoc 'id it)))
                        (all (cdr (assoc 'all it))))
                    (--map (cons (format f id (car it)) (cdr it)) all)) jpop-cache-alist)))

(defun jpop-find-file-from-all-projects ()
  "Call `jpop--find-file` for files contained within any currently cached projects."
  (interactive)
  (let ((all-project-files-alist (jpop--get-all-project-files "[%s] %s")))
    (jpop--find-file all-project-files-alist 'find-file)))

(defun jpop-find-file-from-all-projects-other-window ()
  "Call `jpop--find-file-other-window` for files contained within any currently cached projects."
  (interactive)
  (let ((all-project-files-alist (jpop--get-all-project-files "[%s] %s")))
    (jpop--find-file all-project-files-alist 'find-file-other-window)))

(defun jpop-find-dired ()
  "Interactively find a directory in your project.

Select a directory matched using `completing-read` against the contents
of `jpop-file-alist` filtered to only include unique direcotries."
  (interactive)
  (unless jpop-id
    (error "[jpop] You have not set a project yet - Set one by calling `jpop-find-file`"))

  (let* ((dir-list
          (mapcar
           (lambda (item)
             (let* ((limit 6)
                    (split-dir (reverse (-take limit (reverse (split-string item "/")))))
                    (dir (mapconcat 'identity split-dir "/")))
               (cons (format (if (< limit (length split-dir)) "/%s" ".../%s") dir) item)))
           (-uniq (--map (file-name-directory (cadr it)) (-concat jpop-all-alist jpop-test-alist)))))
         (read-dir (completing-read "Dired: " dir-list))
         (dir (cdr (assoc read-dir dir-list))))

    (find-file dir)))

(defun jpop-toggle-open-test-other-window ()
  "Open associated test class if it exists in the other window."
  (interactive)
  (jpop-toggle-open-test 'find-file-other-window))

(defun jpop-toggle-open-test (&optional f)
  "Open associated test class if it exists using F."
  (interactive)
  (let* ((find-f (or f 'find-file))
         (file-name (buffer-file-name))
         (filters (mapcar (lambda (r) (concat r "\\(\\.[a-z]+$\\)")) jpop-test-filter-regexps))
         (test-p (car (-non-nil (mapcar (lambda (r) (when (string-match r file-name) r)) filters))))
         (neutral-file-name (replace-regexp-in-string (or test-p "") "\\1" (file-name-nondirectory file-name)))
         (result (assoc neutral-file-name (if test-p jpop-all-alist jpop-test-alist))))
    (cond
     ((= 2 (length result)) (funcall find-f (cadr result)))
     ((> (length result) 2) (funcall find-f
                                     (completing-read
                                      (format "[%s] Open test/src file: " jpop-id) (cdr result))))
     (t (if jpop-toggle-test-fallback-hook
            (run-hooks 'jpop-toggle-test-fallback-hook)
          (jpop-message (format "Could not find the test/src file for [%s]" file-name) t))))))

;; Finding Files in Git Repos

(defun jpop--find-file-git (find-f)
  "Create a project quickly out of contained Git project and call FIND-F."
  (let* ((file-name (buffer-file-name))
         (git-repo (locate-dominating-file file-name ".git"))
         (cache-containing-file (jpop-cache-containing file-name)))

    (when (not (or git-repo cache-containing-file))
      (error (format "[jpop] File is not part of a git project [%s]"
                     (file-name-nondirectory (buffer-file-name))) t))

    (if (not cache-containing-file)
        (jpop-change git-repo)
      (jpop-message (format "Cowardly using project find file for [%s]" cache-containing-file) t)
      (jpop-restore-cache cache-containing-file))

    (funcall find-f)))

(defun jpop-git-find-file ()
  "Find a file in a git repository without having to change projets."
  (interactive)
  (jpop--find-file-git 'jpop-find-file))

(defun jpop-git-find-file-other-window ()
  "Find a file in a git repository without having to change projets."
  (interactive)
  (jpop--find-file-git 'jpop-find-file-other-window))

(defun jpop-git-find-test ()
  "Find a file in a git repository without having to change projets."
  (interactive)
  (jpop--find-file-git 'jpop-find-test))

(defun jpop-git-find-test-other-window ()
  "Find a file in a git repository without having to change projets."
  (interactive)
  (jpop--find-file-git 'jpop-find-test-other-window))

(defun jpop-reformat-file (&optional force)
  "Reformat tabs/spaces into correct format for current file.

If FORCE is non-nil then ignore the constrain effect."
  (interactive)
  (if (or (jpop-project-contains (buffer-file-name))
          (or force (not jpop-constrain-reformat)))
      (with-current-buffer (buffer-name)
        (save-excursion
          (goto-char (point-min))
          (while (search-forward (caddr jpop-indent-object) (point-max) t)
            (replace-match (cadr jpop-indent-object))))
        (indent-region (point-min) (point-max))
        (jpop-message
         (format "Reformatted file to use [%s]" (car jpop-indent-object)) t))
    (jpop-message
     (format "Reformat aborted - [%s] is not part of project" (file-name-nondirectory (buffer-file-name))) t)))

(defun jpop-build-space-string ()
  "Build the indent string of spaces.
i.e.  If indent level was 4, the indent string would be '    '."
  (make-string jpop-indent-level ? ))

(defun jpop-kill-project-buffers ()
  "Kill all open buffers in the current project."
  (interactive)
  (let* ((project-buffers (jpop-get-project-buffers)))
    (if project-buffers
        (let ((kill (yes-or-no-p
                     (format "[%s] Kill %d buffers (%s)? " jpop-id (length project-buffers) (mapconcat 'buffer-name project-buffers ", ")))))
          (when kill (mapc (lambda (buf) (kill-buffer buf)) project-buffers)))
      (jpop-message "You currently have no buffers open associated with this project" t))))

(defun jpop-get-project-buffers ()
  "Get a list of buffers within the current project."
  (-filter (lambda (buffer) (let* ((bufname (buffer-file-name buffer)))
                         (and bufname (jpop-project-contains bufname)))) (buffer-list)))

(defun jpop-project-contains (file)
  "Check whether FILE is contained within any of the projects directory paths."
  (-any? (lambda (r) (string-match (replace-regexp-in-string "~" "" r) file))
         (if jpop-project-plist
             (mapcar (lambda (elt) (plist-get elt :dir)) (plist-get jpop-project-plist :dirs))
           (and jpop-current-project-path (list jpop-current-project-path)))))

(defun jpop-cache-containing (file)
  "Find the ID of the first project cache containing FILE.

This algorithm prioritizes plist/json projects over anonymous
directory based ones."
  (unless file (error "[jpop] File is nil"))

  (let* ((cache-containers
          (-flatten-n 1 (--separate (json-plist-p (cdr (assoc 'plist it)))
                                    jpop-cache-alist)))

         (first-cache-containing-file
          (--first (jpop--cache-contains file it) cache-containers)))

    (car first-cache-containing-file)))

(defun jpop-visit-project-file ()
  "Open the project file currently being used."
  (interactive)
  (when jpop-current-project-path
    (when (not (file-directory-p jpop-current-project-path))
      (jpop-message
       (format "Current project is an anonymous path, not a project file [%s]" jpop-current-project-path) t))
    (find-file jpop-current-project-path)))

(defun jpop-get-ctags-supported-languages ()
  "Flatten and concatenate all supported languages for find command."
  (mapconcat 'format (-flatten (mapcar 'cdr jpop-ctags-supported-languages)) "|"))

(defun jpop-get-filter-regexps (&optional separator)
  "Flatten and concatenate all filter regexps for find command with SEPARATOR."
  (let ((local-completion-extensions (--mapcat (list (if (string-match "[a-z]$" it) (format "%s$" it) it)) completion-ignored-extensions)))
    (mapconcat 'regexp-quote (append local-completion-extensions jpop-filter-regexps) (or separator "|"))))

(defun jpop-run ()
  "Run commands associated with the projcet."
  (interactive)
  (unless (plist-get jpop-project-plist :commands)
    (error "[jpop] Current project does not have any associated commands"))

  (let* ((commands (plist-get jpop-project-plist :commands))
         (command (completing-read
                   "Run command: " (--map (substring (symbol-name it) 1) (-filter 'symbolp commands))))
         (command-plist (plist-get commands (intern (format ":%s" command))))

         (cmd  (intern (plist-get command-plist :cmd)))
         (args (append (plist-get command-plist :args) nil))
         (type (plist-get command-plist :type)))

    (cond
     ((string-equal type "lisp")
      (unless (fboundp cmd) (error "[jpop] Lisp function `%s` is not defined" cmd))
      (apply cmd args))
     (t (jpop-message (format "Unknown command type '%s'" type))))))

;;; Get things to do with the project
(defun jpop-get-requirejs-config ()
  "Get the requirejs config from the current project json."
  (let ((configs (append (plist-get jpop-project-plist :libs) nil)))
    (mapcar (lambda (config) (cons (plist-get config :id) (plist-get config :dir))) configs)))

;;; Jpop Mode
;;  Set up for the jpop minor-mode.
(defvar jpop-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'jpop-change)
    (define-key map (kbd "C") 'jpop-switch)
    (define-key map (kbd "r") 'jpop-refresh)
    (define-key map (kbd "p") 'jpop-change-and-find-file)
    (define-key map (kbd "P") 'jpop-switch-and-find-file)
    (define-key map (kbd "f d") 'jpop-find-dired)
    (define-key map (kbd "f e") 'jpop-extended-find-file)
    (define-key map (kbd "f E") 'jpop-extended-find-file-other-window)
    (define-key map (kbd "f f") 'jpop-find-file)
    (define-key map (kbd "f F") 'jpop-find-file-other-window)
    (define-key map (kbd "f t") 'jpop-find-test)
    (define-key map (kbd "f T") 'jpop-find-test-other-window)
    (define-key map (kbd "f a") 'jpop-find-file-from-all-projects)
    (define-key map (kbd "f A") 'jpop-find-file-from-all-projects-other-window)
    (define-key map (kbd "g f") 'jpop-git-find-file)
    (define-key map (kbd "g F") 'jpop-git-find-file-other-window)
    (define-key map (kbd "g t") 'jpop-git-find-test)
    (define-key map (kbd "g T") 'jpop-git-find-test-other-window)
    (define-key map (kbd "t") 'jpop-toggle-open-test)
    (define-key map (kbd "T") 'jpop-toggle-open-test-other-window)
    (define-key map (kbd "l") 'jpop-reformat-file)
    (define-key map (kbd "L") '(lambda (interactive) (jpop-reformat-file t)))
    (define-key map (kbd "o") 'jpop-visit-project-file)
    (define-key map (kbd "b") 'jpop-switch-buffer)
    (define-key map (kbd "B") 'jpop-switch-buffer-other-window)
    (define-key map (kbd "k") 'jpop-kill-project-buffers)
    (define-key map (kbd "s") 'jpop-stylise)
    (define-key map (kbd "x") 'jpop-jpop-run)
    map)
  "Keymap for Jpop commands after `jpop-keymap-prefix'.")
(fset 'jpop-command-map jpop-command-map)
(defvar jpop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jpop-keymap-prefix 'jpop-command-map)
    map)
  "Keymap for Projectile mode.")

;;;###autoload
(define-minor-mode jpop-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `jpop-mode'.  With prefix
ARG, enable `jpop-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `jpop-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `jpop-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
  :lighter  jpop-mode-line
  :keymap jpop-mode-map
  :group 'jpop
  :require 'jpop)

;;;###autoload
(define-globalized-minor-mode jpop-global-mode
  jpop-mode
  jpop-mode)

(provide 'jpop)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; jpop.el ends here
