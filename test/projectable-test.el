;;; projectable-test.el --- Tests for my projectable packages

;; Copyright (C) 2015  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: convenience, tools

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

;;; Code:

(require 'f)
(require 'noflet)
(require 'json)

(defvar root-test-path (f-dirname (f-this-file)))
(defvar root-code-path (f-parent root-test-path))
(defvar root-sandbox-path (f-expand "sandbox" root-test-path))

(require 'projectable (f-expand "projectable.el" root-code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body
     (f-delete root-sandbox-path :force)))

(ert-deftest should-set-projectable-directory ()
  (should (string= projectable-dir (f-slash root-code-path))))

(ert-deftest should-call-load-from-json-when-given-json-file ()
   (let ((f-called nil))
     (noflet ((projectable-load-from-json () (setq f-called t) ))
       (projectable-change (f-expand "projectable-test.json" root-test-path))
       (should f-called))))

(ert-deftest should-call-load-from-path-when-given-a-directory ()
   (let ((f-called nil))
     (noflet ((projectable-load-from-path () (setq f-called t) ))
       (projectable-change root-test-path)
       (should f-called))))

(ert-deftest should-set-the-project-id ()
   (projectable-change (f-expand "projectable-test.json" root-test-path))
   (should (string= "projectable" projectable-id)))

(ert-deftest should-tell-whether-path-is-directory ()
  (should (eq nil (projectable-is-file root-test-path)))
  (should (projectable-is-file (f-expand "projectable-test.json" root-test-path))))

(provide 'projectable-test)
;;; projectable-test.el ends here
