;;; denote-graph.el --- graph generation for Denote         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Matteo Cavada

;; Author: Matteo Cavada <matteo.cavada@inventati.org>
;; Keywords: denote note-taking
;; Version: 0.1.0
;; Package-Requires: ((denote "2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple package to generate DOT files from your `denote-directory`.

;;; Code:

(require 'denote)

(defgroup denote-graph nil
  "DOT graph generation for your `denote` knowledge base."
  :group 'denote)

(defcustom denote-graph-output-filename
  "zettelkasten.dot"
  "Name of the generated DOT file."
  :type 'string
  :group 'denote-graph)

(defcustom denote-graph-filtering-function
  '(lambda (row) t)
  "A function that return =T= when a node is to be included in the final graph.

The function must take a ROW in input (i.e. a three elements list of the form
(timestamp human-name (keywords)), which encodes a node) and must return t when
the row is to be included in the final graph, nil otherwise."
  :type 'function
  :group 'denote-graph
  )

;;; =============================================================================
;;; Utility functions

(defun denote-graph--flatten (lst)
  "Flatten only one level of nested lists in the given LST."
  (apply #'append (mapcar (lambda (x) (if (listp x) x (list x))) lst)))


;;; =============================================================================
;;; Generate nodes and a graph from the `denote` directory.
;;; A node is simply a list of the form: (timestamp human-name (keyword1 keyword2 ...))

(defun denote-graph--extract-keywords-from (absolute-fpath)
  "Return the keywords list of ABSOLUTE-FPATH, if any; otherwise, nil."
  (let ((raw-keywords
	 (split-string (denote-retrieve-filename-keywords absolute-fpath) " ")))
    (if (cl-equalp (car raw-keywords) "")
	nil
	raw-keywords)))

(defun denote-graph--mk-node (absolute-fpath)
  "Generate a node from ABSOLUTE-FPATH.

The node will be structured as (timestamp human-title (keyword1 keyword2 ...))"
  (list (denote-retrieve-filename-identifier absolute-fpath)
	(denote-retrieve-filename-title absolute-fpath)
	(denote-graph--extract-keywords-from absolute-fpath)))

(defun denote-graph--mk-graph ()
  "Generate a Lisp graph from the files in denote-directory."
  ;; A graph is a list of rows.
  ;; A row is a list of nodes; the first node is the root, the rest are its neighbours.
  ;; A node is a list of the form: (timestamp human-name (keyword1 keyword2 ...))
  (cl-loop for fname in (denote-directory-files) collect
    (mapcar #'denote-graph--mk-node
 	    (cons fname (denote-link-return-links fname)))))

;; (insert (prin1-to-string (denote-graph--mk-graph)))


;;; =============================================================================
;;; Convert the lisp representations of nodes into DOT source code

(defun denote-graph--generate-dot-nodename (node)
  "Extract a nodename from NODE."
  ;; Eg. (timestamp human keywords) -> "human"
  (cl-destructuring-bind (_timestamp human-name _keywords) node
    (format "\"%s\"" human-name)))

(defun denote-graph--generate-dot-nodes-list (graph)
  "Generate DOT source code which declares nodes.

The list of unique nodenames is extracted from GRAPH.
The result is a list of strings, each one declaring a
graph node in DOT language."
  (cl-loop
   for tuple in (cl-delete-duplicates (denote-graph--flatten graph) :test 'equalp)
	  collect
	  (denote-graph--generate-dot-nodename tuple)))


;;; =============================================================================
;;; Convert the lisp representations of rows into strings of DOT source code.
;;; A row is a list of nodes, such as ((ts1 name1) (ts2 name2) (ts3 name3)); this
;;; list is translated into name1 -> {name2 name3 ...}

(defun denote-graph--generate-dot-edges (row)
  "Generate DOT code that describes the edges encoded in ROW."
  ;; Eg. generates 'name1 -> {name2 name3 ...}'"
  (cl-destructuring-bind (root-node &rest links) row
    (format "%s -> {%s}"
	    (denote-graph--generate-dot-nodename root-node)
	    (mapconcat 'identity
		       (mapcar #'denote-graph--generate-dot-nodename links)
		       " "))))

(defun denote-graph--generate-dot-edges-list (graph)
  "For each row in GRAPH, generate its DOT source code."
  (cl-loop for row in graph collect
	   (denote-graph--generate-dot-edges row)))


;;; =============================================================================
;;; Functions to exclude some rows from the final graph

(defun denote-graph--filter-undesired-rows (graph)
  "Filters out unwanted nodes from GRAPH."
  (cl-loop for row in graph
	   if (funcall denote-graph-filtering-function row)
	   collect row))


;;; =============================================================================
;;; Finally...

(defun denote-graph-generate-dot-sourcecode ()
  "Generate the DOT source code of your knowledge graph."
  (let ((graph (denote-graph--filter-undesired-rows (denote-graph--mk-graph))))
    (format "digraph {\n%s\n%s\n}"
	    (mapconcat 'identity (denote-graph--generate-dot-nodes-list graph) "\n")
	    (mapconcat 'identity (denote-graph--generate-dot-edges-list graph) "\n"))))

(defun denote-graph-generate-dot-file ()
  "Write to file the DOT source code of your knowledge graph."
  (interactive)
  (message "Generating DOT source code...")
  (with-temp-file denote-graph-output-filename
    (insert (denote-graph-generate-dot-sourcecode)))
  (find-file denote-graph-output-filename)
  (message "Done"))

;; (insert (denote-graph-generate-dot-sourcecode))

(provide 'denote-graph)

;;; denote-graph.el ends here
