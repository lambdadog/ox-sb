;;; ox-sb.el --- SB/SB BBCode Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ashlynn Anderson <contact@pea.sh>
;; Licensed under GNU GPL v3 or later.
;; Attribution to Christian Garbs <mitch@cgarbs.de> for writing ox-bb

;; This file is part of ox-sb.

;; ox-sb is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ox-sb is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ox-sb.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Ashlynn Anderson <contact@pea.sh>
;; URL: https://github.com/lambdadog/ox-sb
;; Keywords: bbcode, org, export, outlines
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (org "8.0"))

;;; Commentary:

;; This library implements a SpaceBattles/Sufficient Velocity BBCode
;; back-end for Org exporter.
;;
;; ox-bb provides three different commands for export:
;;
;; - `ox-sb-export-as-bbcode' exports to a buffer named "*Org BBCode
;;   Export*" and automatically applies `bbcode-mode' if it is
;;   available.
;;
;; - `ox-sb-export-to-kill-ring' does the same and additionally copies
;;   the exported buffer to the kill ring so that the generated BBCode
;;   is available in the Clipboard.
;;
;; - `ox-sb-export-to-bbcode' exports to a file with extension
;;   ".bbcode".

;;; Code:

(require 'ox)

;;; Backend definition

;; internal reminder: for Org format information see
;; https://orgmode.org/worg/dev/org-element-api.html

(org-export-define-backend 'sb
  '((bold . ox-sb-bold)
    (center-block . ox-sb-undefined)
    (clock . ox-sb-undefined)
    (code . ox-sb-code)
    (drawer . ox-sb-undefined)
    (dynamic-block . ox-sb-undefined)
    (entity . ox-sb-entity)
    (example-block . ox-sb-example-block)
    (export-block . ox-sb-undefined)
    (export-snippet . ox-sb-undefined)
    (fixed-width . ox-sb-fixed-width)
    (footnote-definition . ox-sb-footnote-definition)
    (footnote-reference . ox-sb-footnote-reference)
    (headline . ox-sb-headline)
    (horizontal-rule . ox-sb-horizontal-rule)
    (inline-src-block . ox-sb-undefined)
    (inlinetask . ox-sb-undefined)
    (inner-template . ox-sb-inner-template)
    (italic . ox-sb-italic)
    (item . ox-sb-item)
    ;; (keyword . ox-sb-undefined) ;; don't fail, just skip keywords
    (latex-environment . ox-sb-undefined)
    (latex-fragment . ox-sb-undefined)
    (line-break . ox-sb-line-break)
    (link . ox-sb-link)
    (node-property . ox-sb-undefined)
    (paragraph . ox-sb-paragraph)
    (plain-list . ox-sb-plain-list)
    (plain-text . ox-sb-plain-text)
    (planning . ox-sb-undefined)
    (property-drawer . ox-sb-undefined)
    (quote-block . ox-sb-quote-block)
    (radio-target . ox-sb-undefined)
    (section . ox-sb-section)
    (special-block . ox-sb-undefined)
    (src-block . ox-sb-src-block)
    (statistics-cookie . ox-sb-undefined)
    (strike-through . ox-sb-strike-through)
    (subscript . ox-sb-undefined)
    (superscript . ox-sb-undefined)
    (table . ox-sb-table)
    (table-cell . ox-sb-table-cell)
    (table-row . ox-sb-table-row)
    (target . ox-sb-undefined)
    (template . ox-sb-template)
    (timestamp . ox-sb-undefined)
    (underline . ox-sb-underline)
    (verbatim . ox-sb-verbatim)
    (verse-block . ox-sb-undefined))
  :menu-entry
  '(?s "Export to SpaceBattles BBCode"
       ((?B "As BBCode buffer" ox-sb-export-as-bbcode)
	(?f "As BBCode file" ox-sb-export-to-bbcode)
	(?b "As BBCode buffer and to clipboard" ox-sb-export-to-kill-ring))))

;;; Helper methods

;; prevent bogus byte-compiler warning
(declare-function bbcode-mode "bbcode-mode" ())

(defun ox-sb--as-block (text)
  "Format TEXT as a block with leading and trailing newline."
  (concat "\n" text "\n"))

(defun ox-sb--force-leading-newline (text)
  "Make TEXT start with exactly one newline."
  (replace-regexp-in-string "\\`\n*" "\n" text))

(defun ox-sb--format-headline (text level)
  "Format TEXT as a headline of the given LEVEL."
  (let ((indent (cl-case level
		  (0 "")
		  (1 "# ")
		  (2 "== ")
		  (3 "+++ ")
		  (4 ":::: ")
		  (5 "----- ")
		  (t (user-error "Headline level `%s' is not defined yet" level)))))
    (concat
     (ox-sb--put-in-tag
      "b" (ox-sb--put-in-tag
	   "u" (concat indent text)))
     "\n\n")))

(defun ox-sb--put-in-tag (tag contents &optional attributes)
  "Puts the BBcode tag TAG around the CONTENTS string.
Optional ATTRIBUTES for the tag can be given as an alist of
key/value pairs (both strings)."
  (let ((attribute-string (if attributes
			      (mapconcat (function (lambda (attribute)
						     (let ((key (car attribute))
							   (value (cadr attribute)))
						       (format " %s=\"%s\"" key value))))
					 attributes
					 "")
			    "")))
    (format "[%s%s]%s[/%s]" tag attribute-string contents tag)))

(defun ox-sb--put-in-value-tag (tag contents value)
  "Puts the BBcode tag TAG around the CONTENTS string.
The VALUE is assigned directly to the tag instead of a normal
key/value pair."
  (format "[%s=%s]%s[/%s]" tag value contents tag))

(defun ox-sb--fix-url (url)
  "Fix URL returned from `url-encode-url'.
Older versions of Emacs (eg. 24.3 used in the Travis CI minimal
image) prepend \"/\" to urls consisting only of an \"#anchor\"
part.  We don't want this, because we need relative anchors.  Fix
this the hard way."
  (if (string-prefix-p "/#" url)
      (substring url 1)
    url))

(defun ox-sb--put-url (contents href)
  "Puts the CONTENTS inside a [url] tag pointing to HREF.
Automagically escapes the target URL."
  (let* ((target (ox-sb--fix-url (url-encode-url (org-link-unescape href))))
	 (text   (or contents target)))
    (ox-sb--put-in-value-tag "url" text target)))

(defun ox-sb--remove-leading-newline (text)
  "Remove a leading empty line from TEXT."
  (replace-regexp-in-string "\\`\n" "" text))

(defun ox-sb--remove-trailing-newline (text)
  "Remove the trailing newline from TEXT."
  (replace-regexp-in-string "\n\\'" "" text))

;;; Backend callbacks

(defun ox-sb-bold (_bold contents _info)
  "Transcode a BOLD element from Org to BBCode.
CONTENTS is the bold text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-sb--put-in-tag "b" contents))

(defun ox-sb-code (code _contents _info)
  "Transcode a CODE element from Org to BBCode.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (ox-sb--put-in-value-tag "font" (org-element-property :value code) "monospace"))

(defun ox-sb-entity (entity _contents _info)
  "Transcode an ENTITY element from Org to BBCode.
CONTENTS is the definition itself.  INFO is a plist used as a
communication channel."
  (org-element-property :html entity))

(defun ox-sb-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to SB BBCode."
  (format "[FONT=courier new]%s[/FONT]"
	  (ox-sb--remove-trailing-newline
	   (org-export-format-code-default example-block info))))

(defun ox-sb-src-block (code-block _contents info)
  "Transcode a CODE-BLOCK element from Org to SB BBCode."
  (format "[CODE]%s[/CODE]"
	  (ox-sb--remove-trailing-newline
	   (org-export-format-code-default code-block info))))

(defun ox-sb-fixed-width (fixed-width _contents _info)
  "Transcode a FIXED-WIDTH element from Org to BBCode.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (ox-sb--put-in-tag "code"
		     (concat "\n" (org-element-property :value fixed-width))))

(defun ox-sb-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to BBCode.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (eq (org-element-property :type footnote-reference) 'inline)
      (user-error "Inline footnotes not supported yet")
    (let ((n (org-export-get-footnote-number footnote-reference info)))
      (format "^%d " n))))

(defun ox-sb-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn))
	(def (cdr fn)))
    (format "^%d: %s" n def)))

(defun ox-sb-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((org-major (string-to-number (car (split-string (org-release) "\\."))))
	 (fn-alist (if (> org-major 8)
		       (org-export-collect-footnote-definitions
			info (plist-get info :parse-tree))
		     (org-export-collect-footnote-definitions
		      (plist-get info :parse-tree) info)))
	 (fn-alist
	  (cl-loop for (n _label raw) in fn-alist collect
		   (cons n (org-trim (org-export-data raw info)))))
	 (text (mapconcat #'ox-sb-format-footnote-definition fn-alist "\n")))
    (if fn-alist
	(concat "\n" (ox-sb--format-headline "Footnotes" 0) text)
      "")))

(defun ox-sb-headline (headline contents info)
  "Transcode HEADLINE element from Org to BBCode.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let ((title (org-export-data (org-element-property :title headline) info))
	(level (org-export-get-relative-level headline info)))
    (if (org-element-property :footnote-section-p headline)
	""
      (concat
       (ox-sb--format-headline title level)
       contents))))

(defun ox-sb-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode a HORIZONTAL-RULE object to BBCode"
  "[HR=1]")

(defun ox-sb-inner-template (contents info)
  "Return body of document string after BBCode conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   contents
   (ox-sb-footnote-section info)))

(defun ox-sb-italic (_italic contents _info)
  "Transcode a ITALIC element from Org to BBCode.
CONTENTS is the italic text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-sb--put-in-tag "i" contents))

(defun ox-sb-item (item contents info)
  "Transcode a ITEM element from Org to BBCode.
CONTENTS is the contents of the item, as a string.  INFO is
  a plist used as a communication channel."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (text (org-trim contents)))
    (concat
     "[*]"
     (pcase type
       (`descriptive
	(let ((term (let ((tag (org-element-property :tag item)))
		      (and tag (org-export-data tag info)))))
	  (concat
	   (ox-sb--put-in-tag "i" (concat (org-trim term) ":"))
	   " "))))
     text
     "\n")))

(defun ox-sb-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to BBCode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  "[br]_[/br]\n")

(defun ox-sb-link (link contents _info)
  "Transcode a LINK element from Org to BBCode.
CONTENTS is the contents of the link, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link))
	(raw  (org-element-property :raw-link link)))
    (cond
     ((string= type "fuzzy")
      (cond
       ((string-prefix-p "about:" raw)
	(ox-sb--put-url contents raw))
       (t (user-error "Unknown fuzzy LINK type encountered: `%s'" raw))))
     ((member type '("http" "https"))
      (ox-sb--put-url contents (concat type ":" path)))
     (t (user-error "LINK type `%s' not yet supported" type)))))

(defun ox-sb-paragraph (_paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to BBCode.
CONTENTS is the contents of the paragraph, as a string.  INFO is
  a plist used as a communication channel."
  (org-trim contents))

(defun ox-sb-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to BBCode.
CONTENTS is the contents of the plain-list, as a string.  INFO is
  a plist used as a communication channel."
  (let ((type (org-element-property :type plain-list))
	(content-block (ox-sb--as-block (org-trim contents))))
    (concat
     (pcase type
       (`descriptive (ox-sb--put-in-tag "list" content-block))
       (`unordered (ox-sb--put-in-tag "list" content-block))
       (`ordered (ox-sb--put-in-value-tag "list" content-block "1"))
       (other (user-error "PLAIN-LIST type `%s' not yet supported" other)))
     "\n")))

(defun ox-sb-plain-text (text _info)
  "Transcode a TEXT string from Org to BBCode.
INFO is a plist used as a communication channel."
  text)

(defun ox-sb-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to BBCode.
CONTENTS holds the contents of the block.  INFO is a plist used
as a communication channel."
  (ox-sb--put-in-tag "quote" (ox-sb--force-leading-newline contents)))

(defun ox-sb-section (_section contents _info)
  "Transcode a SECTION element from Org to BBCode.
CONTENTS is the contents of the section, as a string.  INFO is a
  plist used as a communication channel."
  (org-trim contents))

(defun ox-sb-strike-through (_strike-through contents _info)
  "Transcode a STRIKE-THROUGH element from Org to BBCode.
CONTENTS is the text with strike-through markup, as a string.
  INFO is a plist used as a communication channel."
  (ox-sb--put-in-tag "s" contents))

(defun ox-sb-table (_table contents _info)
  "Transcode a TABLE element from Org to BBCode.
CONTENTS contains the already rendered body of the table.  INFO
is a plist used as a communication channel."
  (ox-sb--put-in-tag "table" contents))

(defun ox-sb-table-row (_table-row contents _info)
  "Transcode a TABLE-ROW element from Org to BBCode.
CONTENTS contains the already rendered row content.  INFO is a
plist used as a communication channel."
  (if contents
      (ox-sb--put-in-tag "tr" contents)
    ""))

(defun ox-sb-table-cell (_table-cell contents _info)
  "Transcode a TABLE-CELL element from Org to BBCode.
CONTENTS contains the already rendered cell content.  INFO is a
plist used as a communication channel."
  (ox-sb--put-in-tag "td" contents))

(defun ox-sb-template (contents _info)
  "Return complete document string after BBCode conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun ox-sb-undefined (element &optional _contents _info)
  "Throw an error when an unsupported ELEMENT is encountered."
  (user-error "ELEMENT type `%s' not implemented yet" (car element)))

(defun ox-sb-underline (_underline contents _info)
  "Transcode a UNDERLINE element from Org to BBCode.
CONTENTS is the underlined text, as a string.  INFO is
  a plist used as a communication channel."
  (ox-sb--put-in-tag "u" contents))

(defun ox-sb-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM element from Org to BBCode.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (ox-sb--put-in-value-tag "font" (org-element-property :value verbatim) "monospace"))

;;; Export methods

;;;###autoload
(defun ox-sb-export-as-bbcode
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a BBCode buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org BBCode Export*\".  If
available, `bbcode-mode' is enabled in the buffer."
  (interactive)
  (org-export-to-buffer 'sb "*Org SB BBCode Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (when (featurep 'bbcode-mode) (bbcode-mode)))))

;;;###autoload
(defun ox-sb-export-to-bbcode
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a BBCode file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension ".bbcode")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'sb file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-sb-export-to-kill-ring
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a BBCode buffer and kill ring.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

TODO: document BODY-ONLY

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org BBCode Export*\" which is
automatically copied to the kill ring (Clipboard)."
  (interactive)
  (let ((oldval org-export-copy-to-kill-ring))
    (progn
      (setq org-export-copy-to-kill-ring t)
      (ox-sb-export-as-bbcode async subtreep visible-only body-only ext-plist)
      (setq org-export-copy-to-kill-ring oldval))))

;;; Register file

(provide 'ox-sb)

;;; ox-sb.el ends here
