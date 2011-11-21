;;; dabbrev.el --- dynamic abbreviation package

;; Copyright (C) 1985, 86, 92, 94, 96, 1997, 2000, 2001
;;   Free Software Foundation, Inc.

;; Author: Don Morrison
;; Maintainer: Lars Lindberg <Lars.Lindberg@sypro.cap.se>
;; Created: 16 Mars 1992
;; Lindberg's last update version: 5.7
;; Keywords: abbrev expand completion convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The purpose with this package is to let you write just a few
;; characters of words you've written earlier to be able to expand
;; them.
;;
;; To expand a word, just put the point right after the word and press
;; M-/ (dabbrev-expand) or M-C-/ (dabbrev-completion).
;;
;; Check out the customizable variables below to learn about all the
;; features of this package.

;;; Hints and tips for major modes writers:

;; Recommended values		C/Lisp etc	text
;; dabbrev-case-fold-search	nil		t
;; dabbrev-case-replace		nil		t
;;
;; Set the variables you want special for your mode like this:
;; (set (make-local-variable 'dabbrev-case-replace) nil)
;; Then you don't interfere with other modes.
;;
;; If your mode handles buffers that refers to other buffers
;; (i.e. compilation-mode, gud-mode), then try to set
;; `dabbrev-select-buffers-function' or `dabbrev-friend-buffer-function'
;; to a function that point out those buffers.

;; Same goes for major-modes that are connected to other modes.  There
;; are for instance a number of mail-modes.  One for reading, one for
;; creating a new mail etc.  Maybe those should be connected.

;; Example for GNUS (when we write a reply, we want dabbrev to look in
;; the article for expansion):
;; (set (make-local-variable 'dabbrev-friend-buffer-function)
;;      (lambda (buffer)
;;         (save-excursion
;;           (set-buffer buffer)
;;           (memq major-mode '(news-reply-mode gnus-article-mode)))))


;; Known bugs and limitations.
;; - Possible to do several levels of `dabbrev-completion' in the
;;   minibuffer.
;; - dabbrev-completion doesn't handle resetting the globals variables
;;   right.  It resets them after finding the abbrev.

;; Future enhancements
;;  - Check the tags-files? Like tags-complete?
;;  - Add the possibility of searching both forward and backward to
;;    the nearest expansion.
;;  - Check the kill-ring when everything else fails.  (Maybe something
;;  for hippie-expand?).  [Bng] <boris@cs.rochester.edu>

;;; These people gave suggestions:
;;  [hymie]	Hyman Rosen <marks!hymie@jyacc.jyacc.com>
;;  [burgett]	Steve Burgett <burgett@bizet.eecs.berkeley.edu>
;;  [jules]	Julian Gosnell <jules@x.co.uk>
;;  [kifer]	Michael Kifer <kifer@sbcs.sunysb.edu>
;;  [ake]	Ake Stenhoff <extaksf@aom.ericsson.se>
;;  [alon]	Alon Albert <al%imercury@uunet.uu.net>
;;  [tromey]	Tom Tromey <tromey@busco.lanl.gov>
;;  [Rolf]	Rolf Schreiber <rolf@mathematik.uni-stuttgart.de>
;;  [Petri]	Petri Raitio <per@tekla.fi>
;;  [ejb]	Jay Berkenbilt <ejb@ql.org>
;;  [hawley]	Bob Hawley <rth1@quartet.mt.att.com>
;;  ... and to all the people who have participated in the beta tests.

;;; Code:

;;----------------------------------------------------------------
;; Customization variables
;;----------------------------------------------------------------

(defgroup dabbrev nil
  "Dynamic Abbreviations"
  :tag "Dynamic Abbreviations"
  :group 'abbrev
  :group 'convenience)

(defcustom dabbrev-backward-only nil
  "*If non-nil, `dabbrev-expand' only looks backwards."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-limit nil
  "*Limits region searched by `dabbrev-expand' to this many chars away."
  :type '(choice (const :tag "off" nil)
		 integer)
  :group 'dabbrev)

(defcustom dabbrev-abbrev-skip-leading-regexp nil
  "*Regexp for skipping leading characters of an abbreviation.

Example: Set this to \"\\\\$\" for programming languages
in which variable names may appear with or without a leading `$'.
\(For example, in Makefiles.\)

Set this to nil if no characters should be skipped."
  :type '(choice regexp
		 (const :tag "off" nil))
  :group 'dabbrev)

(defcustom dabbrev--eliminate-newlines t
  "*Non-nil means dabbrev should not insert newlines.
Instead it converts them to spaces."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-case-fold-search 'case-fold-search
  "*Control whether dabbrev searches should ignore case.
A value of nil means case is significant.
A value of `case-fold-search' means case is significant
 if `case-fold-search' is nil.
Any other non-nil version means case is not significant."
  :type '(choice (const :tag "off" nil)
		 (const :tag "like search" case-fold-search)
		 (other :tag "on" t))
  :group 'dabbrev)

(defcustom dabbrev-upcase-means-case-search nil
  "*The significance of an uppercase character in an abbreviation.
nil means case fold search, non-nil means case sensitive search.

This variable has an effect only when the value of
`dabbrev-case-fold-search' says to ignore case."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-case-replace 'case-replace
  "*Controls whether dabbrev preserves case when expanding the abbreviation.
A value of nil means preserve case.
A value of `case-replace' means preserve case if `case-replace' is nil.
Any other non-nil version means do not preserve case.

This variable has an effect only when the value of
`dabbrev-case-fold-search' specifies to ignore case."
  :type '(choice (const :tag "off" nil)
		 (const :tag "like M-x query-replace" case-replace)
		 (other :tag "on" t))
  :group 'dabbrev)

(defcustom dabbrev-abbrev-char-regexp nil
  "*Regexp to recognize a character in an abbreviation or expansion.
This regexp will be surrounded with \\\\( ... \\\\) when actually used.

Set this variable to \"\\\\sw\" if you want ordinary words or
\"\\\\sw\\\\|\\\\s_\" if you want symbols (including characters whose
syntax is \"symbol\" as well as those whose syntax is \"word\".

The value nil has a special meaning: the abbreviation is from point to
previous word-start, but the search is for symbols.

For instance, if you are programming in Lisp, `yes-or-no-p' is a symbol,
while `yes', `or', `no' and `p' are considered words.  If this
variable is nil, then expanding `yes-or-no-' looks for a symbol
starting with or containing `no-'.  If you set this variable to
\"\\\\sw\\\\|\\\\s_\", that expansion looks for a symbol starting with
`yes-or-no-'.  Finally, if you set this variable to \"\\\\sw\", then
expanding `yes-or-no-' signals an error because `-' is not part of a word;
but expanding `yes-or-no' looks for a word starting with `no'.

The recommended value is \"\\\\sw\\\\|\\\\s_\"."
  :type '(choice (const nil)
		 regexp)
  :group 'dabbrev)

(defcustom dabbrev-check-all-buffers t
  "*Non-nil means dabbrev package should search *all* buffers.

Dabbrev always searches the current buffer first.  Then, if
`dabbrev-check-other-buffers' says so, it searches the buffers
designated by `dabbrev-select-buffers-function'.

Then, if `dabbrev-check-all-buffers' is non-nil, dabbrev searches
all the other buffers, except those named in `dabbrev-ignored-buffer-names',
or matched by `dabbrev-ignored-regexps'."
  :type 'boolean
  :group 'dabbrev)

(defcustom dabbrev-ignored-buffer-names '("*Messages*" "*Buffer List*")
  "*List of buffer names that dabbrev should not check.
See also `dabbrev-ignored-buffer-regexps'."
  :type '(repeat (string :tag "Buffer name"))
  :group 'dabbrev
  :version "20.3")

(defcustom dabbrev-ignored-buffer-regexps nil
  "*List of regexps matching names of buffers that dabbrev should not check.
See also `dabbrev-ignored-buffer-names'."
  :type '(repeat regexp)
  :group 'dabbrev
  :version "21.1")

(defcustom dabbrev-check-other-buffers t
  "*Should \\[dabbrev-expand] look in other buffers?\

nil: Don't look in other buffers.
t: Also look for expansions in the buffers pointed out by
   `dabbrev-select-buffers-function'.
Anything else: When we can't find any more expansions in
the current buffer, then ask the user whether to look in other
buffers too.

The default value is t."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (other :tag "ask" other))
  :group 'dabbrev)

;; I guess setting this to a function that selects all C- or C++-
;; mode buffers would be a good choice for a debugging buffer,
;; when debugging C- or C++-code.
(defvar dabbrev-select-buffers-function 'dabbrev--select-buffers
  "A function that selects buffers that should be searched by dabbrev.
The function should take no arguments and return a list of buffers to
search for expansions.  See the source of `dabbrev--select-buffers'
for an example.

A mode setting this variable should make it buffer local.")

(defcustom dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p
  "*A function to decide whether dabbrev should search OTHER-BUFFER.
The function should take one argument, OTHER-BUFFER, and return
non-nil if that buffer should be searched.  Have a look at
`dabbrev--same-major-mode-p' for an example.

The value of `dabbrev-friend-buffer-function' has an effect only if
the value of `dabbrev-select-buffers-function' uses it.  The function
`dabbrev--select-buffers' is one function you can use here.

A mode setting this variable should make it buffer local."
  :type 'function
  :group 'dabbrev)

(defcustom dabbrev-search-these-buffers-only nil
  "If non-nil, a list of buffers which dabbrev should search.
If this variable is non-nil, dabbrev will only look in these buffers.
It will not even look in the current buffer if it is not a member of
this list.")

;;----------------------------------------------------------------
;; Internal variables
;;----------------------------------------------------------------

;; Last obarray of completions in `dabbrev-completion'
(defvar dabbrev--last-obarray nil)

;; Table of expansions seen so far
(defvar dabbrev--last-table nil)

;; Last string we tried to expand.
(defvar dabbrev--last-abbreviation nil)

;; Location last abbreviation began
(defvar dabbrev--last-abbrev-location nil)

;; Direction of last dabbrevs search
(defvar dabbrev--last-direction 0)

;; Last expansion of an abbreviation.
(defvar dabbrev--last-expansion nil)

;; Location the last expansion was found.
(defvar dabbrev--last-expansion-location nil)

;; The list of remaining buffers with the same mode as current buffer.
(defvar dabbrev--friend-buffer-list nil)

;; The buffer we looked in last, not counting the current buffer.
(defvar dabbrev--last-buffer nil)

;; The buffer we found the expansion last time.
(defvar dabbrev--last-buffer-found nil)

;; The buffer we last did a completion in.
(defvar dabbrev--last-completion-buffer nil)

;; If non-nil, a function to use when copying successive words.
;; It should be `upcase' or `downcase'.
(defvar dabbrev--last-case-pattern nil)

;; Same as dabbrev-check-other-buffers, but is set for every expand.
(defvar dabbrev--check-other-buffers dabbrev-check-other-buffers)

;; The regexp for recognizing a character in an abbreviation.
(defvar dabbrev--abbrev-char-regexp nil)

;;----------------------------------------------------------------
;; Macros
;;----------------------------------------------------------------

;;; Get the buffer that mini-buffer was activated from
(defsubst dabbrev--minibuffer-origin ()
  (car (cdr (buffer-list))))

;; Make a list of some of the elements of LIST.
;; Check each element of LIST, storing it temporarily in the
;; variable ELEMENT, and include it in the result
;; if CONDITION evaluates non-nil.
(defmacro dabbrev-filter-elements (element list condition)
  `(let (dabbrev-result dabbrev-tail ,element)
    (setq dabbrev-tail ,list)
    (while dabbrev-tail
      (setq ,element (car dabbrev-tail))
      (if ,condition
          (setq dabbrev-result (cons ,element dabbrev-result)))
      (setq dabbrev-tail (cdr dabbrev-tail)))
    (nreverse dabbrev-result)))

;;----------------------------------------------------------------
;; Exported functions
;;----------------------------------------------------------------

;;;###autoload
(define-key esc-map "/" 'dabbrev-expand)
;;;??? Do we want this?
;;;###autoload
(define-key esc-map [?\C-/] 'dabbrev-completion)

;;;###autoload
(defun dabbrev-completion (&optional arg)
  "Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from C-u C-u),
then it searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."

  (interactive "*P")
  ;;; Beginning of Wang's code segment 1
  ;;; Set the major mode of wordlist buffers to the major mode of current
  ;;; buffer so as to have word lists searched for completions.
  (let ((mode major-mode))
    (mapcar (lambda (buffer)
	      (with-current-buffer buffer
		(setq major-mode mode)))
	    (dabbrev-wordlist-buffers)))
  ;;; End of Wang's code segment 1
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
	 (dabbrev-check-all-buffers
	  (and arg (= (prefix-numeric-value arg) 16)))
	 (abbrev (dabbrev--abbrev-at-point))
	 (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
				 case-fold-search
			       dabbrev-case-fold-search)
			     (or (not dabbrev-upcase-means-case-search)
				 (string= abbrev (downcase abbrev)))))
	 (my-obarray dabbrev--last-obarray)
	 init)
    (save-excursion
      (if (and (null arg)
	       my-obarray
	       (or (eq dabbrev--last-completion-buffer (current-buffer))
		   (and (window-minibuffer-p (selected-window))
			(eq dabbrev--last-completion-buffer
			    (dabbrev--minibuffer-origin))))
	       dabbrev--last-abbreviation
	       (>= (length abbrev) (length dabbrev--last-abbreviation))
	       (string= dabbrev--last-abbreviation
			(substring abbrev 0
				   (length dabbrev--last-abbreviation)))
	       (setq init (try-completion abbrev my-obarray)))
	  ;; We can reuse the existing completion list.
	  nil
	;;--------------------------------
	;; New abbreviation to expand.
	;;--------------------------------
	(setq dabbrev--last-abbreviation abbrev)
	;; Find all expansion
	(let ((completion-list
	       (dabbrev--find-all-expansions abbrev ignore-case-p))
	      (completion-ignore-case ignore-case-p))
	  ;; Make an obarray with all expansions
	  (setq my-obarray (make-vector (length completion-list) 0))
	  (or (> (length my-obarray) 0)
	      (error "No dynamic expansion for \"%s\" found%s"
		     abbrev
		     (if dabbrev--check-other-buffers "" " in this-buffer")))
	  (cond
	   ((or (not ignore-case-p)
		(not dabbrev-case-replace))
	    (mapc (function (lambda (string)
			      (intern string my-obarray)))
		    completion-list))
	   ((string= abbrev (upcase abbrev))
	    (mapc (function (lambda (string)
			      (intern (upcase string) my-obarray)))
		    completion-list))
	   ((string= (substring abbrev 0 1)
		     (upcase (substring abbrev 0 1)))
	    (mapc (function (lambda (string)
			      (intern (capitalize string) my-obarray)))
		    completion-list))
	   (t
	    (mapc (function (lambda (string)
			      (intern (downcase string) my-obarray)))
		    completion-list)))
	  (setq dabbrev--last-obarray my-obarray)
	  (setq dabbrev--last-completion-buffer (current-buffer))
	  ;; Find the longest common string.
	  (setq init (try-completion abbrev my-obarray)))))
    ;;--------------------------------
    ;; Let the user choose between the expansions
    ;;--------------------------------
    (or (stringp init)
	(setq init abbrev))
    (cond
     ;; * Replace string fragment with matched common substring completion.
     ((and (not (string-equal init ""))
	   (not (string-equal (downcase init) (downcase abbrev))))
      (if (> (length (all-completions init my-obarray)) 1)
	  (message "Repeat `%s' to see all completions"
		   (key-description (this-command-keys)))
	(message "The only possible completion"))
      (dabbrev--substitute-expansion nil abbrev init nil))
     (t
      ;; * String is a common substring completion already.  Make list.
      (message "Making completion list...")
      (with-output-to-temp-buffer " *Completions*"
	(display-completion-list (all-completions init my-obarray)))
      (message "Making completion list...done")))
    (and (window-minibuffer-p (selected-window))
	 (message nil))))

;;; Brief history of improvement by Zhongxiao (David) Wang, zwang@mail.smu.edu
;;;
;;; March 15, 2001
;;;---------------
;;; Basic functions implemented.
;;;
;;; May 4, 2002
;;;---------------
;;; Add features to enable use of dictionary completion.
;;;
;;; May 9, 2002
;;;---------------
;;; Originally, documents were linked to word lists by file name extension. Thanks to
;;; Richard Stallman's suggestion. Now, we link document to word list by current major
;;; mode. That's really a good idea. In practice, it's quite often that your document
;;; will use different word list completion with different parts. For example, you
;;; may compile a LaTeX document on Java programming. Most of the time you may use
;;; both English dictionary completion and Java word list completion. However, you
;;; may want to compare Java and C++. Naturally, when typing C++ code, you hope to use
;;; C++ word list to reduce both typing workload and typos. It's really possible. The
;;; only thing you need to do is to change major mode by, say, pressing M-x C++-mode
;;; <Enter> or switch back by M-x latex-mode<Enter>. In practice, perhaps, you change
;;; major mode not because of dynamical completion but because you want to enjoy the
;;; syntax highlighting or indentation. Anyway, you enjoy several features
;;; simultaneously.

;;; Beginning of Wang's code segment 2
(defvar dabbrev-wordlist-directory "~/.dabbrev/"
  "This string variable indicates where word list files reside.

As a system administrator, in order to share word list and dictionary files
to all users, you need only to have this variable point to which directory
they live in. Remember that the path must be in UNIX format. For example,
with RedHat Linux, you may add statement like

(setq dabbrev-wordlist-directory
      \"/usr/share/emacs/20.7/.dabbrev/\")
      ; \"//IP-address/path/\" is allowed in LAN environment

to the `/etc/skel/.emacs' file. Be careful the ending / is necessary.")

;;; Different default dictionary for every buffer can be used by adding statement like
;;;
;;;  (setq dabbrev-dictionary "~/.dabbrev/British-English.dab")
;;;                         ; "//IP-address/path/British-English.dab" is allowed.
;;;
;;; to `.emacs' file. Dictionary file must be given relative or absolute path.
(defvar dabbrev-dictionary "American-English.dab")

(defun dabbrev-default-dictionary ()
  (if (string-match "/" dabbrev-dictionary)
      dabbrev-dictionary
    (concat dabbrev-wordlist-directory dabbrev-dictionary)))

;;; Global variable `dabbrev-dictionary-enabled':
;;;   t - Let dictionary completion be available with every buffer.
;;; nil - Dictionary completion is disabled, but word list completion for
;;;       major modes may still be available.
;;; Notice
;;; ------
;;; Whether dictionary completion is enabled or disabled, with any buffer, you can
;;; load dictionary via command `dabbrev-load-dictionary'. By doing so, you can
;;; not only assign a particular dictionary for the document in the buffer, but also
;;; put a higher priority on the use of dictionary completion with the very
;;; document. In orther words, even if dictionary completion is disabled by setting
;;; `dabbrev-dictionary-enabled' to nil, manually loading dictionary with
;;; `dabbrev-load-dictionary' will enable dictionary completion on the very document.
;;; The information about which dictionary to use with a document will be desktop-
;;; saved for next Emacs sessions if desktop-saving is your configuration, and you've
;;; not closed the document manually before ending an Emacs session.
(defvar dabbrev-dictionary-enabled t)

;;; Each buffer has a permanent-local variable `dabbrev-local-dictionary' to
;;; remember which dictionary it uses.
(defvar dabbrev-local-dictionary nil)
(make-variable-buffer-local 'dabbrev-local-dictionary)
(put 'dabbrev-local-dictionary 'permanent-local t)

;;; dictionary can be used by current buffer
;;; If particular dictionary is assgned, return it. Or, return default dictionary
;;; if it's not disabled. Otherwise, return nil.
(defun dabbrev-possible-dictionary ()
  (or (and dabbrev-local-dictionary
	   (file-exists-p dabbrev-local-dictionary) ; file may not exist again
	   (file-readable-p dabbrev-local-dictionary)
	   dabbrev-local-dictionary)
      (and dabbrev-dictionary-enabled
	   (let ((dic (dabbrev-default-dictionary)))
	     (and (file-exists-p dic)
		  (file-readable-p dic)
		  dic)))))

;;; Sometimes, the name of a major mode cannot be used to name your word list file.
;;; For example, c++-mode.dab is an illegal file name under Microsoft DOS. In this
;;; case, you need to define association list 'dabbrev-wordlist-file-mapping in
;;; your `.emacs' file which defines mapping from the major modes to the word list
;;; file names. It looks like
;;;
;;;(setq dabbrev-wordlist-file-mapping
;;;      '((f90-mode . "~/.mydabbrev/fortran-mode.dab") ... ))
;;;
;;; where the Fortran 90 mode, f90-mode, is mapped to a word list file,
;;; ~/.mydabbrev/fortran-mode.dab. It also shows you how to have more major modes
;;; share one word list.
;;;
;;; Please notice that the file name should be given relative or absolute path. If
;;; neither relative nor absolute path is given, the file is supposed to locate in
;;; the directory, `dabbrev-wordlist-directory'.
;;;
;;; That both `dabbrev-dictionary' and `dabbrev-wordlist-file-mapping' require
;;; relative or absolute path will benefit system administrators. For example, the
;;; default word list and dictionary files are stored on a shared directory, say,
;;; /usr/share/emacs/21.2.1/.dabbrev/. In this case, if individual users have their
;;; own favorite word list and dictionary files, they can put them in their local
;;; directories and configure these two variables. Of course, he can also have
;;; `dabbrev-wordlist-directory' point to the directory of his own word list files.
(defvar dabbrev-wordlist-file-mapping nil)

;;; To update info when ending an Emacs session
(add-hook 'desktop-save-hook 'dabbrev-desktop-hooking)

(defun dabbrev-desktop-hooking ()
  ;;; configure Emacs to save desktop if needed
  (when (fboundp 'desktop-read)
    ;;; don't save word lists
    (or (string-match "\\*Jargon" desktop-buffers-not-to-save)
	(setq desktop-buffers-not-to-save
	      (concat desktop-buffers-not-to-save
		      "\\|^\\*Jargon\\*\\|^\\*Dictionary\\*")))
    ;;; remember dictionaries for different buffers
    (or (memq 'dabbrev-local-dictionary desktop-locals-to-save)
	(setq desktop-locals-to-save
	      (append desktop-locals-to-save
		      '(dabbrev-local-dictionary desktop-buffers-not-to-save))))))

;;;###autoload
(defun dabbrev-load-dictionary (&optional arg)
  "Load or change dictionary for current buffer during Emacs session.

This command enables you to assign different dictionary to different
buffer. If you have configured your `.emacs' to save desktop, this kind
of information will be saved automatically for your next sessions.

See also `desktop-save', `desktop-load-default' and `desktop-read'."
  (interactive "*P")
  (let ((default-dic (dabbrev-default-dictionary)) file)
    (setq file (read-file-name
		(concat "Dictionary to be used (default "
			(file-name-nondirectory default-dic) "): ")
		dabbrev-wordlist-directory
		default-dic))
    (if (and (file-exists-p file) (file-readable-p file))
	(let ((old-dic (dabbrev-possible-dictionary)))
	  ;;; Word lists for major modes are small and may suffer frequent switch
	  ;;; back and forth, whereas dictionaries are huge and seldom changed.
	  ;;; Thus, we take an attitude that before loading the new dictionary,
	  ;;; we close the old dictionary connected to current buffer and thus
	  ;;; release its memory, but never close a loaded word list.
	  (if old-dic
	      (or (string= old-dic file)
		  (let (buf-name)
		    (kill-buffer (with-current-buffer    ; kill old dictionary
				     (dabbrev-wordlist-buffer old-dic "*Dictionary*")
				   (setq buf-name (buffer-name))
				   (if (string-match "^\\*Jargon\\*" buf-name)
				       (setq buf-name
					     (dabbrev-make-buffer-name "*Dictionary*")))
				   (current-buffer)))
		    ; load new dictionary and give it the same buffer name
		    (dabbrev-wordlist-buffer file "*Dictionary*" buf-name)))
	    (dabbrev-wordlist-buffer file "*Dictionary*"))  ; load new dictionary
	  (setq dabbrev-local-dictionary file))
      (message "Dictionary %s not found" file))))

(defun dabbrev-make-buffer-name (title)
  "Make buffer name for dictionary and jargon buffers.

TITLE is either \"*Dictionary*\" or \"*Jargon*\". The corresponding output will
be \"*Dictionary*[<n>]\" or \"*Jargon*[<n>]\" as an available buffer name."
  (let (buf-name n)
    (if (not (get-buffer title))
	title
      (setq n 2)
      (while (and (setq buf-name (concat title "<" (int-to-string n) ">"))
		  (get-buffer buf-name))
	(setq n (1+ n)))
      buf-name)))

(defun dabbrev-wordlist-buffer (file title &optional buf-name)
  "Load word list file `FILE' into a buffer named `BUF-NAME'.

Return the buffer named `BUF-NAME'. If `BUF-NAME' is not provided, it will be
generated by `dabbrev-make-buffer-name', using `TITLE', e.g., \"*Jargon*\"."
  (let ((buf-list (buffer-list))
	buffer)
    (setq buffer (find-file-noselect file))
    (or (member buffer buf-list)
        ;;; new *Jargon* or *Dictionary* opened
	(when buffer
	  (if (null buf-name)
	      (setq buf-name (dabbrev-make-buffer-name title)))
          ;;; rename buffer and set read-only property
	  (with-current-buffer buffer
	    (rename-buffer buf-name)
	    (setq buffer-read-only t))))
    buffer))

(defun dabbrev-wordlist-buffers ()
  "Buffers of word lists available for current buffer at this moment.

There are at most two buffers, i.e., *Jargon*[<n>] and *Dictionary*[<n>]."
  (let ((dictionary (dabbrev-possible-dictionary))
	(mapping (assoc major-mode dabbrev-wordlist-file-mapping))
	file buffers)
    (setq buffers
	  (if dictionary
	      (list (dabbrev-wordlist-buffer dictionary "*Dictionary*")))
	  file (expand-file-name
		(if mapping
		    ;;; If the mapping is given a relative/absolute path,
		    ;;; just use it. Or, assume it resides in directory,
		    ;;; `dabbrev-wordlist-directory'.
		    (let ((file (cdr mapping)))
		      (if (string-match "/" file) ; path in UNIX format is given
			  file
			(concat dabbrev-wordlist-directory file)))
		  (concat dabbrev-wordlist-directory
			  (prin1-to-string major-mode) ".dab"))))
    (if (and (file-exists-p file) (file-readable-p file))
	(setq buffers  ;;; *Jargon* has higher search priority than *Dictionary*
	      (cons (dabbrev-wordlist-buffer file "*Jargon*")
		    buffers)))
    buffers))
; End of Wang's code segment 2

;;;###autoload
(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function'.

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]."
  (interactive "*P")
  (let (abbrev record-case-pattern
	       expansion old direction (orig-point (point)))
    ;; abbrev -- the abbrev to expand
    ;; expansion -- the expansion found (eventually) or nil until then
    ;; old -- the text currently in the buffer
    ;;    (the abbrev, or the previously-made expansion)
    (save-excursion
      (if (and (null arg)
	       (markerp dabbrev--last-abbrev-location)
	       (marker-position dabbrev--last-abbrev-location)
	       (or (eq last-command this-command)
		   (and (window-minibuffer-p (selected-window))
			(= dabbrev--last-abbrev-location
			   (point)))))
	  ;; Find a different expansion for the same abbrev as last time.
	  (progn
	    (setq abbrev dabbrev--last-abbreviation)
	    (setq old dabbrev--last-expansion)
	    (setq direction dabbrev--last-direction))
	;; If the user inserts a space after expanding
	;; and then asks to expand again, always fetch the next word.
	(if (and (eq (preceding-char) ?\ )
		 (markerp dabbrev--last-abbrev-location)
		 (marker-position dabbrev--last-abbrev-location)
		 (= (point) (1+ dabbrev--last-abbrev-location)))
	    (progn
	      ;; The "abbrev" to expand is just the space.
	      (setq abbrev " ")
	      (save-excursion
		(if dabbrev--last-buffer
		    (set-buffer dabbrev--last-buffer))
		;; Find the end of the last "expansion" word.
		(if (or (eq dabbrev--last-direction 1)
			(and (eq dabbrev--last-direction 0)
			     (< dabbrev--last-expansion-location (point))))
		    (setq dabbrev--last-expansion-location
			  (+ dabbrev--last-expansion-location
			     (length dabbrev--last-expansion))))
		(goto-char dabbrev--last-expansion-location)
		;; Take the following word, with intermediate separators,
		;; as our expansion this time.
		(re-search-forward
		 (concat "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
		(setq expansion (buffer-substring-no-properties
				 dabbrev--last-expansion-location (point)))

		;; Record the end of this expansion, in case we repeat this.
		(setq dabbrev--last-expansion-location (point)))
	      ;; Indicate that dabbrev--last-expansion-location is
	      ;; at the end of the expansion.
	      (setq dabbrev--last-direction -1))

	  ;; We have a different abbrev to expand.
	  (dabbrev--reset-global-variables)
	  (setq direction (if (null arg)
			      (if dabbrev-backward-only 1 0)
			    (prefix-numeric-value arg)))
	  (setq abbrev (dabbrev--abbrev-at-point))
	  (setq record-case-pattern t)
	  (setq old nil)))

      ;;--------------------------------
      ;; Find the expansion
      ;;--------------------------------
      (or expansion
	  (setq expansion
		(dabbrev--find-expansion abbrev direction
					 (and (if (eq dabbrev-case-fold-search 'case-fold-search)
						  case-fold-search
						dabbrev-case-fold-search)
					      (or (not dabbrev-upcase-means-case-search)
						  (string= abbrev (downcase abbrev))))))
          ;;; Beginning of Wang's code segment 3
	  (and (setq dabbrev--friend-buffer-list (dabbrev-wordlist-buffers))
	       (setq expansion
		     (dabbrev--find-expansion abbrev direction
			  (and (eval dabbrev-case-fold-search)
			       (or (not dabbrev-upcase-means-case-search)
				   (string= abbrev (downcase abbrev)))))))))
          ;;; End of Wang's code segment 3
    (cond
     ((not expansion)
      (dabbrev--reset-global-variables)
      (if old
	  (save-excursion
	    (setq buffer-undo-list (cons orig-point buffer-undo-list))
	    ;; Put back the original abbrev with its original case pattern.
	    (search-backward old)
	    (insert abbrev)
	    (delete-region (point) (+ (point) (length old)))))
      (error "No%s dynamic expansion for `%s' found"
	     (if old " further" "") abbrev))
     (t
      (if (not (or (eq dabbrev--last-buffer dabbrev--last-buffer-found)
		   (minibuffer-window-active-p (selected-window))))
	  (progn
	    (message "Expansion found in '%s'"
		     (buffer-name dabbrev--last-buffer))
	    (setq dabbrev--last-buffer-found dabbrev--last-buffer))
	(message nil))
      (if (and (or (eq (current-buffer) dabbrev--last-buffer)
		   (null dabbrev--last-buffer))
	       (numberp dabbrev--last-expansion-location)
	       (and (> dabbrev--last-expansion-location (point))))
	  (setq dabbrev--last-expansion-location
		(copy-marker dabbrev--last-expansion-location)))
      ;; Success: stick it in and return.
      (setq buffer-undo-list (cons orig-point buffer-undo-list))
      (dabbrev--substitute-expansion old abbrev expansion
				     record-case-pattern)

      ;; Save state for re-expand.
      (setq dabbrev--last-expansion expansion)
      (setq dabbrev--last-abbreviation abbrev)
      (setq dabbrev--last-abbrev-location (point-marker))))))

;;----------------------------------------------------------------
;; Local functions
;;----------------------------------------------------------------

;;; Checks if OTHER-BUFFER has the same major mode as current buffer.
(defun dabbrev--same-major-mode-p (other-buffer)
  (eq major-mode
      (save-excursion
	(set-buffer other-buffer)
	major-mode)))

;;; Back over all abbrev type characters and then moves forward over
;;; all skip characters.
(defun dabbrev--goto-start-of-abbrev ()
  ;; Move backwards over abbrev chars
  (save-match-data
    (if (not (bobp))
	(progn
	  (forward-char -1)
	  (while (and (looking-at dabbrev--abbrev-char-regexp)
		      (not (bobp)))
	    (forward-char -1))
	  (or (looking-at dabbrev--abbrev-char-regexp)
	      (forward-char 1))))
    (and dabbrev-abbrev-skip-leading-regexp
	 (while (looking-at dabbrev-abbrev-skip-leading-regexp)
	   (forward-char 1)))))

;;; Extract the symbol at point to serve as abbreviation.
(defun dabbrev--abbrev-at-point ()
  ;; Check for error
  (if (bobp)
      (error "No possible abbreviation preceding point"))
  ;; Return abbrev at point
  (save-excursion
    ;; Record the end of the abbreviation.
    (setq dabbrev--last-abbrev-location (point))
    ;; If we aren't right after an abbreviation,
    ;; move point back to just after one.
    ;; This is so the user can get successive words
    ;; by typing the punctuation followed by M-/.
    (save-match-data
      (if (save-excursion
	    (forward-char -1)
	    (not (looking-at (concat "\\("
				     (or dabbrev-abbrev-char-regexp
					 "\\sw\\|\\s_")
				     "\\)+"))))
	  (if (re-search-backward (or dabbrev-abbrev-char-regexp
				      "\\sw\\|\\s_")
				  nil t)
	      (forward-char 1)
	    (error "No possible abbreviation preceding point"))))
    ;; Now find the beginning of that one.
    (dabbrev--goto-start-of-abbrev)
    (buffer-substring-no-properties
     dabbrev--last-abbrev-location (point))))

;;; Initializes all global variables
(defun dabbrev--reset-global-variables ()
  ;; dabbrev--last-obarray and dabbrev--last-completion-buffer
  ;; must not be reset here.
  (setq dabbrev--last-table nil
	dabbrev--last-abbreviation nil
	dabbrev--last-abbrev-location nil
	dabbrev--last-direction nil
	dabbrev--last-expansion nil
	dabbrev--last-expansion-location nil
	dabbrev--friend-buffer-list nil
	dabbrev--last-buffer nil
	dabbrev--last-buffer-found nil
	dabbrev--abbrev-char-regexp (or dabbrev-abbrev-char-regexp
					"\\sw\\|\\s_")
	dabbrev--check-other-buffers dabbrev-check-other-buffers))

(defun dabbrev--select-buffers ()
  "Return a list of other buffers to search for a possible abbrev.
The current buffer is not included in the list.

This function makes a list of all the buffers returned by `buffer-list',
then discards buffers whose names match `dabbrev-ignored-buffer-names'
or `dabbrev-ignored-buffer-regexps'.  It also discards buffers for which
`dabbrev-friend-buffer-function', if it is bound, returns nil when called
with the buffer as argument.
It returns the list of the buffers that are not discarded."
  (dabbrev-filter-elements
   buffer (buffer-list)
   (and (not (eq (current-buffer) buffer))
	(not (dabbrev--ignore-buffer-p buffer))
	(boundp 'dabbrev-friend-buffer-function)
	(funcall dabbrev-friend-buffer-function buffer))))

(defun dabbrev--try-find (abbrev reverse n ignore-case)
  "Search for ABBREV, backwards if REVERSE, N times.
If IGNORE-CASE is non-nil, ignore case while searching.
Return the expansion found, and save the location of the start
of the expansion in `dabbrev--last-expansion-location'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((expansion nil))
	(and dabbrev--last-expansion-location
	     (goto-char dabbrev--last-expansion-location))
	(let ((case-fold-search ignore-case)
	      (count n))
	  (while (and (> count 0)
		      (setq expansion (dabbrev--search abbrev
						       reverse
						       ignore-case)))
	    (setq count (1- count))))
	(and expansion
	     (setq dabbrev--last-expansion-location (point)))
	expansion))))

(defun dabbrev--find-all-expansions (abbrev ignore-case)
  "Return a list of all possible expansions of ABBREV.
If IGNORE-CASE is non-nil, accept matches which differ in case."
  (let ((all-expansions nil)
	expansion)
    (save-excursion
      (goto-char (point-min))
      (while (setq expansion (dabbrev--find-expansion abbrev -1 ignore-case))
	(setq all-expansions (cons expansion all-expansions))))
    all-expansions))

(defun dabbrev--scanning-message ()
  (unless (window-minibuffer-p (selected-window))
    (message "Scanning `%s'" (buffer-name (current-buffer)))))

(defun dabbrev--ignore-buffer-p (buffer)
  "Return non-nil if BUFFER should be ignored by dabbrev."
  (let ((bn (buffer-name buffer)))
    (or (member bn dabbrev-ignored-buffer-names)
	(let ((tail dabbrev-ignored-buffer-regexps)
	      (match nil))
	  (while (and tail (not match))
	    (setq match (string-match (car tail) bn)
		  tail (cdr tail)))
	  match))))

(defun dabbrev--find-expansion (abbrev direction ignore-case)
  "Find one occurrence of ABBREV, and return the expansion.
DIRECTION > 0 means look that many times backwards.
DIRECTION < 0 means look that many times forward.
DIRECTION = 0 means try both backward and forward.
IGNORE-CASE non-nil means ignore case when searching.
This sets `dabbrev--last-direction' to 1 or -1 according
to the direction in which the occurrence was actually found.
It sets `dabbrev--last-expansion-location' to the location 
of the start of the occurrence."
  (save-excursion
    ;; If we were scanning something other than the current buffer,
    ;; continue scanning there.
    (when dabbrev--last-buffer
      (set-buffer dabbrev--last-buffer)
      (dabbrev--scanning-message))
    (or
     ;; ------------------------------------------
     ;; Look backward in current buffer.
     ;; ------------------------------------------
     (and (not dabbrev-search-these-buffers-only)
	  (>= direction 0)
	  (setq dabbrev--last-direction (min 1 direction))
	  (dabbrev--try-find abbrev t
			     (max 1 direction)
			     ignore-case))
     ;; ------------------------------------------
     ;; Look forward in current buffer
     ;; or whatever buffer we were last scanning.
     ;; ------------------------------------------
     (and (or (not dabbrev-search-these-buffers-only)
	      dabbrev--last-buffer)
	  (<= direction 0)
	  (setq dabbrev--last-direction -1)
	  (dabbrev--try-find abbrev nil
			     (max 1 (- direction))
			     ignore-case))
     ;; ------------------------------------------
     ;; Look in other buffers.
     ;; Always start at (point-min) and look forward.
     ;; ------------------------------------------
     (progn
       (setq dabbrev--last-direction -1)
       (unless dabbrev--last-buffer
	 ;; If we have just now begun to search other buffers,
	 ;; determine which other buffers we should check.
	 ;; Put that list in dabbrev--friend-buffer-list.
	 (or dabbrev--friend-buffer-list
	     (setq dabbrev--friend-buffer-list
		   (dabbrev--make-friend-buffer-list))))
       ;; Walk through the buffers till we find a match.
       (let (expansion)
	 (while (and (not expansion) dabbrev--friend-buffer-list)
	   (setq dabbrev--last-buffer
		 (car dabbrev--friend-buffer-list))
	   (setq dabbrev--friend-buffer-list
		 (cdr dabbrev--friend-buffer-list))
	   (set-buffer dabbrev--last-buffer)
	   (dabbrev--scanning-message)
	   (setq dabbrev--last-expansion-location (point-min))
	   (setq expansion (dabbrev--try-find abbrev nil 1 ignore-case)))
	 expansion)))))

;; Compute the list of buffers to scan.
;; If dabbrev-search-these-buffers-only, then the current buffer
;; is included in this list if it should be searched.
;; Otherwise, the current buffer is searched first specially.,
;; and it is not included in this list.
(defun dabbrev--make-friend-buffer-list ()
  (let ((list (mapcar (function get-buffer)
		      dabbrev-search-these-buffers-only)))
    (when (and (null dabbrev-search-these-buffers-only)
	       dabbrev--check-other-buffers
	       (or (eq dabbrev--check-other-buffers t)
		   (setq dabbrev--check-other-buffers
			 (y-or-n-p "Scan other buffers also? "))))
      (setq list (funcall dabbrev-select-buffers-function))
      ;; If dabbrev-check-all-buffers, tack on all the other
      ;; buffers at the end of the list, except those which are
      ;; specifically to be ignored.
      (if dabbrev-check-all-buffers
	  (setq list
		(append list
			(dabbrev-filter-elements
			 buffer (buffer-list)
			 (and (not (memq buffer list))
			      (not (dabbrev--ignore-buffer-p buffer)))))))
      ;; Remove the current buffer.
      (setq list (delq (current-buffer) list)))
    ;; Move buffers in the list that are visible on the screen
    ;; to the front of the list, but don't add anything to the list.
    (if list
	(walk-windows (lambda (w)
			(unless (eq w (selected-window))
			  (if (memq (window-buffer w) list)
			      (setq list
				    (cons (window-buffer w)
					  (delq (window-buffer w)
						list))))))))
    ;; In a minibuffer, search the buffer it was activated from,
    ;; first after the minibuffer itself.  Unless we aren't supposed
    ;; to search the current buffer either.
    (if (and (window-minibuffer-p (selected-window))
	     (not dabbrev-search-these-buffers-only))
	(setq list
	      (cons (dabbrev--minibuffer-origin)
		    (delq (dabbrev--minibuffer-origin) list))))

    ;;; Beginning of Wang's code segment 4
    ;;; Don't search *Jargon*s and *Dictionary*s. You'll be told to do so via
    ;;; directly provided `dabbrev--friend-buffer-list'.
    (or (eq this-command 'dabbrev-completion)
	(setq list
	      (dabbrev-filter-elements
	       buffer
	       list
	       (not (string-match "^\\*Jargon\\*\\|^\\*Dictionary\\*"
				  (buffer-name buffer))))))
    ;;; End of Wang's code segment 4
    list))

(defun dabbrev--safe-replace-match (string &optional fixedcase literal)
  (if (eq major-mode 'picture-mode)
      (picture-replace-match string fixedcase literal)
    (replace-match string fixedcase literal)))

;;;----------------------------------------------------------------
(defun dabbrev--substitute-expansion (old abbrev expansion record-case-pattern)
  "Replace OLD with EXPANSION in the buffer.
OLD is text currently in the buffer, perhaps the abbreviation
or perhaps another expansion that was tried previously.
ABBREV is the abbreviation we are expanding.
It is \" \" if we are copying subsequent words.
EXPANSION is the expansion substring to be used this time.
RECORD-CASE-PATTERN, if non-nil, means set `dabbrev--last-case-pattern'
to record whether we upcased the expansion, downcased it, or did neither."
  ;;(undo-boundary)
  (let ((use-case-replace (and (if (eq dabbrev-case-fold-search 'case-fold-search)
				   case-fold-search
				 dabbrev-case-fold-search)
			       (or (not dabbrev-upcase-means-case-search)
				   (string= abbrev (downcase abbrev)))
			       (if (eq dabbrev-case-replace 'case-replace)
				   case-replace
				 dabbrev-case-replace))))

    ;; If we upcased or downcased the original expansion,
    ;; do likewise for the subsequent words when we copy them.
    ;; Don't do any of the usual case processing, though.
    (when (equal abbrev " ")
      (if dabbrev--last-case-pattern
	  (setq expansion
		(funcall dabbrev--last-case-pattern expansion)))
      (setq use-case-replace nil))

    ;; If the expansion has mixed case
    ;; and it is not simply a capitalized word,
    ;; or if the abbrev has mixed case,
    ;; and if the given abbrev's case pattern
    ;; matches the start of the expansion,
    ;; copy the expansion's case
    ;; instead of downcasing all the rest.
    ;; Treat a one-capital-letter abbrev as "not all upper case",
    ;; so as to force preservation of the expansion's pattern
    ;; if the expansion starts with a capital letter.
    (let ((expansion-rest (substring expansion 1)))
      (if (and (not (and (or (string= expansion-rest (downcase expansion-rest))
			     (string= expansion-rest (upcase expansion-rest)))
			 (or (string= abbrev (downcase abbrev))
			     (and (string= abbrev (upcase abbrev))
				  (> (length abbrev) 1)))))
	       (string= abbrev
			(substring expansion 0 (length abbrev))))
	  (setq use-case-replace nil)))

    ;; If the abbrev and the expansion are both all-lower-case
    ;; then don't do any conversion.  The conversion would be a no-op
    ;; for this replacement, but it would carry forward to subsequent words.
    ;; The goal of this is to preven that carrying forward.
    (if (and (string= expansion (downcase expansion))
	     (string= abbrev (downcase abbrev)))
	(setq use-case-replace nil))

    (if use-case-replace
	(setq expansion (downcase expansion)))

    ;; In case we insert subsequent words,
    ;; record if we upcased or downcased the first word,
    ;; in order to do likewise for subsequent words.
    (and record-case-pattern
	 (setq dabbrev--last-case-pattern 
	       (and use-case-replace
		    (cond ((equal abbrev (upcase abbrev)) 'upcase)
			  ((equal abbrev (downcase abbrev)) 'downcase)))))

    ;; Convert newlines to spaces.
    (if dabbrev--eliminate-newlines
	(while (string-match "\n" expansion)
	  (setq expansion (replace-match " " nil nil expansion))))

    (if old
	(save-excursion
	  (search-backward old))
      ;;(set-match-data (list (point-marker) (point-marker)))
      (search-backward abbrev)
      (search-forward abbrev))

    ;; Make case of replacement conform to case of abbreviation
    ;; provided (1) that kind of thing is enabled in this buffer
    ;; and (2) the replacement itself is all lower case.
    (dabbrev--safe-replace-match expansion
				 (not use-case-replace)
				 t)))


;;;----------------------------------------------------------------
;;; Search function used by dabbrevs library.


(defun dabbrev--search (abbrev reverse ignore-case)
  "Search for something that could be used to expand ABBREV.

Second arg, REVERSE, is t for reverse search, nil for forward.
The variable `dabbrev-limit' controls the maximum search region size.
Third argument IGNORE-CASE non-nil means treat case as insignificant while
looking for a match and when comparing with previous matches.  Also if
that's non-nil and the match is found at the beginning of a sentence
and is in lower case except for the initial then it is converted to
all lower case for return.

Table of expansions already seen is examined in buffer
`dabbrev--last-table' so that only distinct possibilities are found
by dabbrev-re-expand.

Returns the expansion found, or nil if not found.
Leaves point at the location of the start of the expansion."
  (save-match-data
    (let ((pattern1 (concat (regexp-quote abbrev)
			    "\\(" dabbrev--abbrev-char-regexp "\\)"))
	  (pattern2 (concat (regexp-quote abbrev)
			   "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
	  (found-string nil))
      ;; Limited search.
      (save-restriction
	(and dabbrev-limit
	     (narrow-to-region dabbrev--last-expansion-location
			       (+ (point)
				  (if reverse (- dabbrev-limit) dabbrev-limit))))
	;;--------------------------------
	;; Look for a distinct expansion, using dabbrev--last-table.
	;;--------------------------------
	(while (and (not found-string)
		    (if reverse
			(re-search-backward pattern1 nil t)
		      (re-search-forward pattern1 nil t)))
	  (goto-char (match-beginning 0))
	  ;; In case we matched in the middle of a word,
	  ;; back up to start of word and verify we still match.
	  (dabbrev--goto-start-of-abbrev)

	  (if (not (looking-at pattern1))
	      nil
	    ;; We have a truly valid match.  Find the end.
	    (re-search-forward pattern2)
	    (setq found-string (buffer-substring-no-properties
				(match-beginning 1) (match-end 1)))
	    (and ignore-case (setq found-string (downcase found-string)))
	    ;; Ignore this match if it's already in the table.
	    (if (dabbrev-filter-elements
		 table-string dabbrev--last-table
		 (string= found-string table-string))
		(setq found-string nil)))
	  ;; Prepare to continue searching.
	  (if reverse
	      (goto-char (match-beginning 0))
	    (goto-char (match-end 0))))
	;; If we found something, use it.
	(if found-string
	    ;; Put it into `dabbrev--last-table'
	    ;; and return it (either downcased, or as is).
	    (let ((result (buffer-substring-no-properties
			   (match-beginning 0) (match-end 0)))
		  start) ; Wang
	      ;;; Beginning od Wang's code segment 5
	      ;;; If match in *Jargon* buffer and at the beginning of a line,
	      ;;; take the whole line as expansion. This solution helps a lot
	      ;;; when people put built-in subroutines' syntax info of a
	      ;;; programming language into its word list file.
	      ;;;
	      ;;; Warning: In the word list files, different items must reside
	      ;;; on different lines.
	      ;;; July 10, 2002
	      (when (string-match "*Jargon" (prin1-to-string dabbrev--last-buffer))
		(setq start (point))
		(beginning-of-line)
		(if (eq start (+ (point) (length result)))
		    (progn
		      (setq start (point))
		      (end-of-line)
		      (setq result (buffer-substring-no-properties start (point))))
		  (if (> start (+ (point) (length result) 1)) ; allow item: \begin
		      (setq found-string nil))))
	      (when found-string
	      ;;; End of Wang's code segment 5
		(setq dabbrev--last-table
		      (cons found-string dabbrev--last-table))
		result)))))))

(mapcar (lambda (msg)
	  (add-to-list 'debug-ignored-errors msg))
	'("^No dynamic expansion for .* found$"
	  "^No further dynamic expansion for .* found$"
	  "^No possible abbreviation preceding point$"))

(provide 'dabbrev)

;;; dabbrev.el ends here
