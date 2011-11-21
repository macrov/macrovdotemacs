--------------------
WORD LIST COMPLETION
--------------------

If you have experience of using package `dabbrev' (dynamic abbreviation),
I guess you would feel it is really a very nice feature of the great Emacs.
Searching all the buffers for the dynamic completion, it saves lots of your
time by providing you with opportunity just to press M-/ and C-/ but not to
repetitively type the same long strings.

In that way, how about when you are developing a new and/or small
project? The context you have built is so small, if any, that it can
not help a lot. However, when you are in a project developed in C, C++,
Java, TeX, HTML, Perl, Tcl or any other specific language, your typing
at some extent is bounded within the word list of that language. By
word, I mean any word or string native to a language. It may be
preserved keywords, such as `for', `while' and `continue' in C/C++/Java.
Furthermore, it may also be the name of a built-in function, data
structure, method, data member et al. To a modern object-oriented
programming language, the latter is in a huge volume. To remember the
exact spelling of so many built-in words is really boring and simply
impossible.

Again, how about writing an English essay? The language has a tremendous
volume of vocabularies, and you can always use a very small subset of
it. When typing, it's often the case you forget or are not sure how
to spell the whole word but do remember its first letters. I guess
everyone hope that, in this case, the computer editor is able to help
you fill the rest part of the word. In other situations, for some words
of great length, for example, telecommunication, you feel it boring to
type so long a word. Indeed, after typing `telecom', the rest is so
obvious that the computer should know and do our favor. Okay. The
improved dynamical abbreviation/expansion/completion feature is now for
you. If you provide Emacs with a dictionary, a special kind of word lists,
say, American-English.dab, it works just in the way you wish. For example,
after `teleco', pressing M-/ will bring a completion to you. If it is not
the word or word form you want, press M-/ once again... (Try to feel C-/
also.) You will get it unless the word list has not the very word or word
form you mean. Of course, the more the first letters of a word you input,
the faster you pick up the rest.

We name any kind of the completion above as WORD LIST COMPLETION. In
particular, we'll call those based on dictionary as DICTIONARY COMPLETIONS
and the others, native to an individual major mode, as JARGON COMPLETIONS.

The above considerations drove me to extend the function of package
`dabbrev' in my Spring Break, 2001. My work aimed to pre-build some word
list files. When dynamic abbreviation fails to make completion, it has
Emacs open the very word list according to the current major mode of the
document you are editing (Note: This is the final solution from Richard
Stallman. My original design used the extension of a file name to determine
the word list.), and search the word list for expansion. Thanks to lots of
people who are so altruistic as to make their work accessible for the public.
Their editing and sorting work enables me to build several word lists in a
short time. In the downloaded package, I include word lists for American
English, TeX/LaTeX, C, C++, Emacs List, HTML, Java, Perl and so on.

SPECIAL NOTICE: Most of editors available provide function like spell
checking. It works on what you have input rather than what you try to input.
Few implement the feature we have described.


-----------------------------
HOW TO BUILD YOUR WORD LISTS?
-----------------------------

- WORD LIST FILES: .DAB

Word list is not a good term. A word list file consists mainly of a group
of words or phrases native to the special kind of document you are editing.
In practice, a word list file aims to provide Emacs with pre-built CONTEXT
which is absolutely not limited to syntactical words or phrases. For
example, FORM="FORMATTED" and FORM="UNFORMATTED" are not grammatical
objects, but both can be useful `phrases' of our word lists for fortran-mode.
Just imagine that pressing M-/ after FORM=" will finish the rest for you!

Dictionary is a special kind of word lists. It has no structural requirement. 
You just put every word and phrase in any way you like.

However, other kind of word list file has structural requirement. You need
to put every word and phrase native to your major mode on a separate line.
The 'phrase' is a broad concept. You can take the built-in subroutines'
syntax as a special phrase. Such as, MPI function MPI_SEND:

MPI_SEND(buf, count, datatype, dest, tag, comm) 

Thus, for example, the content of word list file for fortran-mode looks like:

FORM="FORMATTED"
FORM="UNFORMATTED"
ACCESS="SEQUENTIAL"
ACCESS="DIRECT"
STATUS="OLD"
STATUS="NEW"
OPEN(UNIT=, FILE="", ACCESS="", STATUS="", FORM="")
.EQV.
.FALSE.
.GE.
.GT.
...
MPI_WAIT(request, status) 
MPI_WAITALL( count, array_of_requests, array_of_statuses) 
MPI_WTICK()

This structure has a very nice feature. That is, when an expansion is found
at the beginning of a line, dynamical expansion will take the whole line as
the expansion. It's easy to see that expansion like MPI_WAIT(request, status)
can provide programmer additional information about the very subroutine.

Please notice that word list files are normally put in a directory pointed
by `dabbrev-wordlist-directory'. Of course, if you want to have more
control over where to put your word list files, you can either change the
default value of this variable or define the location of your word list
files through association list `dabbrev-wordlist-file-mapping'.

By the way, Emacs first tries to finish completion in the normal documents
you opened. If it fails, it tries to find completion automatically in the
special word list assigned to your current major mode. If it fails again,
it turns to the general dictionary for completion. This is a reasonable
order according to the recency principle.

Normally, a word list file is named as <major-mode>.dab. But this may not
work occasionally. For example, c++-mode.dab is an illegal file name on
MS-DOS. In this case, you need to define the mapping between the major
modes and the word list files in your `.emacs' file in the following way

(setq dabbrev-wordlist-file-mapping
      '((f90-mode . "~/.dabbrev/fortran-mode.dab") ... ))

where f90-mode is mapped to word list file ~/.dabbrev/fortran-mode.dab. It also
shows you how to have more major modes share one word list. Please notice that
the file name should be given relative or absolute path. If neither relative
nor absolute path is given, the word list file is supposed to reside in the
shared directory, `dabbrev-wordlist-directory'.

Notice: http://ficus-www.cs.ucla.edu/geoff/ispell-dictionaries.html is an
important resource of dictionaries for various languages.


-----------------------------
HOW TO INSTALL AND CONFIGURE?
-----------------------------

First, unzip dabbrev.tar.gz in your home directory.

Then, open the configuration file, `.emacs', in your home directory and add
statement like

    (load-file "~/.dabbrev/dabbrev.elc")

Of course, if you are system adminstrator, you can move ~/.dabbrev/dabbrev.elc
into some directory covered by `load-path'. Then, you just need to add

    (require 'dabbrev)

to the `.emacs' file under your home directory. (If you use Xemacs, you need
to start Xemacs to byte-compile `dabbrev.el' so as to generate a `dabbrev.elc'
for Xemacs.)

If you want to use the dictionary of another language rather than the
default one, American English, you should provide your dictionaries right
under directory, ~/.dabbrev/, and name them as British-English.dab.
Besides, you need to put statement like

    (setq dabbrev-dictionary "~/.dabbrev/British-English.dab")

in your `.emacs' file. If no path is given, the dictionary is supposed to live
in directory define by `dabbrev-wordlist-directory'.

You can only use one dictionary at a time, but you can change dictionary during
the session of Emacs by pressing M-x dabbrev-load-dictionary<Enter> and then
selecting the new dictionary.

In many cases, for example, you edit TeX/LaTeX document and want to use not
only the special word list, or jargon, of TeX/LaTeX, but also the dictionary
of certain human language. In this case, you need not to do anything since
that is the default configuration of `dabbrev' package. On the other hand,
you can forbid/disable dictionary completion by adding statement

    (setq dabbrev-dictionary-enabled nil)

to your `.emacs' file. It must appear after (require 'dabbrev).

Notice
------
Whether dictionary completion is enabled or disabled, with any buffer, you can
load dictionary via command `dabbrev-load-dictionary'. By doing so, you can
not only assign a particular dictionary for the document in the buffer, but also
put a `higher' priority on the use of dictionary completion with the very
document. In other words, even if dictionary completion is generally disabled by
setting `dabbrev-dictionary-enabled' to nil, manually loading dictionary with
`dabbrev-load-dictionary' will enable dictionary completion on the very document.
The information about which dictionary to use with a document will be desktop-
saved for next Emacs sessions if desktop-saving is your configuration, and you've
not closed the document manually before ending an Emacs session.

As for how to configure desktop-saving, the following code segment works if you
add them to your `.emacs' file.

;;; activate desktop-save if needed
(require 'desktop)
(or (file-exists-p (if (string-match "XEmacs\\|Lucid" (version))
		       "~/emacs.dsk"
		     "~/.emacs.desktop"))
    (desktop-save "~/"))
;;; restore desktop of last session
(desktop-load-default)
(desktop-read)


-------------------------
FOR SYSTEM ADMINISTRATORS
-------------------------

In practice, we have system administrators in our mind when implementing
`dabbrev' package. As a system administrator, you can plan to put word list
and dictionary files in a directory shared by all users. This of course can
save much of the storage. It's your duty to configure the string variable
`dabbrev-wordlist-directory', i.e., to have it point to wherever you copy
the shared word list and dictionary files. Remember that the path must be
in UNIX format.

For example, with RedHat Linux, you can add statement like

   (setq dabbrev-wordlist-directory "/usr/share/emacs/20.7/.dabbrev/")

to the `/etc/skel/.emacs' file. Please be careful that the last / in
"/usr/share/emacs/20.7/.dabbrev/" is necessary. Of course, you can simply
modify the default value of `dabbrev-wordlist-directory' in Emacs Lisp
source file, <emacs-install-directory>/lisp/dabbrev.el for GNU Emacs or
<xemacs-install-directory>/xemacs-packages/lisp/edit-utils/dabbrev.el for
Xemacs, and byte-compile it again.

You are allowed to share word list files in a LAN environment by setting
the above variable like

   (setq dabbrev-wordlist-directory "//129.119.144.38/share/.dabbrev/")

where the final / is required.



FOR YOUR CONVENIENCE, A SAMPLE CONFIGURATION `.EMACS' FILE IS ALREADY
THERE IN ~/.DABBREV/. YOU NEED TO COPY IT TO YOUR HOME DIRECTORY IN ORDER
TO HAVE IT WORK.


Any comments and bug reports are welcome. Thanks.


Zhongxiao (David) Wang
zwang@mail.smu.edu
03/15/2001 on Spring Break
Modified on 05/26/2002
