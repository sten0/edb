# Makefile for btxdb
# Makefile,v 1.18 1993/11/20 22:22:53 ohl Exp

VERSION = 0.6

prefix = $(HOME)

ELISP_DIR = $(prefix)/emacs
INFO_DIR = $(prefix)/info
FMT_DIR = $(prefix)/tex/bibtex/fmt

INSTALL = install
INSTALL_DATA = install -m 644

# Emacs Lisp byte-compiler.  Make sure that it can find EDB.
EMACS = emacs
EFLAGS = -l default.el
EC = $(EMACS) -batch $(EFLAGS) -f batch-byte-compile

CVSTAG = BTXDB_`echo $(VERSION) | sed s/[.]/_/g`
EXPORT_FLAG = -r $(CVSTAG)

SUPPORT = README INSTALL NEWS Makefile TODO ChangeLog \
	btxdb.texi btxdb.info examples/bibtex.bib
ELISP_FILES = btxdb.el
FMT_FILES = article.fmt bibtex.fmt book.fmt booklet.fmt conference.fmt \
	inbook.fmt incollection.fmt inproceedings.fmt manual.fmt \
	mastersthesis.fmt misc.fmt phdthesis.fmt proceedings.fmt \
	techreport.fmt unpublished.fmt
JOURNAL_FILES = journals.dat journals.dba journals.fmt \
	journals-full.rep journals-abbrev.rep

DIST_FILES = $(SUPPORT) $(ELISP_FILES) $(FMT_FILES) $(JOURNAL_FILES)
DIST_DIR = btxdb-$(VERSION)
TAR_FILE = btxdb-$(VERSION).tar.Z
SHAR_FILE = btxdb-$(VERSION).shar

TMP_FILE = /tmp/btxdb.tmp

all: btxdb.elc journals.el btxdb.info btxdb.dvi

install: btxdb.elc btxdb.info $(FMT_FILES)
	$(INSTALL_DATA) btxdb.elc $(ELISP_DIR)
	$(INSTALL_DATA) $(FMT_FILES) $(FMT_DIR)
	$(INSTALL_DATA) btxdb.info $(INFO_DIR)

btxdb.elc: btxdb.el
	$(EC) btxdb.el

INSTALL: btxdb.info
	cat -v btxdb.info | sed -n '/^Installation$$/,/^^_$$/p' \
	  | sed '$$d' > $@

NEWS: btxdb.info
	cat -v btxdb.info | sed -n '/^News$$/,/^^_$$/p' | sed '$$d' > $@

btxdb.info: btxdb.texi
	makeinfo btxdb.texi

btxdb.dvi: btxdb.texi
	tex btxdb.texi
	texindex btxdb.cp btxdb.fn btxdb.ky btxdb.pg btxdb.tp btxdb.vr
	tex btxdb.texi
	tex btxdb.texi

journals.el: journals.dat
	echo '(db-find-file "journals.dat") (btxdb-update-journals)' \
	 > $(TMP_FILE)
	emacs -batch $(EFLAGS) -l $(TMP_FILE)
	rm -fr $(TMP_FILE)

clean:
	rm -f *~ *.cp *.fn *.ky *.pg *.tp *.vr *.cps *.fns *.kys *.pgs \
		*.tps *.vrs *.dvi *.log *.aux *.toc

veryclean: clean
	rm -f *.elc journals.el	journals-full.bib journals-abbrev.bib \
		btxdb.info $(TAR_FILE) $(SHAR_FILE) INSTALL NEWS

commit:
	cvs commit -m 'freezing version $(VERSION)'
	cvs tag $(CVSTAG)

tar: $(TAR_FILE)
shar: $(SHAR_FILE)
ftp: /home/ftp/pub/$(TAR_FILE)
/home/ftp/pub/$(TAR_FILE): $(TAR_FILE)
	cp $(TAR_FILE) $@

$(TAR_FILE): $(DIST_FILES)
	rm -fr $(DIST_DIR)
	cvs export -d $(DIST_DIR) $(EXPORT_FLAG) btxdb
	tar cvf - $(DIST_DIR) | compress > $(TAR_FILE)
	rm -fr $(DIST_DIR)

$(SHAR_FILE): $(DIST_FILES)
	shar -pX $(DIST_FILES) > $(SHAR_FILE)

