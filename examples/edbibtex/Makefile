# Makefile for edbibtex 0.4

# The basename of your bibliographic database
BASENAME = bibliography

# The directory that contains your bibliographic database
BIBDIR = /home/mike/txt/water

# The directory in which you would like edbibtex to be installed
BASEDIR = /usr/lib/emacs/site-lisp

# The directory in which you would like your format files to be installed
FORMATDIR = $(BASEDIR)/formats

# The directory in which you would like your documentation to be installed
INFODIR = /usr/info

##############################################################
# You probably won't want to change anything after this line #
##############################################################

FORMATS = article.fmt bibtex.fmt book.fmt booklet.fmt \
	inbook.fmt incollection.fmt inproceedings.fmt manual.fmt \
	mastersthesis.fmt misc.fmt phdthesis.fmt preamble.fmt proceedings.fmt \
	string.fmt techreport.fmt unpublished.fmt

DOCUMENTATION = edbibtex.texinfo

CODE = edbibtex.el bibtex.dba compile.el Makefile

ALL = $(FORMATS) $(DOCUMENTATION) $(CODE)

all : edbibtex.elc edbibtex.info

install : 
	cp bibtex.fmt $(FORMATDIR)/$(BASENAME).fmt
	mv $(FORMATS) $(FORMATDIR)
	mv edbibtex.info $(INFODIR)
	cp bibtex.dba $(BIBDIR)/$(BASENAME).dba
	mv edbibtex.elc bibtex.dba $(BASEDIR)

clean : 
	rm $(ALL)

edbibtex.elc : edbibtex.el
	emacs -batch -l compile.el

edbibtex.info : edbibtex.texinfo
	makeinfo edbibtex.texinfo

edbibtex.tgz : $(ALL)
	tar -cvzf edbibtex.tgz --directory .. \
	--exclude RCS --exclude junk edbibtex

edbibtex.uue : edbibtex.tgz
	uuencode edbibtex.tgz edbibtex.tgz > edbibtex.uue




