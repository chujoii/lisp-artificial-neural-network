WWW_PATH=/var/www/localhost

all : pdf

# generate pdf from TeX
#
# for correct generate "tableofcontents" and "ref"
# you need run pdflatex twice (or fourfold)
pdf :
	git describe --always master > revision.txt
	pdflatex algorithm.tex
	bibtex algorithm
	pdflatex algorithm.tex

# clean directory
clean :
	rm *aux *bbl *log *toc *tdo *out *lg *idx *idv *dvi *pdf revision.txt
