PAPER=paper

all: paper.pdf

compiled/paper_scrbl.zo: *.rkt *.scrbl
	raco make -v $(PAPER).scrbl

paper.pdf: pkg compiled/paper_scrbl.zo texstyle.tex
	scribble ++extra fig-stats.tex ++extra fig-staging.tex ++style texstyle.tex --pdf $(PAPER).scrbl

paper.tex: pkg compiled/paper_scrbl.zo texstyle.tex
	scribble ++style texstyle.tex --latex $(PAPER).scrbl

pkg:
	raco pkg install --skip-installed trivial

teaser:
	xelatex teaser.tex

clean:
	rm -r compiled

