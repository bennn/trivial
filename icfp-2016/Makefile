PAPER=pearl

all: pearl.pdf

compiled/pearl_scrbl.zo: *.rkt *.scrbl
	raco make -v $(PAPER).scrbl

pearl.pdf: pkg compiled/pearl_scrbl.zo texstyle.tex
	scribble ++extra fig-experience.tex ++extra fig-stxclass.tex ++extra fig-stats.tex ++style texstyle.tex --pdf $(PAPER).scrbl

pearl.tex: pkg compiled/pearl_scrbl.zo texstyle.tex
	scribble ++style texstyle.tex --latex $(PAPER).scrbl

pkg:
	raco pkg install --skip-installed trivial

teaser:
	xelatex teaser.tex

clean:
	rm -r compiled

