output = output
rnwfile = popularityPrediction

all:
		R CMD Sweave $(rnwfile).Rnw
		-mkdir -p $(output)
		-cp *.sty $(output)
		-mv *.tex *.pdf *.eps $(output)
		cd $(output); pdflatex $(rnwfile).tex

tex:
		cd $(output); pdflatex $(rnwfile).tex

clean:
		-rm $(output)/*
