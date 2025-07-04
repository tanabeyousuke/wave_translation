TARGET = kadai
TEX_ENGINE = uplatex
PDF_VIEWER = evince

.PHONY: all clean view

all: $(TARGET).pdf

$(TARGET).pdf: $(TARGET).tex
	$(TEX_ENGINE) $<
	dvipdfmx $(TARGET).dvi

view: $(TARGET).pdf
	$(PDF_VIEWER) $(TARGET).pdf &

clean:
	rm -f *.aux *.log *.dvi *.pdf *.toc *.nav *.snm *.out
