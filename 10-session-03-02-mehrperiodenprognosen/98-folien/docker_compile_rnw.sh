#!/bin/bash

set -x  # Log all commands

cd "/home/rstudio/workspace/10-session-03-02-mehrperiodenprognosen/98-folien" || exit

pwd  # Check the current directory

rm -f *.aux *.bbl *.blg *.log *.toc

Rscript -e "library(knitr); knit('hfwu_za_sose_26_03_02_mehrperiodenprognosen.Rnw')" 2>&1 | tee knit.log

# Explicit path to ensure pdflatex finds the file
lualatex hfwu_za_sose_26_03_02_mehrperiodenprognosen.tex
lualatex hfwu_za_sose_26_03_02_mehrperiodenprognosen.tex

export BIBINPUTS="/home/rstudio/workspace/98-literatur"
# bibtex hfwu_za_sose_26_03_02_mehrperiodenprognosen
biber hfwu_za_sose_26_03_02_mehrperiodenprognosen

lualatex hfwu_za_sose_26_03_02_mehrperiodenprognosen.tex
lualatex hfwu_za_sose_26_03_02_mehrperiodenprognosen.tex

read -p "Press Enter to continue..."
