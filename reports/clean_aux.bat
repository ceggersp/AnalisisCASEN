@echo off
REM Clean LaTeX auxiliary files from the reports folder
REM Keeps only .tex and .pdf files

echo Cleaning LaTeX auxiliary files...

del /q *.aux 2>nul
del /q *.log 2>nul
del /q *.out 2>nul
del /q *.toc 2>nul
del /q *.lof 2>nul
del /q *.lot 2>nul
del /q *.bbl 2>nul
del /q *.blg 2>nul
del /q *.fls 2>nul
del /q *.fdb_latexmk 2>nul
del /q *.synctex.gz 2>nul
del /q *.nav 2>nul
del /q *.snm 2>nul
del /q *.vrb 2>nul
del /q *.run.xml 2>nul
del /q *.bcf 2>nul
del /q *.idx 2>nul
del /q *.ind 2>nul
del /q *.ilg 2>nul
del /q *.glo 2>nul
del /q *.gls 2>nul
del /q *.glg 2>nul
del /q *.xdy 2>nul

echo Done! Only .tex and .pdf files remain.
pause
