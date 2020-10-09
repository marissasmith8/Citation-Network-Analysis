FOR /f "usebackq" %%x IN (`Rscript -e "Rcpp:::CxxFlags()"`) DO SET PKG_CXXFLAGS=%%x
R CMD SHLIB R/biSBMR/biSBMWin_R.cpp