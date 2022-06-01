#!/usr/bin/env Rscript

install.packages('https://cran.r-project.org/src/contrib/Archive/SDMTools/SDMTools_1.1-221.2.tar.gz', repos=NULL, type='source');
if ( ! library('SDMTools', character.only=TRUE, logical.return=TRUE) ) {
        quit(status=1, save='no')
}