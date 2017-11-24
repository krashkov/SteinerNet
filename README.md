# SteinerNet v2
Steiner Tree Approach for Graph Analysis

This library is made in R programming language. 

This version of SteirNet (v2) works with igraph.

### Older versions
older versions of SteirNet upto (v1.3) works with igraph0.

## Installation
For using this library these libraries are needed to included:
### RBGL http://www.bioconductor.org/packages/release/bioc/html/RBGL.html

For that run: 

source("https://bioconductor.org/biocLite.R")

biocLite("RBGL")

### igraph  https://cran.r-project.org/web/packages/igraph/index.html

install.packages("igraph")

##### igraph0 for versions upto 1.7 

download and manually install the last version from here https://cran.r-project.org/src/contrib/Archive/igraph0/igraph0_0.5.7.tar.gz

### limma http://bioconductor.org/packages/release/bioc/html/limma.html

For that run: 

source("https://bioconductor.org/biocLite.R")

biocLite("limma")

## Version History on Cran
https://cran.r-project.org/src/contrib/Archive/SteinerNet/

# News
## 2017 Nov 
Due to number of requests to work with newer version of R which are based on igraph instead of igraph0
I update the code of SteinerNet 


# Citation
To use this code in your research please cite this the article related to this package:

Steiner tree methods for optimal sub-network identification: an empirical study

Afshin Sadeghi and Holger Fröhlich
BMC Bioinformatics 2013 14:144
https://doi.org/10.1186/1471-2105-14-144


