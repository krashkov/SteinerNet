# SteinerNet

![](http://www.r-pkg.org/badges/version-ago/SteinerNet)
![](http://cranlogs.r-pkg.org/badges/grand-total/SteinerNet)


The Steiner tree problem on unweighted graphs seeks a minimum subtree (i.e. subtree with minimal number of edges), containing a given subset of the vertices (terminals). This problem is NP-complete. This package provides several heuristic and one exact approach for finding Steiner trees, as well as tools for analyzing resultant trees and comparing different algorithms.

This R package was originally applied to analyzing biological networks. For more information about algorithms and their application, see [original article](https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-144) and [tutorial](https://github.com/krashkov/SteinerNet/blob/master/vignettes/tutorial.pdf).



# Installation

The installation process may differ to different versions of package. See information below.

### Version 3.0.0

This is the current version of the package. It only requires installation of `igraph` package.

    install.packages("SteinerNet")

### Version 2.0

To install older versions of packages from CRAN, you need `install_version()` from `devtools` package. SteinerNet v2.0 works with `igraph`, `limma` and `RBGL` packages.

    install.packages("igraph")

    source("https://bioconductor.org/biocLite.R")
    biocLite("RBGL")

    source("https://bioconductor.org/biocLite.R")
    biocLite("limma")

    install_version("SteinerNet", version = "2.0")

### Versions before 2.0

The installation process of older versions is similar to previous, but note, that they works with [`igraph0` package](https://cran.r-project.org/src/contrib/Archive/igraph0).

    install_version("igraph0", version = "0.5.7")

    source("https://bioconductor.org/biocLite.R")
    biocLite("RBGL")

    source("https://bioconductor.org/biocLite.R")
    biocLite("limma")

    install_version("SteinerNet", version = "1.x")

### Version history on Cran

https://cran.r-project.org/src/contrib/Archive/SteinerNet/



# News

### August 2018

SteinerNet v3.0.1 release.

* If vertices of input graph of `steinertree` function has a `name` attribute, vertices of the output graph won't have `realname` attribute. The original names will be still at `name` attribute.

### June 2018

SteinerNet v3.0.0 release. Main changes:

* Critical bugs was fixed.  A list may be provided on demand.
* Structure of files and functions was refreshed
* **Backward incompatible changes were made** (behavior of some functions and list of passed arguments was slightly changed). On the one hand, it makes the work with library simpler and more intuitive, but on the other hand, the workflow is changed too. So if you want to reproduce the results of the original article, please see [tutorial](https://github.com/krashkov/SteinerNet/blob/master/vignettes/tutorial.pdf) and documentation.


### November 2017

SteinerNet v2.0 release. Support of igraph graphs are added.



# Citation

To use this code in your research please cite this article related to this package:

*Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network identification: an empirical study", BMC Bioinformatics 2013 14:144 doi:10.1186/1471-2105-14-144.*
