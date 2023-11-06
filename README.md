# DESPOTA <!-- <img src="man/img/hexlogo.png" align="right" height="138.5" /> -->

<!-- badger source: https://github.com/GuangchuangYu/badger -->
<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/DESPOTA?color=orange)](https://cran.r-project.org/package=DESPOTA) -->
[![R-CMD-check](https://github.com/blog-neas/DESPOTA/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/blog-neas/DESPOTA/actions/workflows/check-standard.yaml)
<!-- [![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: Active - The project is being actively developed](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/blog-neas/DESPOTA/branch/main/graph/badge.svg?token=0XHCFZZYN8)](https://codecov.io/gh/blog-neas/DESPOTA) 
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3) -->
<!-- badges: end -->


## Overview
DESPOTA (**DE**ndogram **S**licing through a **P**ermutati**O**n **T**est **A**pproach) is a novel approach exploiting permutation tests in order to automatically detect a partition among those embedded in a dendrogram. Unlike the traditional approach, DESPOTA includes in the search space also partitions not corresponding to horizontal cuts of the dendrogram.

The output of hierarchical clustering methods is typically displayed as a dendrogram describing a family of nested partitions. However, the exploitable partitions are usually restricted to those relying on horizontal cuts of the tree, missing the possibility to explore the whole set of partitions housed in the dendrogram. We introduced an algorithm, DESPOTA, exploiting the methodological framework of permutation tests, that permits a partition to be automatically found where clusters do not necessarily obey the above principle. Our solution adapts to every choice of the distance metric and agglomeration criterion used to grow the tree.

You can find more information on **DESPOTA** [following this link](http://domenicovistocco.it/en/dv-blog/despota-page/).


## Installation

``` r
# Install from CRAN 
# !!!---not available at the moment---!!!
# install.packages("DESPOTA")

# Install the stable version from GitHub
# install.packages("devtools")
devtools::install_github("blog-neas/DESPOTA")
# Or the development version from GitHub 
devtools::install_github("blog-neas/DESPOTA", ref = "devel")
```

## Refecence

Bruzzese, D., & Vistocco, D. (2015). DESPOTA: DEndrogram slicing through a pemutation test approach. Journal of classification, 32, 285-304.


## Roadmap

#### Main Steps

- [x] DESPOTA base function
- [x] DESPOTA parallelized version
- [ ] TD-DESPOTA
- [ ] Visualization
	- [ ] Summary
	- [x] Plotting

#### Secondary Steps

- [ ] Define package structure and state
	- [ ] Functions
	- [x] Dependencies list
- [x] Licensing: GPL-3
- [ ] Testing
- [ ] Documentation
	- [ ] Function documentation
	- [ ] Vignettes
- [ ] Maintenance and distribution
	- [ ] Continuous integration
	- [ ] Releasing to CRAN
	- [ ] Lifecycle
	- [ ] References
- [ ] Further steps and developments

### Report a bug 🐛

Using `DESPOTA` and discovered a bug? That's annoying! Don't let others have the same experience and report it as an [issue on GitHub][new_issue] so we can fix it. A good bug report makes it easier for us to do so, so please include:

* Your operating system name and version (e.g. Mac OS 10.13.6).
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

Care to fix bugs or implement new functionality for `DESPOTA`? Awesome! 👏 Have a look at the [issue list][issues] and leave a comment on the things you want to work on.


--------------------------------------------------------------------------------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.

