# DESPOTA

DESPOTA (**DE**ndogram **S**licing through a **P**ermutati**O**n **T**est **A**pproach) is a novel approach exploiting permutation tests in order to automatically detect a partition among those embedded in a dendrogram. Unlike the traditional approach, DESPOTA includes in the search space also partitions not corresponding to horizontal cuts of the dendrogram.

The output of hierarchical clustering methods is typically displayed as a dendrogram describing a family of nested partitions. However, the exploitable partitions are usually restricted to those relying on horizontal cuts of the tree, missing the possibility to explore the whole set of partitions housed in the dendrogram. We introduced an algorithm, DESPOTA, exploiting the methodological framework of permutation tests, that permits a partition to be automatically found where clusters do not necessarily obey the above principle. Our solution adapts to every choice of the distance metric and agglomeration criterion used to grow the tree.

You can find more information on **DESPOTA** [following this link](http://domenicovistocco.it/en/dv-blog/despota-page/).

## Installation

``` r
# Install from CRAN 
# !!!---not available at the moment---!!!
# install.packages("DESPOTA")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("blog-neas/DESPOTA")
```
