![alt text](logo_small.png)

# cellexalvrR

[![DOI](https://zenodo.org/badge/133559720.svg)](https://zenodo.org/badge/latestdoi/133559720)

cellexalvrR is an R package to prep single-cell expression data for exploration/use in our virtual realitiy data analysis platform [CellexalVR](https://cellexalvr.med.lu.se/) ([on github](https://github.com/sonejilab/cellexalvr)), and also to provide back-end functions that perform in-session calculations.


# Install using:

```
install.packages("devtools")

devtools::install_github("sonejilab/cellexalvrR" )
library(cellexalvrR)
```

cellexalvrR is productively used under R 4.0.2 and 4.1.0.

# Preparing data using cellexalvrR

The vignette detailing how to use the package to prepare data for CellexalVR can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/helperScripts/vignettes/cellexalvrRQuickStart.html). It shows how to create a cellexalvr object ready for export, and also describes a couple of functions to make converting formats and making metadata easier.


# Vignettes

The most up to date vignette about data import and export can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/helperScripts/vignettes/cellexalvrRQuickStart.html).

Linear selections and post VR reporting for linear selections can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/helperScripts/vignettes/cellexalvrR-linearSelections-vignette.html). The portable log zip file created in this vignette can be downloaded [from here](https://github.com/sonejilab/cellexalvrR/blob/master/inst/extdata/PortableLog_linearExample.zip).

# Currently used R packages:

```
R version 4.1.0 (2021-05-18)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_Sweden.1252  LC_CTYPE=English_Sweden.1252
[3] LC_MONETARY=English_Sweden.1252 LC_NUMERIC=C
[5] LC_TIME=English_Sweden.1252

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] cellexalvrR_0.14.8 Matrix_1.3-4

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.7             compiler_4.1.0         GenomeInfoDb_1.28.4
 [4] XVector_0.32.0         R.methodsS3_1.8.1      R.utils_2.11.0
 [7] bitops_1.0-7           tools_4.1.0            zlibbioc_1.38.0
[10] bit_4.0.4              RSQLite_2.2.8          memoise_2.0.1
[13] lattice_0.20-45        pkgconfig_2.0.3        png_0.1-7
[16] rlang_0.4.12           graph_1.70.0           DBI_1.1.1
[19] parallel_4.1.0         SparseM_1.81           xfun_0.28
[22] topGO_2.44.0           fastmap_1.1.0          GenomeInfoDbData_1.2.6
[25] stringr_1.4.0          knitr_1.36             org.Mm.eg.db_3.13.0
[28] httr_1.4.2             Biostrings_2.60.2      S4Vectors_0.30.2
[31] vctrs_0.3.8            IRanges_2.26.0         stats4_4.1.0
[34] bit64_4.0.5            grid_4.1.0             Biobase_2.52.0
[37] R6_2.5.1               AnnotationDbi_1.54.1   bookdown_0.24
[40] magrittr_2.0.1         GO.db_3.13.0           blob_1.2.2
[43] org.Hs.eg.db_3.13.0    matrixStats_0.61.0     BiocGenerics_0.38.0
[46] FastWilcoxTest_0.1.16  KEGGREST_1.32.0        stringi_1.7.5
[49] RCurl_1.98-1.5         cachem_1.0.6           crayon_1.4.2
[52] R.oo_1.24.0
```
