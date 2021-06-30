![alt text](logo_small.png)

# cellexalvrR

cellexalvrR is an R package to prep single-cell expression data for exploration/use in our virtual realitiy data analysis platform [CellexalVR](https://cellexalvr.med.lu.se/) ([on github](https://github.com/sonejilab/cellexalvr)), and also to provide back-end functions that perform in-session calculations.


# Install using:

```
install.packages("devtools")

devtools::install_github("sonejilab/cellexalvrR")
library(cellexalvrR)
```

cellexalvrR is productively used under R 4.0.2 and 4.1.0.

# Preparing data using cellexalvrR

The vignette detailing how to use the package to prepare data for CellexalVR can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/inst/doc/cellexalvrRQuickStart.html). It shows how to create a cellexalvr object ready for export, and also describes a couple of functions to make converting formats and making metadata easier.


# Vignettes

The most up to date vignette about data im and export can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/inst/doc/cellexalvrRQuickStart.html).

Linear selections and post VR reporting for linear selections can be found [here](https://htmlpreview.github.io/?https://github.com/sonejilab/cellexalvrR/blob/master/inst/doc/cellexalvrR-linearSelections-vignette.html).

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
[1] cellexalvrR_0.14.6 Matrix_1.3-3

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6             compiler_4.1.0         GenomeInfoDb_1.28.0
 [4] XVector_0.32.0         R.methodsS3_1.8.1      R.utils_2.10.1
 [7] bitops_1.0-7           tools_4.1.0            zlibbioc_1.38.0
[10] bit_4.0.4              RSQLite_2.2.7          memoise_2.0.0
[13] lattice_0.20-44        pkgconfig_2.0.3        png_0.1-7
[16] rlang_0.4.11           graph_1.70.0           DBI_1.1.1
[19] rstudioapi_0.13        parallel_4.1.0         SparseM_1.81
[22] xfun_0.23              topGO_2.44.0           fastmap_1.1.0
[25] GenomeInfoDbData_1.2.6 stringr_1.4.0          knitr_1.33
[28] org.Mm.eg.db_3.13.0    httr_1.4.2             Biostrings_2.60.1
[31] S4Vectors_0.30.0       vctrs_0.3.8            IRanges_2.26.0
[34] stats4_4.1.0           bit64_4.0.5            grid_4.1.0
[37] Biobase_2.52.0         R6_2.5.0               AnnotationDbi_1.54.1
[40] bookdown_0.22          magrittr_2.0.1         GO.db_3.13.0
[43] blob_1.2.1             org.Hs.eg.db_3.13.0    matrixStats_0.59.0
[46] BiocGenerics_0.38.0    FastWilcoxTest_0.1.16  KEGGREST_1.32.0
[49] stringi_1.6.1          RCurl_1.98-1.3         cachem_1.0.5
[52] crayon_1.4.1           R.oo_1.24.0
```