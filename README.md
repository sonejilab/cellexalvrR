![alt text](logo_small.png)

<!-- badges: start -->
  [![R-CMD-check](https://github.com/sonejilab/cellexalvrR/workflows/R-CMD-check/badge.svg)](https://github.com/sonejilab/cellexalvrR/actions)
  <!-- badges: end -->

# cellexalvrR
cellexalvrR is an R package to prep single-cell expression data for exploration/use in the [cellexalVR](https://cellexalvr.med.lu.se/) ([on github](https://github.com/sonejilab/cellexalvr)) environment, and also provides back-end functions that perform in-session calculations.


# Install using:
```
install.packages("devtools")

devtools::install_github("sonejilab/cellexalvrR")
library(cellexalvrR)
```

cellexalvrR is productively used under R 4.0.2 and 4.1.0.

# Preparing data using cellexalvrR
The vignette detailing how to use the package to prepare data for CellexalVR can be found [here](https://cellexalvr.med.lu.se/cellexalvrr-vignette). It shows how to create a cellexalvr object ready for export, and also describes a couple of functions to make converting formats and making metadata easier.

