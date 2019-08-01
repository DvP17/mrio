Work with MRIO data
===================

This R-package provides various methods for environmentally-extended
multi-regional input–output (EE-MRIO) analysis. It provides different
methods for calculating EEMRIO matrices and characterization factors.

-   supports Exiobase3 and Eora
-   characterization factors:
    -   **b**iodiversity **l**oss `"bl"` (currently only for Exiobase)
    -   **b**lue **w**ater consumption `"bw"`
    -   **c**limate **c**hange impacts `"cc"`
    -   **en**ergy demand `"en"`
    -   **l**and **u**se `"lu"`
    -   **m**aterial **f**ootprint `"mf"`
    -   **w**ater **s**tress `"ws"` (currently only for Exiobase)
-   calculate:
    -   **p**roduction to **d**emand matrix `"pd"`
    -   **p**roduction to target **d**emand matrix `"no-double-pt"`
    -   **t**arget to final **s**upply matrix `"no-double-ts"`
    -   **t**arget to final **d**emand matrix `"no-double-td"`
    -   **p**roduction to final **d**emand matrix `"no-double-pd"`
-   create country dyads

First Steps
-----------

### 1 Install MRIO-Package

``` r
install.packages("remotes")
remotes::install_github("dvp17/mrio")
```

### 2 Calculate Matrix

Before you begin, set your working directory to the path where you store
your Eora and Exiobase files:

e.g. `setwd("C:/Data/Exiobase")` or `setwd("C:/Data/Eora")`

``` r
setwd("C:/Data/Exiobase")
exio_cc <- mrio::exioloop(1995:2000, "cc", "pd")
utils::View(exio_cc[1:100,])
```

Please be aware that the process of calculating the matrices will take
some time.

### 3 Create Dyads

``` r
setwd("C:/Data/Exiobase")
exio_cc_dyad <- mrio::dyads(1995:1996, exio_cc)
utils::View(exio_cc_dyad[1:100,])
```

Also the construction of the dyads is time-consuming. Please notice that
no progress bar or estimated time for process completion is given
before.

### 4 Look Into Characterization Factors

For further information on which extensions are include for which
environmental impact have a look on the characterization factors listed
in the package. Notice that calculation of water stress and biodiversity
loss is a little more complex, which makes renders it difficult to
provide a list on those CFs.

``` r
utils::View(mrio::cf_eora)
utils::View(mrio::cf_eora[mrio::cf_eora$cf_cc > 0,])
```

Recent Advancements
-------------------

-   added processing of pxp-matrices from Exiobase

Plan
----

-   add WIOD data
-   add `bl` and `ws` for Eora
-   include social indicators
-   structural path calculation?
-   implement function to download data automatically
