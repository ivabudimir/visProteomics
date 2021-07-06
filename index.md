
<!-- README.md is generated from README.Rmd -->

## visProteomics

**visProteomics** helps with the exploration and visualization of
proteomics data obtained with the process of fractionation.
Fractionation of samples is often used in proteomics to improve
signal-to-noise ratio and proteome coverage. **visProteomics** explores
attributes of proteins detected in different fractions through both
graphical and tabular representation.

Venn/Euler diagrams are widely used for data summarization and there are
many plotting tools available. **visProteomics** defines a complementary
tabular function. The resulting data frame summarizes attributes of
elements found in different regions of the Venn diagram. This generic
function can be used for any data set.

## visProteomics package as part of proteomics analysis

![visProteomics_illustration](https://user-images.githubusercontent.com/37818185/124632049-459e8c80-de84-11eb-814e-9f80fdb10245.png)

## Installation

``` r
devtools::install_github("ivabudimir/visProteomics")
```

For installation of vignettes together with the package, add
“build\_vignettes=TRUE”.

## Usage

``` r
library(visProteomics)
```

For the exploration and visualization of a fractionated proteomic sample, read vignette *[vis-fractions](articles/vis-fractions.html)*. If you need help with the data preparation because the software for protein identification returned a separate list of proteins for each fraction, first read vignette *[merge-fractions](articles/merge-fractions.html)*.

For the tabular description od the Venn diagram regions, read vignette *[explore-venn](articles/explore-venn.html)*.

## License

visProteomics is open source software, licensed under
[GPL-3](https://github.com/ivabudimir/visProteomics/blob/master/LICENSE).
