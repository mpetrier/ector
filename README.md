# ector
An R package that detects ectopic gene expression in cancer


# Installation


## From github

```R
# install.packages("devtools")
devtools::install_github("mpetrier/ector")
```


## From sources

### Get sources

To get the current development version from github:

```
git clone https://github.com/mpetrier/ector
cd ector
R
```


### Build package

```R
install.packages("devtools")
devtools::load_all(); devtools::document(); devtools::install(); devtools::check(build_args="--no-build-vignettes")
```

### Example

```R
setwd("vignettes")
rmarkdown::render("vignette_ector.Rmd")
```
