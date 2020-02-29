# ector
An R package that detects ectopic gene expression in cancer


## Installation

```R
# install.packages("devtools")
devtools::install_github("mpetrier/ector", build_vignettes=TRUE)
```

To learn on how to use this method, please visit the vignette of our package.

```R
vignette("vignette_ector", package = "ector")
```


## Development

### Get sources

To get the current development version from github:

```
git clone https://github.com/mpetrier/ector
cd ector
R
```


### Build package

```R
# install.packages("devtools")
devtools::load_all(); devtools::document(); devtools::install(build_vignettes=TRUE); devtools::check()
```
