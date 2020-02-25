# ector
An R package that detects ectopic gene expression in cancer


## Installation

```R
# install.packages("devtools")
devtools::install_github("mpetrier/ector")
```


## Develpment

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
devtools::load_all(); devtools::document(); devtools::install(); devtools::check()
```

### Example

```R
rmarkdown::render("vignettes/vignette_ector.Rmd")
```
