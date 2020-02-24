#' Compute mean plus 3 sd on a vector 
#'
#' This function computes mean plus 3 sd on a vector
#' @param x a vector of genes expression
#' @importFrom stats sd
#' @export
m3sd = function(x) {
  mnsd(x,3)
}

#' Compute mean plus n sd on a vector 
#'
#' This function computes mean plus n sd on a vector
#' @param x a vector of genes expression
#' @param n a numeric corresponding to the number of sd to add
#' @importFrom stats sd
#' @export
mnsd = function(x,n) {
  mean(x)+(n*sd(x))
}
