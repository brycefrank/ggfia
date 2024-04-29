#' Via ggforce
#' https://rdrr.io/cran/ggforce/src/R/aaa.R
combine_aes <- function(aes1, aes2) {
  aes_all <- c(aes1[setdiff(names(aes1), names(aes2))], aes2)
  class(aes_all) <- class(aes1)
  aes_all
}