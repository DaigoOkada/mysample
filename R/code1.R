#' Calculation of minor allele frequency
#'
#' This Function is to calculate minor allele frequency.library
#' @param geno        vector, the number of three genotypes AA, Aa, aa
#' @param comment       a logical value indicating whether the comment "Hellow!" is shown. default is TRUE.
#' @return Minor allele frequency of the locus
#' @details
#' @keywords NA
#' @export
#' @examples
#' library(mysample)
#' geno <- c(22,56,22)
#' maf_calculation(N)
#'

maf_calculation <- function(geno,comment=TRUE){
    AA <- geno[1]
    Aa <- geno[2]
    aa <- geno[3]
    n <- sum(geno)
    freq <- (2*AA + Aa)/(2*n)
    maf <- min(freq, 1-freq)
    if(comment){
      print("Hello!")
    }
    return(maf)
}

#' The genotype data
#'
#' The dataset of the samples from multinomial distribution.
#'
#' @format A matrix with 100 row and 3 variables
#' \describe{
#' \item{AA}{the number of the individuals with genotype AA}
#' \item{Aa}{the number of the individuals with genotype Aa}
#' \item{Aa}{the number of the individuals with genotype aa}
#' }
"geno"

#' @useDynLib mysample
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) { library.dynam.unload("mysample", libpath)}

#Rcpp::sourceCpp("./src/myrcpp.cpp")
