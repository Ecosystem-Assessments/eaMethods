#' Strings to remove from the scientific names of species
#'
#' A dataset containing a list of regex strings used to clean up the scientific names of species
#'
#' @format A data frame with 1 variable:
#' \describe{
#'   \item{remove}{regex strings to remove from species names}
#'   ...
#' }
"taxa_clean"

#' Species to remove from a dataset
#'
#' A dataset containing a list of taxonomic  names to remove from a data.frame
#'
#' @format A data frame with 2 variable:
#' \describe{
#'   \item{remove}{list of taxonomic names to remove from a data.frame}
#'   \item{documentation}{reasoning for the removal}
#'   ...
#' }
"taxa_remove"

#' Species to combine in a dataset
#'
#' A dataset containing a list of taxonomic names to combine in a data.frame
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{to}{new taxonomic name to use}
#'   \item{from}{taxonomic name to transform}
#'   \item{documentation}{reasoning for the removal}
#'   ...
#' }
"taxa_combine"
