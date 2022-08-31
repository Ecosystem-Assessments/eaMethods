#' eaMethods: collection of functions used in our ecosystemic assessments
#'
#' @docType package
#' @name eaMethods
#'
#' @importFrom utils read.csv write.csv read.table
NULL


# ------------------------------------------------------------------------------
# Helper function to remove and then add whitespaces again
trim_then_add <- function(string) {
  string <- stringr::str_trim(string, side = "both")
  string <- glue::glue(" {string} ")
  string
}
