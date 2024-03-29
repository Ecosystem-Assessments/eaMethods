#' eaMethods: collection of functions used in our ecosystemic assessments
#'
#' @docType package
#' @name eaMethods
#'
#' @importFrom taxize classification
#' @importFrom utils read.csv write.csv read.table
#' @importFrom worrms wm_name2id_
NULL


# ------------------------------------------------------------------------------
# Helper function to remove and then add whitespaces again
trim_then_add <- function(string) {
  string <- as.vector(string)
  string <- stringr::str_trim(string, side = "both")
  string <- glue::glue(" {string} ")
  string
}

# ------------------------------------------------------------------------------
# Messages
msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::green(txt), appendLF = appendLF)
  invisible(txt)
}
