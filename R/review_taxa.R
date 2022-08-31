#' Function to clean, combine and remove taxa names
#'
#' This function clean taxa names, combines taxa that are similar taxonomically and that may
#' be hard to distinguish in the field, and removes taxa whose identification is too coarse to
#' be useful.
#'
#' @param df data.frame containing the list of species to be cleaned
#' @param field name of the field containing the list of scientific species names
#' @param review what type(s) of review to perform between c("clean","remove","combine"). "clean" removes a certain number of text usually accompanying species names, such as "sp." and "spp.". "combine" modifies the names of species to combine certain species based on a list of species that should be combined (view code), and "remove" removes all species that are contained in a list of species that should be removed.
#'
#' @keywords taxonomy
#' @keywords species
#'
#' @name review_taxa
#'
#' @returns This function returns the provided data.frame with the names of the species reviewd,
#' cleaned, combined and/or removed. Note that this function was initially built for the species
#' of the estuary and gulf of St. Lawrence and extended to include species of the North West
#' Atlantic. Using this function on any list should be done with care and be thhoroughly reviewed
#' afterwarsd. Also note that the justification for combinations and removals are documented in
#' the code.
#'
#' @examples
#' df <- data.frame(
#'   species = c("Boltenia", "Boltenia sp.", "Triglops spp.", "Triglops murrayi", "Amphipoda"),
#'   frequency = c(61, 100, 27, 50, 2)
#' )
#' field <- "species"
#'
#' # Clean
#' df <- clean_taxa(df, field)
#' df
#'
#' # Remove
#' df <- remove_taxa(df, field)
#' df
#'
#' # Combine
#' df <- combine_taxa(df, field)
#' df
#'
#' @export
review_taxa <- function(df, field, review = c("clean", "remove", "combine")) {
  # Clean up species names
  if ("clean" %in% review) df[, field] <- clean_taxa(df[, field])
  if ("remove" %in% review) df <- remove_taxa(df, field)
  if ("combine" %in% review) df[, field] <- combine_taxa(df[, field])
}

#' @name review_taxa
#' @export
clean_taxa <- function(df, field) {
  rm <- taxa_clean$remove
  nm <- df[, field]

  # Trim spaces, then add them again
  rm <- trim_then_add(rm)
  nm <- trim_then_add(nm)

  # Species names to lowercase
  nm <- tolower(nm)

  # Remove from species names
  for (i in 1:length(nm)) {
    for (j in 1:length(rm)) {
      nm[i] <- gsub(rm[j], " ", nm[i]) |>
        trim_then_add()
    }
  }

  nm <- stringr::str_trim(nm, side = "both") |> # Trim spaces
    stringr::str_squish() |> # Remove multiple whitespaces
    stringr::str_to_sentence() # First letter as capital

  # Put back in data.frame
  df[, field] <- nm

  # Return
  df
}

#' @name review_taxa
#' @export
remove_taxa <- function(df, field) {
  rm <- taxa_remove$remove
  uid <- df[, field] %in% rm
  df[!uid, ]
}

#' @name review_taxa
#' @export
combine_taxa <- function(df, field) {
  for (i in 1:nrow(taxa_combine)) {
    uid <- df[, field] %in% taxa_combine$from[i]
    df[uid, field] <- taxa_combine$to[i]
  }
  df
}
