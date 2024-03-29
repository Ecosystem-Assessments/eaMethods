#' Function to clean, combine and remove taxa names
#'
#' This function clean taxa names, combines taxa that are similar taxonomically and that may
#' be hard to distinguish in the field, and removes taxa whose identification is too coarse to
#' be useful.
#'
#' @param df data.frame containing the list of species to be cleaned
#' @param field name of the field containing the list of scientific species names
#' @param review what type(s) of review to perform between c("clean", "remove", "combine", "aphia", "classification"). "clean" removes a certain number of text usually accompanying species names, such as "sp." and "spp.". "combine" modifies the names of species to combine certain species based on a list of species that should be combined (view code), "remove" removes all species that are contained in a list of species that should be removed, "aphia" gets aphiaID of species using the `worrms` package, and "classification" uses the aphia ID and the `taxize` package to get taxonomic classification of species.
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
#'   species = c(
#'     "Boltenia c.,large", "Boltenia sp. (UNID)",
#'     "Triglops spp.", "Triglops murrayi",
#'     "Amphipoda"
#'   ),
#'   frequency = c(61, 100, 27, 50, 2)
#' )
#' field <- "species"
#'
#' review_taxa(df, field, "clean")
#' review_taxa(df, field, c("clean", "remove"))
#' review_taxa(df, field, c("clean", "combine"))
#' review_taxa(df, field, c("clean", "aphia"))
#' (df <- review_taxa(df, field, c("clean", "remove", "combine", "aphia")))
#' df <- unique(df[, c("species", "aphiaID")])
#' review_taxa(df, field, c("clean", "remove", "combine", "aphia", "classification"))
#'
#' @export
review_taxa <- function(df, field, review = c("clean", "remove", "combine")) {
  # Clean up species names
  if ("clean" %in% review) df <- clean_taxa(df, field)
  if ("remove" %in% review) df <- remove_taxa(df, field)
  if ("combine" %in% review) df <- combine_taxa(df, field)
  if ("aphia" %in% review) df <- get_aphia(df, field)
  if ("classification" %in% review) df <- get_classification(df)
  df
}

#' @name review_taxa
#' @export
clean_taxa <- function(df, field) {
  rm <- taxa_clean$remove
  nm <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(field))
  nm <- nm[, 1]

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
  nm <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(field))
  nm <- nm[, 1]
  uid <- nm %in% rm
  df[!uid, ]
}

#' @name review_taxa
#' @export
combine_taxa <- function(df, field) {
  nm <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(field))
  nm <- nm[, 1]

  for (i in 1:nrow(taxa_combine)) {
    uid <- nm %in% taxa_combine$from[i]
    nm[uid] <- taxa_combine$to[i]
  }
  df[, field] <- nm
  df
}

#' @name review_taxa
#' @export
get_aphia <- function(df, field) {
  nm <- as.data.frame(df) |>
    dplyr::select(dplyr::any_of(field))
  nm <- nm[, 1]

  # Get species AphiaID
  uid <- worrms::wm_name2id_(name = nm)
  dat <- data.frame(species = names(uid), aphiaID = unlist(uid))
  df <- dplyr::left_join(df, dat, by = setNames("species", field)) |>
    dplyr::mutate(aphiaID = ifelse(aphiaID == -999, NA, aphiaID))

  # Verify missing ones and replace with known Aphia in aphia.csv
  idna <- is.na(df$aphiaID)
  verif <- df[idna, ] |>
    dplyr::select(-aphiaID) |>
    dplyr::left_join(taxa_aphia, by = setNames("Species", field))

  # Put back together
  df$aphiaID[idna] <- verif$aphiaID

  # return
  df
}

#' @name review_taxa
#' @export
get_classification <- function(df) {
  classif <- taxize::classification(df$aphiaID, db = "worms")
  x <- classif
  nm <- names(classif)

  for (i in 1:length(classif)) {
    x[[i]] <- as.data.frame(x[[i]])
    if ("id" %in% colnames(x[[i]])) {
      x[[i]] <- dplyr::select(x[[i]], -id)
    }

    # -----
    if ("species" %in% x[[i]]$rank) {
      gn <- x[[i]]$rank == "Genus"
      sp <- x[[i]]$rank == "Species"
      temp <- c(paste(x[[i]]$name[gn], x[[i]]$name[sp]), "ScientificName")
    } else {
      temp <- c(dplyr::last(x[[i]]$name), "ScientificName")
    }
    x[[i]] <- rbind(x[[i]], temp)

    # -----
    x[[i]]$aphiaID <- nm[i]
  }

  # -----
  class(x) <- "list"
  x <- dplyr::bind_rows(x)

  # -----
  x <- tidyr::pivot_wider(
    x,
    id_cols = aphiaID,
    names_from = rank,
    values_from = name
  ) |>
    dplyr::mutate(aphiaID = as.numeric(aphiaID)) |>
    dplyr::select(aphiaID, ScientificName, any_of(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")))


  # -----
  df$aphiaID <- as.numeric(df$aphiaID)
  x$aphiaID <- as.numeric(x$aphiaID)
  if (any(is.na(df$aphiaID))) rlang::abort("Aphia ID must all be valid. Check list.")
  df <- dplyr::left_join(df, x, by = "aphiaID")

  # -----
  df
}
