#' Function to evaluate fishing intensity
#'
#' The function evaluates fishing intensity over a regular grid
#'
#' @param fishData fishing data as sf object
#' @param areaGrid regular grid as sf object
#' @param metric type of fishing intensity metric to evaluate, one of: 1: Fishing effort density, no normalization; 2: Fishing effort density; 3: Fishing biomass yield density; 4: Fishing relative biomass yield density
#' @param id_field name of column containing the unique identifier of the cells in the regular grid
#' @param biomass_field name of column containing biomass data, if applicable
#'
#' @keywords fishing intensity
#'
#' @returns This function returns a data.frame with a column containing the unique identifier of the grid cell and a column containing the assessment of fishing intensity.
#'
#' @export
#'
#' @examples
#' grd <- sf::st_make_grid(cellsize = 1, offset = c(0, 0), n = c(10, 10)) |>
#'   sf::st_sf() |>
#'   dplyr::mutate(uid = 1:100)
#' fish <- data.frame(lon = runif(50, 0, 10), lat = runif(50, 0, 10)) |>
#'   sf::st_as_sf(coords = c("lon", "lat")) |>
#'   sf::st_buffer(dist = 0.5) |>
#'   dplyr::mutate(biomass = runif(50, 0, 10000))
#'
#' int1 <- fishing_intensity(fishData = fish, areaGrid = grd, metric = 1)
#' int2 <- fishing_intensity(fishData = fish, areaGrid = grd, metric = 2)
#' int3 <- fishing_intensity(fishData = fish, areaGrid = grd, metric = 3)
#' int4 <- fishing_intensity(fishData = fish, areaGrid = grd, metric = 4)
#'
#' grd <- dplyr::left_join(grd, int1, by = "uid")
#' grd <- dplyr::left_join(grd, int2, by = "uid")
#' grd <- dplyr::left_join(grd, int3, by = "uid")
#' grd <- dplyr::left_join(grd, int4, by = "uid")
#'
#' plot(grd)
fishing_intensity <- function(fishData, areaGrid, metric = 1, id_field = "uid", biomass_field = "biomass") {
  # ~~~~~       INITIAL MEASUREMENTS        ~~~~~ #
  # Calculate total area of all points transformed in polygons and bind to polygons
  fishData$AreaTotKM2 <- as.numeric(sf::st_area(fishData) / 1000000)

  # Calculate total biomass and bind to polygons
  if (metric %in% c(3, 4)) fishData$BiomassTotKg <- sum(fishData[, biomass_field, drop = TRUE])

  # Intersect points as polygons to study grid as polygons
  # WARNING: Very inefficient for metric 1, which would only need to use st_intersects
  # NOTE: Selecting only grid cells for which we have overlapping data would make this faster.
  #       This is because study grids would usually contain much more cells than those actually
  #       containing fishing activities.
  #       Any user should consider doing this in their own script.
  fishData <- suppressWarnings(sf::st_intersection(areaGrid, fishData))

  # Calculate area of intersected geometries and bind to spatial polygons data frame
  fishData$AreaKM2 <- as.numeric(sf::st_area(fishData) / 1000000)

  # Remove geometry
  fishData <- sf::st_drop_geometry(fishData)

  # Calculate proportion of intersected geometries from total area and bind to spatial polygons data frame
  fishData$PropAreaTot <- fishData$AreaKM2 / fishData$AreaTotKM2

  # Calculate relative biomass as a proportion of fishing area and bind to polygons data frame
  if (metric %in% c(3, 4)) {
    fishData$PropBiomassKg <- fishData[, biomass_field, drop = TRUE] * fishData$PropAreaTot
  }

  # Calculate biomass as a proportion of fishing area and bind to polygons data frame
  if (metric %in% c(3, 4)) {
    fishData$RelPropBiomassKg <- fishData$PropBiomassKg / fishData$BiomassTotKg
  }
  # ~~~~~       FISHING METRICS        ~~~~~ #
  PropAreaTot <- PropBiomassKg <- RelPropBiomassKg <- Var1 <- NULL # For R CMD CHECK

  if (1 == metric) {
    # Metric 1: Fishing effort density, no normalization
    dat <- fishData[, id_field] |>
      table() |>
      as.data.frame() |>
      mutate(Var1 = as.numeric(as.character(Var1)))
    colnames(dat) <- c(id_field, "FishEffortDens")
  }

  if (2 == metric) {
    # Metric 2: Fishing effort density
    dat <- fishData |>
      dplyr::group_by(!!as.name(id_field)) |>
      dplyr::summarize(FishEffortDensProp = sum(PropAreaTot)) |>
      data.frame()
  }

  if (3 == metric) {
    # Metric 3: Fishing biomass yield density
    dat <- fishData |>
      dplyr::group_by(!!as.name(id_field)) |>
      dplyr::summarize(FishBiomassKg = sum(PropBiomassKg)) |>
      data.frame()
  }

  if (4 == metric) {
    # Metric 4: Fishing relative biomass yield density
    dat <- fishData |>
      dplyr::group_by(!!as.name(id_field)) |>
      dplyr::summarize(RelFishBiomassKg = sum(RelPropBiomassKg)) |>
      data.frame()
  }

  return(dat)
}
