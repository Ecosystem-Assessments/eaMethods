# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Smoothing functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Kernel weighted smoothing with arbitrary bounding area
#' Adapted from: https://www.r-bloggers.com/kernel-spatial-smoothing-transforming-points-pattern-to-continuous-coverage/
#' @param df sf object (points)
#' @param field weigth field in sf
#' @param bandwith kernel bandwidth (map units)
#' @param resolution output grid resolution (map units)
#' @param grid sf study grid (polygon)
#' @param out_crs EPSG (should be an equal-area projection)
#'
#' @return a raster object
#' @import btb, raster, fasterize, dplyr, plyr, sf
lissage <- function(df, field, bandwidth, resolution, grid, out_crs = 32198) {
  # Kernel
  # message("computing kernel...")
  suppressMessages({
    kernel <- df %>%
      cbind(., st_coordinates(.)) %>%
      st_set_geometry(NULL) %>%
      dplyr::select(x = X, y = Y, field) %>%
      btb::kernelSmoothing(dfObservations = .,
                           sEPSG = out_crs,
                           iCellSize = resolution,
                           iBandwidth = bandwidth,
                           vQuantiles = NULL,
                           dfCentroids = zone_xy)
  })

  # Values > 1 to 1
  kernel[,field] <- ifelse(kernel[,field,drop=T] > 1, 1, kernel[,field,drop=T])

  # Rasterization
  # message("\nrasterizing...")
  rasterGrid %>%
  # raster::raster(xmn = plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
  #                ymn = plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor),
  #                xmx = plyr::round_any(zone_bbox[3] + bandwidth, resolution, f = ceiling),
  #                ymx = plyr::round_any(zone_bbox[4] + bandwidth, resolution, f = ceiling),
  #                resolution = resolution) %>%
    fasterize::fasterize(kernel, ., field = field)
}

zoneGrid <- function(studyArea, resolution, bandwidth) {
  # Bounding box
  zone_bbox <- st_bbox(studyArea)

  # Grid for analyses
  studyArea %>%
  dplyr::select(geometry) %>%
  st_make_grid(cellsize = resolution,
               offset = c(plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
                          plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor)),
               what = "centers") %>%
  st_sf() %>%
  st_join(studyArea, join = st_intersects, left = FALSE) %>%
  st_coordinates() %>%
  as_tibble() %>%
  dplyr::select(x = X, y = Y)
}