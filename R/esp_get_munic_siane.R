#' @rdname esp_get_munic
#' @description \code{esp_get_munic_siane} use CartoBase ANE as source,
#' provided by Instituto Geografico Nacional (IGN),
#' \href{http://www.ign.es/web/ign/portal}{ign.es}. Years available are
#' 2005 up to today.
#' @source IGN data via a custom CDN (see
#' \url{https://github.com/rOpenSpain/mapSpain/tree/sianedata}).
#'
#' @param resolution Resolution of the polygon. Values available are
#' \code{"3", "6.5"} or  \code{"10"}.
#' @param rawcols Logical. Setting this to \code{TRUE} would add the raw
#' columns of the dataset provided by IGN.
#'
#' @details
#' On \code{esp_get_munic_siane}, \code{year} could be passed as a single
#' year ("YYYY" format, as end of year) or as a specific
#' date ("YYYY-MM-DD" format). Historical information starts as of 2005.
#'
#'
#' @examples
#'
#' Base2 <- esp_get_munic_siane(region = c("Andalucia"))
#' SAN2 <-
#'   esp_get_munic(
#'     region = c("Andalucia"),
#'     munic = c("San", "Santa")
#'   )
#'
#'
#' plot(st_geometry(Base2), col = "cornsilk", border = "grey80")
#' plot(st_geometry(SAN2),
#'      col = "firebrick3",
#'      border = NA,
#'      add = TRUE)
#' @export
esp_get_munic_siane <- function(year = "2019",
                                epsg = "4258",
                                cache = TRUE,
                                update_cache = FALSE,
                                cache_dir = NULL,
                                verbose = FALSE,
                                resolution = 3,
                                region = NULL,
                                munic = NULL,
                                moveCAN = TRUE,
                                rawcols = FALSE) {

  init_epsg <- as.character(epsg)
  year <- as.character(year)

  if (!init_epsg %in% c("4326", "4258", "3035", "3857")) {
    stop("epsg value not valid. It should be one of 4326, 4258, 3035 or 3857")
  }

  # Get Data from SIANE
  data.sf <- esp_hlp_get_siane("munic",
                               resolution,
                               cache,
                               cache_dir,
                               update_cache,
                               verbose,
                               year)

  colnames_init <- colnames(sf::st_drop_geometry(data.sf))
  df <- data.sf

  # Name management
  df$LAU_CODE <- df$id_ine
  df$name <- df$rotulo
  df$cpro <- df$id_prov

  idprov <- sort(unique(mapSpain::esp_codelist$cpro))
  df$cmun <- ifelse(substr(df$LAU_CODE, 1, 2) %in% idprov,
                    substr(df$LAU_CODE, 3, 8),
                    NA)

  cod <-
    unique(mapSpain::esp_codelist[, c("codauto",
                                      "ine.ccaa.name",
                                      "cpro", "ine.prov.name")])

  df2 <- merge(df,
               cod,
               by = "cpro",
               all.x = TRUE,
               no.dups = TRUE)

  data.sf <- df2

  if (!is.null(munic)) {
    munic <- paste(munic, collapse = "|")
    data.sf <- data.sf[grep(munic, data.sf$name), ]
  }

  if (!is.null(region)) {
    tonuts <- esp_hlp_all2prov(region)
    #toprov
    df <- unique(mapSpain::esp_codelist[, c("nuts3.code", "cpro")])
    df <- df[df$nuts3.code %in% tonuts, "cpro"]
    toprov <- unique(df)
    data.sf <- data.sf[data.sf$cpro %in% toprov, ]
  }

  if (nrow(data.sf) == 0) {
    stop("The combination of region and/or munic does ",
         "not return any result")
  }
  # Move CAN

  # Checks
  moving <- FALSE
  moving <- isTRUE(moveCAN) | length(moveCAN) > 1

  if (moving) {
    if (length(grep("05", data.sf$codauto)) > 0) {
      offset <- c(550000, 920000)

      if (length(moveCAN) > 1) {
        coords <- sf::st_point(moveCAN)
        coords <- sf::st_sfc(coords, crs = sf::st_crs(4326))
        coords <- sf::st_transform(coords, 3857)
        coords <- sf::st_coordinates(coords)
        offset <- offset + as.double(coords)
      }

      data.sf <- sf::st_transform(data.sf, 3857)
      PENIN <- data.sf[-grep("05", data.sf$codauto), ]
      CAN <- data.sf[grep("05", data.sf$codauto), ]

      # Move CAN
      CAN <- sf::st_sf(
        sf::st_drop_geometry(CAN),
        geometry = sf::st_geometry(CAN) + offset,
        crs = sf::st_crs(CAN)
      )

      #Regenerate
      if (nrow(PENIN) > 0) {
        data.sf <- rbind(PENIN, CAN)
      } else {
        data.sf <- CAN
      }
    }
  }

  data.sf <- sf::st_transform(data.sf, as.double(init_epsg))
  data.sf <-
    data.sf[order(data.sf$codauto, data.sf$cpro, data.sf$cmun), ]

  namesend <- unique(c(
    colnames_init,
    c(
      "codauto",
      "ine.ccaa.name",
      "cpro",
      "ine.prov.name",
      "cmun",
      "name",
      "LAU_CODE"
    ),
    colnames(data.sf)
  ))

  data.sf <- data.sf[, namesend]

  if (isFALSE(rawcols)) {
    data.sf <- data.sf[, c("codauto",
                           "ine.ccaa.name",
                           "cpro",
                           "ine.prov.name",
                           "cmun",
                           "name",
                           "LAU_CODE")]

  }
  return(data.sf)
}
