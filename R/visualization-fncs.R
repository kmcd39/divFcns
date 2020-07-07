#' quick.leaflet
#'
#' Quick leaflet for polygons that need to be colored by a column.
quick.leaflet <- function(st_df, color_by = "div_type", pal = RColorBrewer::brewer.pal(8, "Dark2"), weight = 8) {
  require(leaflet)
  st_df <- st_transform(st_df, 4326)

  if (!is.null(color_by)) {
    var <- rlang::sym(color_by)
    val <- pull(st_df, !!var)
    pal <- colorFactor(pal, domain = val)
  }

  out <-
    leaflet(st_df) %>%
    addProviderTiles(provider = "CartoDB.DarkMatter") %>%
    addPolygons(
      color = "#008080",#~pal(val),
      fillColor = ~pal(val),
      opacity = .4,
      weight = weight,
      label = lapply(val, shiny::HTML)
    )

  return(out)
}


#' quick.div.map
#'
#' Plots a region boundaries and the overlapping divisions.
#' Takes at minimum just a region.id/.type. Names of objects referenced \code{geo.list}
#' and \code{div} for boundaries and divisions are hardcoded and indexed from.
quick.div.map <- function(region.id, region.type
                          ,div.type = "SIGNT1"
                          ,filter.to.type = NULL
                          ,pal = RColorBrewer::brewer.pal(8, "Dark2")
                          ,weight = 8) {
  require(leaflet)
  boundaries <-
    geo.list[[region.type]] %>%
    filter(region.id == !!region.id) %>%
    st_transform(4326)

  divs <-
    div %>%
    filter(region.id == !!region.id &
             region.type == !!region.type) %>%
    st_transform(4326)


  var <- rlang::sym(div.type)
  val <- pull(divs, !!div.type)
  pal <- colorFactor(pal, domain = val)

  if(!is.null(filter.to.type))
    divs <-
    divs %>%
    filter(!!rlang::sym(div.type) %in% filter.to.type)

  out <-
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.DarkMatter") %>%
    addPolygons(data = boundaries) %>%
    addPolylines(data = divs
                 ,color = ~pal(pull(divs, !!div.type))
                 ,weight = weight
                 ,label = lapply(pull(divs, !!div.type)
                                 , shiny::HTML))
  return(out)
}
