#' polygonal.div
#'
#' Given a boundary and corresponding divisions, within boundary, filters by
#' div.type (currently hardcoded to SIGNT1 column), then finds number of
#' sub-polygons in region. Returns a dataframe with region identifiers and
#' number of polygons. Filter all subpolygons w/ area between 50,000 m^2 (these
#' comprise small corners + highway interchanges for example.) If 'spatial
#' output' is false, it returns the region identifiers with number of polygons.
#' If it's true, an st df with identified polygons is returned.
#' @import lwgeom
#' @export
polygonal.div <- function(boundary, div,
                          div.types = c("U", "S", "I"), min.size = 5e5
                          , verbose = T, spatial.output = FALSE) {

  if(verbose) cat("generating", unique(boundary$region.type),
                  "-",unique(boundary$region.id), "\n" )

  included.divisions <- div %>%
    filter(SIGNT1 %in% div.types)

  # return a df row with identifiers and 0 as measure if no included divisions overlap
  if(nrow(included.divisions) == 0)
    return( abv_out(boundary) %>%
              select(region.id, region.name, region.type) %>%
              mutate(n.polys = 0) )

  sub.polys <-
    lwgeom::st_split(boundary, included.divisions)

  area_filtered <-
    sub.polys %>%
    select(contains("region.")) %>%
    rmapshaper::ms_explode() %>%
    st_transform(4326) %>%
    mutate(area = st_geod_area(geometry)) %>%
    filter(as.numeric(area) > min.size)

  if(spatial.output) {
    out <-
      area_filtered %>%
      mutate(id = seq(1:nrow(.)))
    return(out)
  } else {
    n.polys <- length(area_filtered[[1]]) # length of geometry collection
    out <- abv_out(boundary) %>%
      select(region.id, region.name, region.type) %>%
      #left_join(data.frame(area_filtered))
      mutate(n.polys = n.polys)

    return(out)}
}

# example call:
'pmap_dfr(
  abv_out(geo.list$cz[1:3,])
  , ~polygonal.div(filter(geo.list[[..3]],
                          region.id == ..1),
                   filter(div,z
                          region.type == ..3,
                          region.id == ..1)
                   , div.types = c("I", "S", "U"))
  )
# or for full st df output
atl <- geo.list$cz[1,]
polygonal.div(atl,
              div = filter(div,
                           region.id == atl$region.id &
                             region.type == atl$region.type),
              spatial.output = T)
'
