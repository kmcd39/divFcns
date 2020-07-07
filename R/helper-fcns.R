#' abv_out
#'
#' abbreviates a spatial df; dropping geometry
#' @export
abv_out <- function(sf_df) {
  require(dplyr)
  sf_df %>%
    data.frame() %>%
    select(-geometry)
}



#' names_from_col
#'
#' extract unique values from a particular column.
#' Can be used in conjunction with dplyr::group_split to give df list reasonable names
#' I.e; \code{group_split(df, col) }
#' \code{names(df) <- names_from_col(col) }
#' @export
names_from_col <- function(df, col_name) {
  map(df,
      ~unique(pull(., col_name)))
}

#' build_identifier_df
#'
#' Helper function for fcns that split list of division types x regions. Designed for retaining
#' identifiers; i.e., cz & cz_name for regions.
#' @export
build_identifier_df <- function(df, split_cols) {

  ids <-
    suppressMessages(
      map_dfc(split_cols,
              ~unique(data.frame(pull(df, .)))))

  colnames(ids) <- split_cols
  return(ids)
}

