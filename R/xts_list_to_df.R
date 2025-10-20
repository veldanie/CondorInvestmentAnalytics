
#' List of xts objects to DF.
#'
#' Takes list and returns aggregated xts objetc
#' @param xts_list list of xts objects.
#' @param join join arg, (inner, outer).
#' @return xts
#' @export


xts_list_to_df <- function(xts_list, join='inner'){
  ids <- names(xts_list)
  series_df <- NULL
  for(id in ids){
    series_df <- merge.xts(series_df, xts_list[[id]], join = join)
  }
  colnames(series_df) <- ids
  return(series_df)
}
