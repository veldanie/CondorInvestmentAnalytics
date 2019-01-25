
#' Dissagregate funds
#'
#' Dissagregates funds into assets.
#' @param funds Vector of funds.
#' @param benchmarks DF benchmarks.
#' @return Vector of assets.
#' @export

disagg_funds <- function(funds, benchmarks, other_assets = NULL) {
  if(!all(funds %in% benchmarks$Benchmark)){stop("fund(s) not available in benchmarks!")}

  assets <- NULL
  for (fi in funds){
    assets <- c(assets, benchmarks %>% filter(Benchmark==fi) %>% pull(Asset))
  }
  return(unique(c(assets, other_assets)))
}
