
#' Transaction costs
#'
#' Basis points slippage, and BP commission per traded ammount. Future versions will contain other slippage models as we can have access to more data, e.g. trading volumes, number of trades.
#' @param slippage Fixed percentage over asset price.
#' @param commission Percentage commission.
#' @param purchase Purchase indicator.
#' @return Cost per transaction.
#' @export

transaction_costs <- function(units, price, slippage = 5, commission = 5, purchase = TRUE) {

  exec_price <- price + (2 * purchase - 1) * price * slippage / 10000
  commission_cost <- units * exec_price * commission / 10000
  return(list(exec_price = exec_price, commission_cost = commission_cost))
}
