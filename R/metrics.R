#' Function to calculate RPU, AOV, BR, and TPB
#'
#' This function will calculate the four metrics commonly use in incrementality testing.
#' @param nb_transactions The number of transactions
#' @param nb_users The number of unique users
#' @param revenue The revenue
#' @param nb_buyers The number of buyers
#' @return RPU The Revenue per user
#' @return AOV The Average order value
#' @return BR The Buyer rate
#' @return TPB The Transactions per buyer
#' @export
#' @examples
#' incrementality_metrics(nb_transactions = 11278, nb_users=297073, revenue= 279480.4, nb_buyers=10909)

incrementality_metrics <- function(nb_transactions, nb_users, revenue, nb_buyers){
  list(RPU = revenue /nb_users, BR = nb_buyers/nb_users, AOV = revenue/nb_transactions,
       TPB = nb_transactions/nb_buyers)
}
