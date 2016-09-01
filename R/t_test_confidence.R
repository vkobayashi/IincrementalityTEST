#' Function to calculate Student-t confidence interval
#'
#' This function will calculate the Student-t confidence interval of a given sample of mean differences.
#' @param datawithID
#' @param confinter
#' @return Student-t confidence interval
#' @export

t_test_cf<-function(datawithID, confinter){
  pair_diff<-datawithID[,2]
  n<-length(pair_diff)
  qtile<-confinter/2
  tmp<- qt(qtile, n-1) * sd(pair_diff) / sqrt(n)
  list(confidence_interval=c(mean(pair_diff)+tmp, mean(pair_diff)-tmp), mean= mean(pair_diff),
       standard_error = sd(pair_diff)/sqrt(n))
}
