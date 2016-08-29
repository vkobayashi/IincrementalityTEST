#' Construct a dataframe containing the ab test id and the differences of the specified metric
#'
#' This function caculates the differences of the specified metrics (e.g. RPU, AOV, etc.).
#' @param mydata A Dataframe containg the  variables "iabtest_id", "abt_group", and the metric of choice.
#' @param metric Chosen metric e.g. RPU, AOV, TPB, and BR
#' @return Data frame contaning the differences of the specified metrics for the control and test across all ab tests
#' @export
test_metric<-function(mydata, metric){

dataM<-mydata[,c("iabtest_id",metric, "abt_group")]

abtgroup0<-dataM[dataM$abt_group==0,]
abtgroup1<-dataM[dataM$abt_group==1,]

dataM2<-merge(abtgroup0,abtgroup1, by=("iabtest_id"))
inc_name = paste("inc",metric, sep="_")
dataM2$inc_metric<-dataM2[,2] - dataM2[,4]
dataM2<-rename(dataM2, c(inc_metric = inc_name))
dataM2<-dataM2[, c("iabtest_id", inc_name)]

return(dataM2)
}
