#' Statistic for the Bootstrap
#'
#' This function defines the statistic for the boostrap.
#' @param datadiff Differences of the chosen metric
#' @param indices The indices of the sampled rows
#' @return Bootstrap mean and standard error
#' @export

incrementality_func<-function(datadiff, indices){
  d<-datadiff[indices]
  c(mean(d), var(d)/length(d))
}

#' Hypothesis Testing Bootstrap
#'
#' This function construct the sampling distribution of a metric under a null effect.
#' @param datadiff Differences of the chosen metric centered from the mean of the difference
#' @param indices The indices of the sampled rows
#' @return Bootstrap mean and standard error
#' @export

incrementality_func_hypothesis<-function(datadiff, indices){
  dwinsor<-winsor(datadiff, trim=0.01)
  d<-dwinsor[indices]
  c(mean(d), var(d)/length(d))
}

#' Boostrap routine
#'
#' This function runs the bootstrap routine.
#' @param mydatadiff Contains the differences
#' @param nb_boot Number of bootstrap samples
#' @return Bootstrap object from package "boot"
#' @export

res_boot<-function(mydatadiff, statisticfunc, nb_boot){
  return(boot(mydatadiff, statistic=statisticfunc, R=nb_boot))
}
