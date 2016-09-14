#' Helper function for obtaining the most frequent day of purchase.
#'
#' This function computes the most frequent day of purchase.
#' @param x Vector of data.
#' @return Most frequent day of purchase.
#' @export

fnFreqDay<-function(x) {out<-tryCatch(
  {
    names(which(table(x) == max(table(x)))[1])
  },
  warning= function(cond){ return(NA)
  })
return(out)
}

#' Helper function for obtaining the maximum.
#'
#' This function computes the maximum.
#' @param x Vector of data.
#' @return Maximum value.
#' @export

fnMax<-function(x) {out<-tryCatch(
  {
    max(x,na.rm=TRUE)
  },
  warning= function(cond){ return(NA)
  })
return(out)
}

#' Helper function for obtaining the average.
#'
#' This function computes the average.
#' @param x Vector of data.
#' @return Average value.
#' @export

fnAve<-function(x) {
  if(sum(is.na(x))!=length(x)) {return(mean(x,na.rm=TRUE))
  } else {return(NA)}
}

#' Helper function for obtaining the sum.
#'
#' This function computes the sum.
#' @param x Vector of data.
#' @return Sum value.
#' @export

fnSum<-function(x) {
  if(sum(is.na(x))!=length(x)) {return(sum(x,na.rm=TRUE))
  } else {return(NA)}
}
