library(dygraphs)
convertTimeSeries <- function(month, data){
    set.seed(12) 
  dummy.df <- as.data.frame(matrix(round(rnorm(1200), digits = 2),
                                   nrow = 100, ncol = 12))
  rownames(dummy.df) <- seq(1901, 1)
  colnames(dummy.df) <- month.abb
  dummy.df.ts <- ts(as.vector(t(as.matrix(dummy.df))), 
                    start=c(1901,1), end=c(2000,12), frequency=12)
  

}