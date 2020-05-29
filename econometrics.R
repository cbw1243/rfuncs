# This function calculates Manalanobis distance for the matched data by the Match function in the Matching package.
# The inverse variance is used as the scaling matrix. 

getDist <- function(model, XM){
  stopifnot(class(model) == 'Match')
  match_tr_data <- XM[model$index.treated,]
  match_cn_data <- XM[model$index.control,]
  
  inv.cov <- matrix(1/apply(XM, 2, var), nrow = 1)
  xdiffs <- as.matrix((match_tr_data - match_cn_data)^2)
  
  out <- c(tcrossprod(xdiffs, inv.cov))
  return(out)
}