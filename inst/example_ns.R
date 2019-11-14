#some comment
yy <- function(a=4){
  utils::head(stats::runif(10),a)
  # a comment
}

zz <- function(v=10,a=8){
  utils::head(stats::runif(v),a)
}


yy(6)

zz(30,3)
