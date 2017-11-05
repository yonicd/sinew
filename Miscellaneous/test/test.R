#some comment
yy <- function(a=4){
  head(runif(10),a)
  # a comment
}

zz <- function(v=10,a=8){
  head(runif(v),a)
}


yy(6)

zz(30,3)
