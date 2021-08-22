# https://github.com/yonicd/sinew/issues/71
ganyani_cdf <- function(which) {
  
  gi_params <- ganyani_gi(which)
  
  beta <- gi_params$mean$est / (gi_params$sd$est ^ 2)
  alpha <- gi_params$mean$est * beta
  
  gi_cdf <- function(days) {
    pgamma(days, alpha, beta)
  }
  
  gi_cdf
  
}
