testthat::context('make seealso')

testthat::describe('cutoff',{
  
  ret <- make_seealso(makeOxyFile,1)
  
  it('less than cutoff',expect_equal(ret,"\n#'  "))
  
})


testthat::describe('no cutoff',{
  
  ret <- make_seealso(quantile)

  it('no elements',expect_equal(ret,"\n#'  "))
  
  skip_on_cran()
  
  ret <- make_seealso(makeOxyFile)
  
  it('simple call',expect_true(grepl('link',ret[1])))
    
})
  
