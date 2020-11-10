testthat::context('tabular')

testthat::describe('convert dataframe to tabular header',{
  
  tab <- tabular(mtcars)
  
  it('length',testthat::expect_equal(nchar(tab),3495))
  it('class',testthat::expect_true(inherits(tab,'character')))
     
})

testthat::describe('convert dataframe to tabular no header',{
  
  tab <- tabular(mtcars,header = FALSE)
  
  it('length',testthat::expect_equal(nchar(tab),3291))
  
  
})