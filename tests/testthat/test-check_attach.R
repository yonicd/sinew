testthat::context('check_attach')

testthat::describe('test check attach',{
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  nenv <- new.env()
  
  sinew:::check_attach('testthat::test_dir',nenv)
  
  it('already loaded',testthat::expect_true(length(ls(nenv))==0))
  
  unloadNamespace('ggplot2')
  
  sinew:::check_attach('ggplot2::aes',nenv)
  
  it('not already loaded',testthat::expect_true('package:ggplot2'%in%nenv$toUnload))
})