testthat::context('pretty')

sinew_opts$set(pretty_print=FALSE)

testthat::describe('switches',{
  
  it('force',{
    
    skip_on_travis()
    skip_if_not_rstudio()
    
    loadNamespace('git2r')
    
    x <- pretty_namespace(con = '../assets',force = list(stats='head'))
    
    testthat::expect_equal(sum(grepl('git2r',x)),0)
    testthat::expect_equal(sum(grepl('stats',x)),2)
    
  })
  
  it('ignore',{
    
    loadNamespace('git2r')
    
    x <- pretty_namespace(con = '../assets',ignore  = list(git2r='head'))
    
    testthat::expect_equal(sum(grepl('git2r',x)),0)
  })
  
})

testthat::describe('setup files',{
  ret <- pretty_setup('../assets',text = NULL)
  it('length',testthat::expect_length(ret,2))
  it('class',testthat::expect_true(inherits(ret,'list')))
  it('names',testthat::expect_equal(basename(names(ret)),c('baseline_clean.R','baseline_oxy.R')))
})

testthat::describe('setup file',{
  ret <- pretty_setup('../assets/baseline_clean.R',text = NULL)
  it('length',testthat::expect_length(ret,1))
  it('class',testthat::expect_true(inherits(ret,'list')))
  it('names',testthat::expect_equal(basename(names(ret)),c('baseline_clean.R')))
})

testthat::describe('setup text',{
  ret <- pretty_setup('../assets',text = 'aaa')
  it('length',testthat::expect_length(ret,1))
  it('class',testthat::expect_true(inherits(ret,'list')))
  it('names',testthat::expect_equal(names(ret),'txt1'))
})

testthat::describe('full text',{ 
  skip_on_cran()
  it('txt',{
    
    txt <- 'patient_summaries <- patient_demogs %>% 
  mutate(x = 
    paste0(x + paste0(2)) # script should ignore base ns
  ) %>%
  gather(cov, value) %>%
  group_by(cov) %>%
  mutate_at(vars(value), funs(q10, q25, q50, q75, q90))'
    
    x <- pretty_namespace(text = txt,ask = FALSE)
    
    testthat::expect_equal(
      nrow(x),
      8)
    
  })
})

testthat::describe('full file',{
  skip_on_cran()
  it('file',{
    x <- pretty_namespace(con = '../assets')
    
    testthat::expect_equal(
      length(x),
      2)
    
    testthat::expect_equal(
      nrow(x[[1]]),
      7)
    
    testthat::expect_equal(
      nrow(x[[2]]),
      7)
    
  })
})

sinew_opts$restore()