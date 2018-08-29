testthat::context('pretty')

sinew_opts$set(pretty_print=FALSE)

testthat::describe('full',{
  
  it('file',{
    x <- pretty_namespace(con = '../assets')
    
    testthat::expect_equal(
      length(x),
      2)
    
    testthat::expect_equal(
      length(x[[1]]),
      13)
    
    testthat::expect_equal(
      length(x[[2]]),
      42)
    
  })
  
  
  it('txt',{
    
    txt <- 'patient_summaries <- patient_demogs %>% 
  mutate(x = 
    paste0(x + paste0(2)) # script should ignore base ns
  ) %>%
  gather(cov, value) %>%
  group_by(cov) %>%
  mutate_at(vars(value), funs(q10, q25, q50, q75, q90))'
    
    x <- pretty_namespace(text = txt)
    
    testthat::expect_equal(
      length(x),
      7)
    
  })
})


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

sinew_opts$restore()