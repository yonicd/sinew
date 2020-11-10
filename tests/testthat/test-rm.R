testthat::context('remove oxygen')

  testthat::describe('rm valid actions',{
    
    tf_oxy <- tempfile(pattern = 'baseline',fileext = '.R')
    
    file.copy(
      from = '../assets/baseline_oxy.R',
      tf_oxy,overwrite = TRUE)
    
    skip_if_not_rstudio()
    
    it('no show file',{
      
      rmOxygen(tf_oxy,showonexit = FALSE)
      
      testthat::expect_equal(
        readLines(tf_oxy),
        readLines('../assets/baseline_clean.R')
      )
    })
    
    it('show file',{
      
      rmOxygen(tf_oxy,showonexit = TRUE)
      
      testthat::expect_equal(
        readLines(tf_oxy),
        readLines('../assets/baseline_clean.R')
      )
      
    })
    
  })
  
  testthat::describe('rm invalid actions',{
    
    skip_if_not_rstudio()
    
    tf_txt <- tempfile(pattern = 'baseline',fileext = '.txt')
    
    file.copy(
      from = '../assets/baseline_oxy.R',
      tf_txt,overwrite = TRUE)
    
    it('extension',{
      
      testthat::expect_error(
        rmOxygen(tf_txt,showonexit = FALSE),
        regexp = 'not an .R file'
      )
      
    })
    
    it('path',{
      
      testthat::expect_error(
        rmOxygen(file.path(tempdir(),'baseline_oxy.png'),showonexit = FALSE),
        regexp = 'path to .R file'
      )
      
    })  
    
  })  
