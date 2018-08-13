testthat::context('remove oxygen')

  td_root <- tempdir()

  td <- file.path(td_root,'sinew_test')
  
  if(!dir.exists(td))
    dir.create(td,showWarnings = FALSE,recursive = TRUE)
  
  tf_oxy <- tempfile(pattern = 'baseline',fileext = '.R')
  tf_txt <- tempfile(pattern = 'baseline',fileext = '.txt')
  
  file.copy(
    from = system.file('tests/assets/baseline_oxy.R',package = 'sinew'),
    tf_oxy)
  
  file.copy(
    from = system.file('tests/assets/baseline_oxy.R',package = 'sinew'),
    tf_txt)
  
  testthat::describe('rm valid actions',{
    
    it('no show file',{
      
      rmOxygen(tf_oxy,showonexit = FALSE)
      
      testthat::expect_equal(
        readLines(tf_oxy),
        readLines(system.file('tests/assets/baseline_clean.R',package = 'sinew'))
      )
    })
    
    it('show file',{
      
      rmOxygen(tf_oxy,showonexit = TRUE)
      
      testthat::expect_equal(
        readLines(tf_oxy),
        readLines(system.file('tests/assets/baseline_clean.R',package = 'sinew'))
      )
      
    })
    
  })
  
  testthat::describe('rm invalid actions',{
    
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
