testthat::context('untangle')

test_file <- system.file('untangle_test.R',package = 'sinew')
td <- file.path(tempdir(),'untangle')
invisible({
  lapply(c('text','file'), function(x, td){
    dir.create(file.path(td,x),recursive = TRUE,showWarnings = FALSE)
  }, td = td)
})

txt <- "#some comment
yy <- function(a=4){
 head(runif(10),a)
 # a comment
}
#'
v <- 20
#'
#another comment
zz <- function(v=10,a=3){
 head(runif(v),pmin(a,v))
}
#'
zz(v)
#'
"

# create test data
set.seed(123)
x <- 1:30
n <- sample(2:5,1)
s <- split(x, sort(rep_len(1:n, length(x))))
f <- function(x,l) seq.int(x, x + l)
s_sub <- lapply(c(s,s),function(x){
  f(x[sample(seq(floor(length(x)/2)),1)],sample(2:4,1))
})

s_bind <- append(s,s_sub)

testthat::describe('parent functions',{
  it('find the parents',{
    testthat::expect_equal(s,sinew:::find.parents(s_bind))
  })
  
  it('parent indicies',{
    testthat::expect_equal(1:4,sinew:::which.parents(s_bind))
  })
})

testthat::describe('untangle inputs',{
  
  it('empty',{
    testthat::expect_null(untangle())
  })
  
  untangle(text = txt,dir.out = file.path(td,'text'))
  it('text functions',{
    testthat::expect_equal(list.files(file.path(td,'text')),
                           c('yy.R','zz.R'))
  })
  
  it('text body',{
    testthat::expect_equal(list.files(file.path(td),pattern = '.R$'),
                           c('body.R'))
  })

  untangle(test_file,dir.out = file.path(td,'file'))
  it('file',{
    testthat::expect_equal(
      list.files(file.path(td,'file')), 
      "ganyani_cdf.R"
      )
  })
  
})

unlink(td,recursive = TRUE,force = TRUE)
