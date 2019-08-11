library(testthat)
library(Replicate)


###################### EVALUE: ANNALS PAPER EXAMPLES ###################### 

test_that("Should reject bad inputs in pred_int", {

  expect_error( pred_int( orig.y = .5, orig.vy = .2, rep.y = c( .1, .2 ), rep.vy = c( .2 ) ) )
  
  expect_error( pred_int( orig.y = .5, orig.vy = c(.2, .1), rep.y = c( .1, .2 ), rep.vy = c( .2, .1 ) ) )
  
  expect_error( pred_int( orig.y = c(.5, .3), orig.vy = c(.2, .4), rep.y = .1, rep.vy = .2 ) )
  
  rep.y = c(.1, -2, 0.5, -0.8)
  pi = pred_int( orig.y = .6, orig.vy = .4, rep.y = rep.y, rep.vy = c(.2, .2, 0.4, .001) )
  inside = ( rep.y < pi$int.hi ) & ( rep.y > pi$int.lo )
  
  expect_equal( inside, pi$rep.inside )
  
})
