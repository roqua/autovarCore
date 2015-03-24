context('autovar')

test_that('autovar function returns hello world',{
  expect_equivalent(autovar(),"Hello world!")
})
