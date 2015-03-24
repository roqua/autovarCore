context('validate_params')

test_that('autovar function returns hello otherworld',{
  expect_equivalent(autovarCore:::validate_params(),"Hello otherworld!")
})
