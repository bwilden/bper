

# %notin% -----------------------------------------------------------------

test_that("%notin% does its job", {
  expect_true("a" %notin% c("b", "c"))
  expect_false("a" %notin% c("a", "b", "c"))
})
