test_that("t-test works", {
    expect_equal(my_t.test(c(1:120), "two.sided", 60)[["test_stat"]], 0.157459164, tolerance = .000005)
    expect_equal(my_t.test(c(1:120), "two.sided", 60)[["df"]], 119)
    expect_equal(my_t.test(c(1:120), "two.sided", 60)[["alternative"]], "two.sided")
    expect_equal(my_t.test(c(1:120), "two.sided", 60)[["p_val"]], 0.87514985, tolerance = .000005)
})
