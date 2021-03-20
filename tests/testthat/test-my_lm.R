test_that("fitting linear model works", {
    expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)[1, 1], 47.888516, tolerance = .000005)
    expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)[1, 2], 0.33980532, tolerance = .000005)
    expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)[1, 3], 140.929273, tolerance = .000005)
})
