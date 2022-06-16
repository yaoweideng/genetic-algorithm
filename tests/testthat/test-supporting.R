# generate some test data to work with for our tests
out <- testdata(nvar = 6, rper = 1, npoints = 100, noisevar = 1, linkfunc = identity,
                dist = 'Gaussian', -10, 10)
x <- out[[1]]
fun <- AIC
reg <- glm
reg_fun <- Y ~ X1 + X2 + X3 + X4 + X5 + X6
family <- 'gaussian'
N <- 40
P <- 8*N+6
C <- ncol(x) - 1
n <- 3
bin <- rand_bin(P,C)


# test the logit function using what we know to be the true values of evaluating the expression
test_that("logit works", {
  expect_equal(logit(0), 0.5)
  expect_equal(logit(1000), 1)
  expect_equal(logit(-1000), 0)
})

# here we check that for a random given string, we will have length c
test_that("rand_bin works", {
  expect_equal(str_length(rand_bin(P, C)[sample(1:P, 1)]), C)
})


# test that generation gives binary strings stored as character strings
test_that("generation works", {
  test_generation <- generation(x, fun, reg, reg_fun, family, bin, gen_selection, gen_crossover, gen_mutate)
  expect_equal(str_length(test_generation[sample(1:326, 1)]), 6)
  expect_equal(is.character(test_generation), T)
})

# test that gen_mutate will return a binary string, same as above
test_that("gen_mutate works", {
  expect_equal(str_length(gen_mutate(bin)[1]), 6)
  expect_equal(is.character(gen_mutate(bin)), T)
})

# test that gen_crossover will return a binary string, same as above
test_that("gen_crossover works", {
  expect_equal(str_length(gen_crossover(bin)[1]), 6)
  expect_equal(is.character(gen_crossover(bin)), T)
})

# test that gen_selection will return a binary string, same as above
test_that("gen_selection works", {
  test_selection <- gen_selection(x, fun, reg, reg_fun, family, bin)
  expect_equal(str_length(test_selection[1]), 6)
  expect_equal(is.character(test_selection), T)
})

# test that col_filter will have type and dimensions that we expect
test_that("col_filter works", {
  cf_test <- lapply(bin,function(i){col_filter(x,i,reg_fun)})
  expect_equal(length(cf_test), 326)
  expect_equal(is.list(cf_test), T)
})

# test that fitness will return vector of numeric values of correct length
test_that("col_filter works", {
  fit_test <- lapply(bin,function(i){x_filtered = col_filter(x,i,reg_fun)
    data <- as.data.frame(x_filtered[1])
    dropped_cols <- unlist(x_filtered[2])
    fitness(data,fun, reg, reg_fun, family, dropped_cols)}) %>% unlist()
  expect_equal(length(fit_test), 326)
  expect_equal(is.numeric(fit_test), T)
})

