#' Select Method of Genetic Algorithm
#' @description Variable selection using genetic algorithm.
#' @param x a data frame
#' @param reg regression type
#' @param reg_fun regression formula
#' @param family
#' @param fun fitness function. the default is AIC.
#' @param ngen max number of generations. the default is 40.
#' @param sel generation selection function. a default function is provided, but the user may choose to use a different method.
#' @param cross crossover function. a default function is provided, but the user may choose to use a different method.
#' @param mut mutate function. a default function is provided, but the user may choose to use a different method.
#' @return Principal components and restricted data set.
#' @examples
#' x <- testdata(nvar = 6, rper = 1, npoints = 100, noisevar = 1, linkfunc = identity, dist = 'Gaussian', -10, 10)
#' reg <- glm
#' reg_fun <- Y ~ X1 + X2 + X3 + X4 + X5 + X6
#' family <- "gaussian"
#' fun <- AIC
#'
#' select(x, reg, reg_fun, family, fun)
#'
#' reg <- lm
#' fun <- BIC
#'
#' select(x, reg, reg_fun, family, fun)

select = function(x, reg, reg_fun, family = "gaussian", fun = AIC, ngen=40, sel = gen_selection, cross = gen_crossover, mut = gen_mutate){

  ## check inputs to make sure they are the right type
  if(!is.data.frame(x)) {
    stop("Error: Your data must be in the form of a dataframe.")
  }
  if(!typeof(reg)=="closure") {
    stop("Error: The regression function needs to be a closure.")
  }
  if(!class(reg_fun)=="formula") {
    stop("Error: The reg_fun input is not a valid formula.")
  }
  if(!typeof(fun)=="closure") {
    stop("Error: The fitness function needs to be a closure.")
  }
  if(!ngen%%1==0) {
    stop("Error: The generation number has to be a whole value.")
  }
  if(!typeof(sel)=="closure") {
    stop("Error: The selection function needs to be a closure.")
  }
  if(!typeof(cross)=="closure") {
    stop("Error: The crossover function needs to be a closure.")
  }
  if(!typeof(mut)=="closure") {
    stop("Error: The mutation function needs to be a closure.")
  }

  ## max number of generations
  N = ngen

  ## population, i.e. number of binary strings
  P = 8*N+6

  ## number of dependent variables
  C = ncol(x) - 1

  ## number of consecutive same results before stopping algorithm
  n = 3

  bin  = rand_bin(P,C)

  ## run generations
  done = FALSE
  same = 0
  count = 0
  bin_final_prev = "2"


  while(!done && N>count) {
    count = count + 1
    #print(paste("Gen",count))

    ## skip crossover and mutation on last round to maximize convergence odds
    if (N>count) {
      bin = generation(x, fun, reg, reg_fun, family, bin,sel,cross,mut)
    } else {
      bin = generation(x, fun, reg, reg_fun, family, bin,sel,cross,mut,TRUE)
    }

    ## remove 1 pairing each generation
    bin = tail(bin,-4)
    bin_final_new = tail(bin,1)

    ## stop if n straight of the same combination
    if (bin_final_new == bin_final_prev) {
      same = same + 1
      if (same >= n) {
        done = TRUE
      }
    } else {
      same = 0
    }
    bin_final_prev = bin_final_new

  }

  return(c(bin_final_new,col_filter(x,bin_final_new,reg_fun)[1]))
}
