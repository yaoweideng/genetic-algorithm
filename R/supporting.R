## filters data set x, by '1' columns of binary string, str. Also returns list of removed columns
col_filter = function(x,str,reg_fun) {
  vars = colnames(x)
  indep = str_extract(format(reg_fun),"[:alnum:]+(?=[:space:]*~)") %>% na.omit()
  x_indep = x %>% dplyr::select(indep)
  dep = vars[vars != indep]
  x_dep = x %>% dplyr::select(dep)
  str_list = as.logical(as.numeric(unlist(strsplit(str,""))))
  str_list_neg = !str_list
  dropped_cols = dep[str_list_neg]
  new_x = as.data.frame(x_dep[,str_list])
  new_x = cbind(x_indep,new_x)
  colnames(new_x) = c(indep,dep[str_list])
  return(list(new_x,dropped_cols))
}

## fitness of a set of data using  fitness function, regression type, regression formula, and a list of dropped cols. Also requires list of vars to ignore
fitness = function(x, fun, reg, reg_fun, family, dropped_cols) {
  if (length(dropped_cols>0)) {
    reg_fun_new = lapply(dropped_cols,function(i){paste("-",i)}) %>% unlist() %>% paste(collapse=" ")
    reg_fun_new = paste("~  . ",reg_fun_new,collapse = "") %>% as.formula()
    reg_fun = update(reg_fun,reg_fun_new)
  }

  return(fun(reg(formula = reg_fun, family = family, data = x)))
}

## selection takes in data, fitness function, regression type, regression formula, binary object, returns binary object
gen_selection = function(x, fun, reg, reg_fun, family, bin){
  P = length(bin)

  ## assigning selection probabilities to rows
  fit = lapply(bin,function(i){x_filtered = col_filter(x,i,reg_fun)
    data = as.data.frame(x_filtered[1])
    dropped_cols = unlist(x_filtered[2])
    fitness(data,fun, reg, reg_fun, family, dropped_cols)}) %>% unlist()
  bin_fit = as.data.frame(cbind(unlist(bin),fit))
  bin_fit$fit = as.numeric(bin_fit$fit)
  bin_fit = bin_fit[order(bin_fit$fit,decreasing = TRUE),]
  bin_fit = mutate(bin_fit,prob = 2*(1:P)/(P*(P+1)))
  bin_fit = mutate(bin_fit,csum = cumsum(prob))

  new_bin = data.frame(matrix(nrow=P,ncol=2))
  colnames(new_bin) = c("bin","fit")

  ## generate new binary object based on probabilities
  for (i in 1:P) {
    u = runif(1)

    a = 0
    index = 0
    if (u<bin_fit[1,4]) {
      new_bin[P,1] = bin_fit[1,1]
      new_bin[P,2] = bin_fit[1,2]
      a = 1
    }

    while(a==0) {
      index = index + 1
      if (u >= bin_fit[index-1,4] && u <= bin_fit[index,4]) {
        new_bin[i,1] = bin_fit[index,1]
        new_bin[i,2] = bin_fit[index,2]
        a = 1
      }
    }
  }

  ## re-sort by fit
  new_bin = new_bin[order(new_bin$fit,decreasing = TRUE),]

  return(new_bin[,1])
}

## crossover takes in binary object, returns binary object
## CURRENTLY REQUIRES P = EVEN
gen_crossover = function(bin) {
  p = length(bin)

  ## swap takes in two binary strings and returns their offspring, combined into a list
  swap = function(x,y) {
    c = nchar(x)

    ## index of random split point
    spl = sample(1:c,1)
    x1 = substr(x,0,spl)
    x2 = substr(x,spl+1,c)
    y1 = substr(y,0,spl)
    y2 = substr(y,spl+1,c)
    x = paste0(x1,y2)
    y = paste0(y1,x2)
    return(c(x,y))
  }

  new_bin = c()

  ## perform swap on adjacent rows of binary object
  for (i in seq(1,p,2)) {
    new_bin = c(new_bin,
                swap(bin[i], bin[i+1]))
  }
  return(new_bin)
}

## mutation takes in binary object, returns binary object
gen_mutate = function(bin) {
  p = length(bin)

  ## mutate a single binary string, currently at rate 1/C
  mutate_one = function(x) {
    c = nchar(x)

    ## swap 1's for 0's and vice versa
    toggle = function(a) {
      if (a=="1") {
        return("0")
      } else {
        return("1")
      }
    }

    for (i in 1:c) {
      if (runif(1,min=0,max=1)<(1/c)) {
        x = paste0(substr(x,1,i-1),
                   toggle(substr(x,i,i)),
                   substr(x,i+1,c))
      }
    }
    return(x)
  }

  new_bin = lapply(bin,mutate_one)

  return(unlist(new_bin))
}


## takes in data, fitness function, regression type, regression formula, binary object, returns binary object
generation = function(x, fun, reg, reg_fun, family, bin, sel, cross, mut, last=FALSE){
  select_bin = sel(x, fun, reg, reg_fun, family, bin)
  if (!last) {
    crossover_bin = cross(select_bin)
    mutate_bin = mut(crossover_bin)
    return(mutate_bin)
  } else {
    return(select_bin)
  }

  return(mutate_bin)
}

## Link function used for drawing logistic regression data
logit <- function(z) {
  return(1/(1+exp(-z)))
}

## Returns test data with correct relevant variables and a bin object
testdata = function(nvar,rper,npoints,noisevar,linkfunc,dist,minx,maxx){ 

  # True Response/Related Predictors
  beta = sample(1:5,nvar*rper,replace=TRUE)
  beta0 = sample(1:5,1,replace=TRUE)
  X_selected = matrix(runif(npoints*(nvar*rper),min=minx,max=maxx),ncol=(nvar*rper))
  
  # Linear Response
  Y_nonoise = X_selected%*%beta + beta0
  
  # Unrelated Predictors
  if(rper != 1) {
    ncol = ceiling(nvar*(1-rper))
    X = cbind(X_selected,matrix(runif(npoints*ncol,min=minx,max=maxx),ncol=ncol))
  }
  else {
    X = X_selected
  }
  
  ##Types of Regressions
  Y = Y_nonoise + rnorm(length(Y_nonoise),0,noisevar)
  Y = linkfunc(Y)
  
  if(dist == 'Poisson') {
    #Note no error term due to poisson variance
    print("With poisson regression noisevar will be ignored")
   for(i in 1:length(Y)) {
      Y[i] = rpois(1,Y_nonoise[i])
    }
  }
  else if(dist == 'Bernoulli') {
    for(i in 1:length(Y)) {
      Y[i] = rbinom(1,1,Y[i])
    }
  }
  else if(dist == 'Gaussian') {
    Y = Y
  }
  else {
    stop("Error: Invalid distribution")
  }
    
  #Collect into Dataframe
  Y = as.data.frame(Y)
  X = as.data.frame(X)
  
  #Scramble relavent variable placements
  indexscramble = sample(1:nvar,nvar,replace=FALSE)
  X = X[indexscramble]
  bin = c(as.character(rep(1,nvar*rper)),as.character(rep(0,(nvar-nvar*rper))))
  bin = bin[indexscramble]
  colnames(X) = paste0("X",as.character(c(1:nvar)))
  colnames(Y) = c("Y")
  
  #Return "true" variables and data
  bin = paste(bin, collapse = '')
  data = cbind(X,Y)
  beta = c(beta[indexscramble])

  return(list('data'=data,'bin'=bin,'beta'= beta,'beta0' = beta0))
}

## generate starting binary object
rand_bin = function(p,c) {
  bin = c()
  for (i in 1:p) {
    str = paste0(sample(0:1,c,replace = TRUE),collapse = "")
    bin = c(bin,str)
  }
  return(bin)
}
