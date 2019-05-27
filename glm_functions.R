get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

lm_formula <- function(variables.vec,
                       dependent = '',
                       interactions = F, 
                       quadratics = F,
                       non.num.vars = NA) {
  if (interactions) {
    collapse.symbol <- '*'
  } else {
    collapse.symbol <- '+'
  }
  if (quadratics) {
    quadratic.formula <- paste0('+' , paste0('I(',
                                             setdiff(variables.vec,
                                                     non.num.vars),
                                             '^2)',
                                             collapse = '+'))
  } else {
    quadratic.formula <- ''
  }
  
  as.formula(paste0(dependent, '~ ',
                    paste0(paste0(variables.vec, collapse = collapse.symbol),
                           quadratic.formula)))
}

get_truncated_spline = function(y, x, n_knots, poly_order){
  knots <- seq(min(x), max(x), length=n_knots)
  x_eval = seq(min(x), max(x), length=250)
  coef_list = list()
  knot_coef_list = list()
  x_eval_list =   list()
  knot_x_eval_list = list()
  
  for (i in 1:poly_order) {
    coef_list[[i]] = x^i
    x_eval_list[[i]] = x_eval^i
  }
  for (i in 1:(n_knots)) {
    knot_coef_list[[i]] = ((x - knots[i])^poly_order) * (x > knots[i])
    knot_x_eval_list[[i]] = ((x_eval - knots[i])^poly_order) * (x_eval > knots[i])
  }
  
  x_eval_df = data.frame(rep(1,250), x_eval_list, knot_x_eval_list)
  temp_df = data.frame(y, coef_list, knot_coef_list)
  colnames(x_eval_df) <- c('c', as.character(1:(poly_order + n_knots)))
  colnames(temp_df) <- c('profit', as.character(1:(poly_order + n_knots)))
  
  model_trunc <- lm(profit~., data=temp_df)
  a = x_eval_df[, !is.na(coef(model_trunc))]
  b = coef(model_trunc)[!is.na(coef(model_trunc))]
  fitted_trunc <- as.matrix(a)%*%b
  
  return(list(x_eval, fitted_trunc, model_trunc))
}

get_bsplines <- function(x, y, nrknots){
  minx <- min(x)-0.001
  maxx <- max(x)+0.001
  step <- (maxx-minx)/(nrknots-1)
  inner.knots <- seq(minx,maxx,length=nrknots)
  knots <- seq(minx-2*step,maxx+2*step,by=step)
  
  xseq <- seq(min(x),max(x),length=100)
  
  B <- spline.des(knots=knots, x, ord=3)$design
  Bfit <- spline.des(knots=knots, xseq, ord=3)$design
  
  betahat <- solve(t(B)%*%B)%*%t(B)%*%y
  fitted <- Bfit%*%betahat
  return(list(xseq, fitted))
}

get_lambda <- function(x, y, nrknots) {
  
  minx <- min(x)-0.001
  maxx <- max(x)+0.001
  step <- (maxx-minx)/(nrknots-1)
  inner.knots <- seq(minx,maxx,length=nrknots)
  knots <- seq(minx-3*step,maxx+3*step,by=step)
  
  D2 <- matrix(0,nrknots,nrknots+2)
  for(i in 1:nrknots) {
    D2[i,i] <- 1
    D2[i,i+1] <- -2
    D2[i,i+2] <- 1
  }
  K2 <- t(D2)%*%D2
  
  B <- spline.des(knots=knots,x,ord=4)$design
  
  lambda <- seq(1, 200, length=250)
  #lambda <- c(1:10 %o% 10^(-1:1.5))
  gcv <- rep(0,length(lambda))
  aic <- rep(0,length(lambda))
  bic <- rep(0,length(lambda))
  
  n <- length(x)
  
  for(i in 1:length(lambda)) {
    S <- B%*%solve(t(B)%*%B + lambda[i]*K2)%*%t(B)
    diags <- diag(S)
    trs <- mean(diags)
    df <- sum(diags)
    fit <- as.vector(S%*%y)
    gcv[i] <- mean(((y-fit)/(1-trs))^2)
    #  aic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + 2*df
    #  bic[i] <- n*log(sigma2) + sum((data$z-fit)^2)/sigma2 + log(n)*df
    sigma2 <- sum((y-fit)^2)/n
    aic[i] <- n*log(sigma2) + 2*(df+1)
    bic[i] <- n*log(sigma2) + log(n)*(df+1)
  }
  return(list('lambda' = lambda,
              'gcv' = gcv,
              'AIC' = aic,
              'BIC' = bic))
}


get_cubic_psplines <- function(x, y, nrknots, lambda) {
  minx <- min(x)-0.001
  maxx <- max(x)+0.001
  step <- (maxx-minx)/(nrknots-1)
  inner.knots <- seq(minx,maxx,length=nrknots)
  knots <- seq(minx-3*step,maxx+3*step,by=step)
  
  xplot <- seq(min(x),max(x),length=500)
  xobs <- unique(x)
  nunique <- length(xobs)
  
  D2 <- matrix(0,nrknots,nrknots+2)
  for(i in 1:nrknots)
  {
    D2[i,i] <- 1
    D2[i,i+1] <- -2
    D2[i,i+2] <- 1
  }
  K2 <- t(D2)%*%D2
  
  B <- spline.des(knots=knots, x , ord=4)$design
  Bobs <- spline.des(knots=knots, xobs, ord=4)$design
  Bplot <- spline.des(knots=knots, xplot, ord=4)$design
  
  betahat <- solve(t(B)%*%B + lambda*K2)%*%t(B)%*%y
  fitted <- B%*%betahat
  fittedplot <- Bplot%*%betahat
  return(list(xplot, fittedplot))
}
