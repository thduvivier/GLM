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
