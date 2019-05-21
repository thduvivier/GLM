# Authors: Bruno Esposito and Thomas Duvivier
rm(list = ls())

library(openxlsx)
library(corrplot)
library(stargazer)
library(splines)

# Set up ----
#setwd('/Users/bruno_esposito/Dropbox/KUL/Courses/2nd Semester/Generalized Linear Models/Assignment2')
setwd('D:/Dropbox/KUL/Courses/2nd Semester/Generalized Linear Models/Assignment2')

# Open dataset ----
raw_df = read.xlsx('./data/IMDb.xlsx')
head(raw_df)

selected_vars = c('profit', 'budget', 'director_facebook_likes',
                  'content_rating', 'duration')
df = raw_df[, selected_vars]
df$content_rating = factor(df$content_rating)

head(df)

# 1. Descriptive stats ----
stargazer(df)
summary(df)
pairs(df, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})
corrplot(cor(df[, !(colnames(df) %in% c('content_rating'))]))

model1 = lm(profit ~ budget + director_facebook_likes + content_rating, data=df)
summary(model1)

par(mfrow = c(2,2))
plot(model1)
anova(model1)

df_ord = df[order(df$budget), ]
model_loess <- loess(profit ~ budget, span = 2/3, degree = 2, data=df_ord)
par(mfrow = c(1,1))
plot(df_ord$budget, df_ord$profit)
lines(df_ord$budget, predict(model_loess), col='red')


# TODO: 4 plot of profit vs covariates

# Model profit - budget ----
# Check nonlinearity
df$budget.class1 <- as.numeric(I(df$budget <= 50))
df$budget.class2 <- as.numeric(I(df$budget > 50 & df$budget <= 100))
df$budget.class3 <- as.numeric(I(df$budget > 100 & df$budget <= 150))
df$budget.class4 <- as.numeric(I(df$budget > 150 & df$budget <= 200))
df$budget.class5 <- as.numeric(I(df$budget <= 250))

model_discrete = lm(profit ~ budget.class1 + budget.class2 + budget.class3 + 
                      budget.class4 + budget.class5, data = df)
summary(model_discrete)
plot(sort(df$budget), predict(model_discrete)[order(df$budget)], type='l')
#lines(sort(df$budget), predict(model_discrete)[order(df$budget)])


# Polynomial 
model_poly = list()
model_poly[[1]] = lm(profit ~ budget, df)
temp_formula = 'profit ~ budget'
for (i in 2:10) {
  temp_formula = paste(temp_formula, ' + I(budget^', i, ')', sep='')
  model_poly[[i]] = lm(formula(temp_formula), df)
}

out_bic = unlist(lapply(model_poly, BIC))
out_aic = unlist(lapply(model_poly, AIC))

out_bic
out_aic

# Order 1 is the best model 
par(mfrow=c(1,1))
plot(df$budget, df$profit)
abline(model_poly[[1]], col='red')



# Truncated polynomial splines
get_truncated_spline = function(y, x, n_knots, poly_order){
  knots <- seq(min(df$budget), max(df$budget), length=n_knots)
  x_eval = seq(min(df$budget), max(df$budget), length=250)
  coef_list = list()
  knot_coef_list = list()
  x_eval_list =   list()
  knot_x_eval_list = list()
  
  
  for (i in 1:poly_order) {
    coef_list[[i]] = df$budget^i
    x_eval_list[[i]] = x_eval^i
  }
  for (i in 1:(n_knots)) {
    knot_coef_list[[i]] = ((df$budget - knots[i])^poly_order) * (df$budget > knots[i])
    knot_x_eval_list[[i]] = ((x_eval - knots[i])^poly_order) * (x_eval > knots[i])
  }
  
  x_eval_df = data.frame(rep(1,250), x_eval_list, knot_x_eval_list)
  temp_df = data.frame(df$profit, coef_list, knot_coef_list)
  
  colnames(x_eval_df) <- c('c', as.character(1:(poly_order + n_knots)))
  colnames(temp_df) <- c('profit', as.character(1:(poly_order + n_knots)))
  head(temp_df)
  head(x_eval_df)
  
  model_trunc <- lm(profit~., data=temp_df)
  
  # FIX NAs HERE
  fitted_trunc <- as.matrix(x_eval_df)%*%coef(model_trunc)
  return(list(x_eval, fitted_trunc, model_trunc))
}

trun_splineA = get_truncated_spline(df$profit, df$budget, n_knots=2, poly_order=2)
trun_splineB = get_truncated_spline(df$profit, df$budget, n_knots=3, poly_order=2)
trun_splineC = get_truncated_spline(df$profit, df$budget, n_knots=5, poly_order=2)
par(mfrow=c(1,1))
plot(df$budget, df$profit)
lines(trun_splineA[[1]], trun_splineA[[2]], lwd=2, col="blue", lty='solid')
lines(trun_splineB[[1]], trun_splineB[[2]], lwd=2, col="red", lty='dashed')
lines(trun_splineC[[1]], trun_splineC[[2]], lwd=2, col="darkgreen", lty='dotted')
legend('topright', c('2 knots', '3 knots', '5 knots'), 
       col=c('blue', 'red', 'darkgreen'), 
       lty=c('solid', 'dashed', 'dotted'))



# B-splines

nrknots <- 3
minx <- min(df$budget)-0.001
maxx <- max(df$budget)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-2*step,maxx+2*step,by=step)

xseq <- seq(min(df$budget),max(df$budget),length=10)

B <- spline.des(knots=knots, df$budget, ord=3)$design
Bfit <- spline.des(knots=knots, xseq, ord=3)$design

x <- df$budget
y <- df$profit

plot(x, y,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)

nb <- ncol(Bfit)
for (ib in 1:nb){  
  lines(xseq, Bfit[,ib], lwd=2, col="steelblue") 
}


betahat <- solve(t(B)%*%B)%*%t(B)%*%y
fitted <- Bfit%*%betahat

plot(x, y,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)

Bfitscaled <- Bfit
for(i in 1:(nrknots+1)) {
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]
}

nb <- ncol(Bfit)
for (ib in 1:nb)
{  lines(xseq,Bfitscaled[,ib],lwd=2,col="steelblue") }


plot(x, y,xlab="",ylab="",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
lines(xseq,fitted,lwd=2,col="steelblue")

