library(readxl) 
library(ggplot2)
library(reshape2)µ
library(MASS)
library(splines)


setwd("C:/Users/duviv/Documents/University/KUL/S2/Abandoned/Generalized Linear Models/Group work")
df <- read_xlsx("IMDb.xlsx")
df <- df[,c("profit", "budget", "director_facebook_likes", "content_rating")]
df$content_rating <- as.factor(df$content_rating)

# 1. Descriptive ----

df <- df[,-4]
# 1.a Correlation Heatmap
cormat <- round(cor(df, method = "pearson", use = "complete.obs"), 2)
cormat
# cormat <- round(cor(df, method = "pearson", use = "complete.obs"), 2)
cormat
melted_cor <- melt(cormat)

library(ggplot2)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Plots with profit----
fit1 <- lm(profit ~ budget, data = df)
fit2 <- lm(profit ~ budget + I(budget^2), data = df)
fit3 <- lm(profit ~ budget + I(budget^2) + I(budget^3), data = df)

pdf("./profit-budgete2.pdf", width=7, height=4, pointsize=10)
plot(df$budget, df$profit, xlab = "budget", ylab="profit")
lines(df$budget, predict(fit1), col="red")
lines(df$budget, predict(fit2), col="green")
lines(df$budget, predict(fit3), col="blue")
legend(230, 350, c("Degree 1", "Degree 2", "Degree 3"), lty = 1, col = c("red", "green", "blue"))
dev.off()

model_poly = list()
model_poly[[1]] = lm(profit ~ budget, df)
temp_formula = 'profit ~ budget'
for (i in 2:10) {
  temp_formula = paste(temp_formula, ' + I(budget^', i, ')', sep='')
  model_poly[[i]] = lm(formula(temp_formula), df)
}


pdf("./profit-budgete-poly.pdf", width=7, height=4, pointsize=10)
plot(df$budget, df$profit, xlab = "budget", ylab="profit")
lines(sort(df$budget), fitted(model_poly[[1]])[order(df$budget)], col='red') 
lines(sort(df$budget), fitted(model_poly[[3]])[order(df$budget)], col='green') 
lines(sort(df$budget), fitted(model_poly[[10]])[order(df$budget)], col='blue') 
legend(230, 350, c("Degree 1", "Degree 3", "Degree 10"), lty = 1, col = c("red", "green", "blue"))
dev.off()



# 2. Model the profit as a function of budget only, use the following techniques:----
# Covariates used in this analysis are content_rating, budget, director_facebook_likes.

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

#  2.a Polynomial regression model----
fit1 <- lm(profit ~ budget + I(budget^2), data = df)
summary(fit1)
plot(df$profit ~ df$budget)
boxplot(df$profit)

#  2.b Truncated polynomial splines of degree 2 (consider k=2, 3 and 5 knots)----
range(df$budget)
knots1 <- c(8,300)
knots2 <- c(8, 146, 300)
knots3 <- c(8, 77, 146, 223, 300)

# k = 2
b1 <- df$budget
b2 <- df$budget^2
b3 <- (df$budget-knots1[1])^2*(df$budget > knots1[1])
lm3 <- lm(profit ~ b1 + b2 + b3, data = df)
summary(lm3)

b_eval <- matrix(0,250,12)
b_eval[,1] <- rep(1,250)
b_eval[,2] <- age_eval
b_eval[,3] <- age_eval^2
b_eval[,4] <- (age_eval-knots[2])^2*(age_eval > knots[2])
....
b_eval[,12] <- (age_eval-knots[10])^2*(age_eval > knots[10])
fitted3 <- round(b_eval%*%coef(lm3),5)

#  2.c B-splines of degree 2 (consider m=3, 5 and 8 knots)----

# B-splines

# 3 knots

nrknots <- 3
minx <- min(df$budget)-0.001
maxx <- max(df$budget)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-2*step,maxx+2*step,by=step)

xseq <- seq(min(df$budget),max(df$budget),length=100)

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
lines(xseq,fitted,lwd=2,col="steelblue")


# 5 knots

nrknots <- 5
minx <- min(df$budget)-0.001
maxx <- max(df$budget)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-2*step,maxx+2*step,by=step)

xseq <- seq(min(df$budget),max(df$budget),length=100)

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
lines(xseq,fitted,lwd=2,col="steelblue")


# 8 knots


nrknots <- 8
minx <- min(df$budget)-0.001
maxx <- max(df$budget)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-2*step,maxx+2*step,by=step)

xseq <- seq(min(df$budget),max(df$budget),length=100)

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
lines(xseq,fitted,lwd=2,col="steelblue")


# 2.d Cubic P-splines (consider k=5, 8 and 20 knots)----

x <- df$budget
y <- df$profit
# 2.d.1 Based on B-splines----
# Optimal Lambda----
# nrknots = 5----
nrknots <- 5

# Optimal lambda determined in program crossvalidation_simulated.r

optlambda <- 0.01

minx <- min(x)-0.001
maxx <- max(x)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

xseq <- seq(minx+0.001,maxx-0.001,length=500)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,x,ord=4)$design
Bfit <- spline.des(knots=knots,xseq,ord=4)$design


betahat <- solve(t(B)%*%B + optlambda*K1)%*%t(B)%*%y
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(x, y,xlab=" ",ylab=" ",main="5 knots, optimal lambda = 0.01",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
lines(xseq,fitted,lwd=2,col="steelblue")

# nrknots = 8----
nrknots <- 8

# Optimal lambda determined in program crossvalidation_simulated.r

optlambda <- 0.01

minx <- min(x)-0.001
maxx <- max(x)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

xseq <- seq(minx+0.001,maxx-0.001,length=500)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,x,ord=4)$design
Bfit <- spline.des(knots=knots,xseq,ord=4)$design


betahat <- solve(t(B)%*%B + optlambda*K1)%*%t(B)%*%y
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(x, y,xlab=" ",ylab=" ",main="8 knots, optimal lambda = 0.01",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
lines(xseq,fitted,lwd=2,col="steelblue")

# nrknots = 20----
nrknots <- 20

# Optimal lambda determined in program crossvalidation_simulated.r

optlambda <- 20

minx <- min(x)-0.001
maxx <- max(x)+0.001
step <- (maxx-minx)/(nrknots-1)
inner.knots <- seq(minx,maxx,length=nrknots)
knots <- seq(minx-3*step,maxx+3*step,by=step)

xseq <- seq(minx+0.001,maxx-0.001,length=500)

D1 <- matrix(0,nrknots+1,nrknots+2)
for(i in 1:(nrknots+1))
{
  D1[i,i] <- -1
  D1[i,i+1] <- 1
}
D2 <- matrix(0,nrknots,nrknots+2)
for(i in 1:nrknots)
{
  D2[i,i] <- 1
  D2[i,i+1] <- -2
  D2[i,i+2] <- 1
}
K1 <- t(D1)%*%D1
K2 <- t(D2)%*%D2

B <- spline.des(knots=knots,x,ord=4)$design
Bfit <- spline.des(knots=knots,xseq,ord=4)$design


betahat <- solve(t(B)%*%B + optlambda*K1)%*%t(B)%*%y
fitted <- Bfit%*%betahat

Bfitscaled <- Bfit
for(i in 1:(nrknots+2))
  Bfitscaled[,i] <- Bfit[,i]*betahat[i]

plot(x, y,xlab=" ",ylab=" ",main="20 knots, optimal lambda = 0.01",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
lines(xseq,fitted,lwd=2,col="steelblue")

# 2.d.2 Based on TP----
# MIGHT ?

# 3. Include the other covariates in the model and determine what variables show an ----
# impact on the profit.

# 4. A movie is defined successful when the profit is positive. Fit a model that relates ----
# the probability of success and the covariates considered above.

