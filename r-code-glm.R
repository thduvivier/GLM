library(readxl) 
library(ggplot2)
library(reshape2)µ
library(MASS)
library(splines)
library(mgcv)


setwd("C:/Users/duviv/Documents/University/KUL/S2/Abandoned/Generalized-Linear-Models/Group-work")
df <- read_xlsx("IMDb.xlsx")
df <- df[,c("profit", "budget", "director_facebook_likes", "content_rating", "duration")]
df$content_rating <- as.factor(df$content_rating)
df$profit.bin <- ifelse(df$profit>0, 1, 0)


# 1. Descriptive ----

sapply(df, mean, na.rm=TRUE)
sapply(df, sd, na.rm=TRUE)

# Tables
ag <- aggregate(profit ~ content_rating, df, function(x) c(mean = mean(x), sd = sd(x)))




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
fds

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
lm.fit <- lm(profit ~ budget + director_facebook_likes, data =df)
summary(lm.fit)
AIC(lm.fit)

plot(df$budget, df$profit)
plot(df$director_facebook_likes, df$profit)
ggplot(df, aes(content_rating, profit)) + 
  geom_point()


abline(lm(profit~director_facebook_likes, data=df))
df.no.out <- df[-13,]
abline(lm(profit~director_facebook_likes, data=df.no.out), col="red")
plot(df$content_rating, df$profit)

df$director_facebook_likes2 <- df$director_facebook_likes^2
plot(df$director_facebook_likes2, df$profit)

df.gam <- gam(profit~s(df$budget, bs="ps", k=20)+s(df$director_facebook_likes,bs="ps", k=20)+
                    df$content_rating, data=df)

plot(df.gam, residuals=FALSE, cex=1.3, col="red", shade=TRUE)
gam.check(munich.gam,col="blue")
plot(munich.gam,residuals=TRUE,cex=1.3,col="red",shade=TRUE)
gam.check(munich.gam,col="blue")

# 4. A movie is defined successful when the profit is positive. Fit a model that relates ----
# the probability of success and the covariates considered above.

ggplot(df, aes(budget, profit) ) +
  geom_point() +
  stat_smooth()
ggplot(df, aes(duration, profit) ) +
  geom_point() +
  stat_smooth(method="loess")
ggplot(df, aes(director_facebook_likes, profit) ) +
  geom_point() +
  stat_smooth(method="loess")
ggplot(df, aes(content_rating, y=profit) ) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")
# /!\ All of them are flat /!\


# The model

# Linear no interaction

# First we start with simple model, then increase by one variable at a time
# Two different tests are in use: LRT or score test (Rao)

# start with budget
logit.fit1 <- glm(I(profit>0) ~ budget, data = df, family = binomial) 
summary(logit.fit1)
add1(logit.fit,~ . + director_facebook_likes + content_rating + duration, test="LRT")

# start with duration
logit.fit2 <- glm(I(profit>0) ~ duration, data = df, family = binomial) 
summary(logit.fit2)
add1(logit.fit,~ . + director_facebook_likes + content_rating + budget, test="LRT")

# start with director_facebook_likes
logit.fit3 <- glm(I(profit>0) ~ director_facebook_likes, data = df, family = binomial) 
summary(logit.fit3)
add1(logit.fit,~ . + budget + content_rating + duration, test="LRT")

# start with content_rating
logit.fit4 <- glm(I(profit>0) ~ content_rating, data = df, family = binomial) 
summary(logit.fit4)
add1(logit.fit,~ . + director_facebook_likes + budget + duration, test="LRT")

AIC(logit.fit1, logit.fit2, logit.fit3, logit.fit4)

###
# Quadratic No interactions
###

log.fit.quad.1 <- glm(I(profit>0) ~ 
                 budget + I(budget^2) + I(budget^3) + 
                 duration + I(duration^2) + I(duration^3) + 
                 director_facebook_likes + I(director_facebook_likes^2) + I(director_facebook_likes^3)+
                 content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.quad.1)

log.fit.quad.2 <- glm(I(profit>0) ~ 
                        budget + I(budget^2), 
                      family = binomial,
                      data = df)
summary(log.fit.quad.2)
log.fit.quad.3 <- glm(I(profit>0) ~ 
                        duration + I(duration^2)
                        , 
                      family = binomial,
                      data = df)
summary(log.fit.quad.3)
log.fit.quad.4 <- glm(I(profit>0) ~
                        director_facebook_likes + I(director_facebook_likes^2) + I(director_facebook_likes^3)
                        , 
                      family = binomial,
                      data = df)
summary(log.fit.quad.4)

###
# Interactions
###

# Then we start with fully specified model 
# Add or delete variables one by one
log.fit.1 <- glm(I(profit>0) ~ director_facebook_likes + duration + budget*director_facebook_likes , 
                 family = binomial,
                 data = df)
summary(log.fit.1)

log.fit.2 <- glm(I(profit>0) ~ budget + duration + budget*duration, 
                 family = binomial,
                 data = df)
summary(log.fit.2)

log.fit.4 <- glm(I(profit>0) ~ budget + content_rating + budget*content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.4)

log.fit.5 <- glm(I(profit>0) ~ duration + content_rating + duration*content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.5)

log.fit.6 <- glm(I(profit>0) ~ budget + content_rating + budget*content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.6)

log.fit.7 <- glm(I(profit>0) ~ director_facebook_likes + content_rating + director_facebook_likes*content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.7)


# Apply GAM
logitgam1 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) + 
                 duration +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)
summary(logitgam1)

logitgam2 <- gam(I(profit > 0) ~ 
                 budget + 
                 s(duration, bs="ps", k=30) +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)
summary(logitgam2)

logitgam3 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) + 
                 s(duration, bs="ps", k=30) +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)
summary(logitgam3)

logitgam4 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30),
                 data=df, family = binomial)
summary(logitgam4)

logitgam5 <- gam(I(profit > 0) ~ 
                 budget,
                 data=df, family = binomial)
summary(logitgam5)

logitgam6 <- gam(I(profit > 0) ~ 
                 s(duration, bs="ps", k=30),
                 data=df, family = binomial)
summary(logitgam6)

logitgam7 <- gam(I(profit > 0) ~ 
                 duration,
                 data=df, family = binomial)
summary(logitgam7)

logitgam8 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) +
                 duration,
                 data=df, family = binomial)
summary(logitgam8)

# Compare models
AIC(log.fit.1, logitgam1, logitgam2, logitgam3, logitgam4, logitgam5, logitgam6, 
        logitgam7, logitgam8)

summary(logitgam4)

