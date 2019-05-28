# Authors: Bruno Esposito and Thomas Duvivier
rm(list = ls())

library(openxlsx)
library(corrplot)
library(stargazer)
library(ggplot2)
library(reshape2)
library(MASS)
library(splines)
library(mgcv)

source("./glm_functions.r")

thomas.path <- paste0("C:/Users/duviv/Documents/University/KUL/S2/",
                      "Abandoned/Generalized-Linear-Models/Group-work")
bruno.path <- paste0("/Users/bruno_esposito/Dropbox/KUL/Courses/",
                      "2nd Semester/Generalized Linear Models/",
                      "Assignment2/GLM")

setwd(bruno.path)
raw_df <- read.xlsx("./data/IMDb.xlsx")
df <- raw_df[,c("profit", "budget", "director_facebook_likes", "content_rating", "duration")]
df$content_rating <- as.factor(df$content_rating)
df$profit.bin <- ifelse(df$profit>0, 1, 0)


# 1. Descriptive ----

# Remove content rating because it is a factor variable
df_descriptive <- df[, !(colnames(df) %in% c('content_rating'))]
# 1.a Correlation Heatmap
cormat <- round(cor(df_descriptive, method = "pearson", 
                    use = "complete.obs"), 2)
cormat
# cormat <- round(cor(df, method = "pearson", use = "complete.obs"), 2)
cormat
melted_cor <- melt(cormat)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

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
plot(df_ord$budget, df_ord$profit,ylab='Profit', xlab='Fitted values')
lines(df_ord$budget, predict(model_loess), col='red')



# TODO: 4 plot of profit vs covariates

#  2.a Polynomial regression model----
fit1 <- lm(profit ~ budget, data = df)
fit2 <- lm(profit ~ budget + I(budget^2), data = df)
fit3 <- lm(profit ~ budget + I(budget^2) + I(budget^3), data = df)


#pdf("./figures/T2_profit-budgete2.pdf", width=7, height=4, pointsize=12)
#plot(df$budget, df$profit, xlab = "Budget", ylab="Profit")
#lines(df$budget, predict(fit1), col="red")
#lines(df$budget, predict(fit2), col="green")
#lines(df$budget, predict(fit3), col="blue")
#legend(230, 350, c("Degree 1", "Degree 2", "Degree 3"), lty = 1, col = c("red", "green", "blue"))
#dev.off()

model_poly = list()
model_poly[[1]] = lm(profit ~ budget, df)
temp_formula = 'profit ~ budget'
for (i in 2:10) {
  temp_formula = paste(temp_formula, ' + I(budget^', i, ')', sep='')
  model_poly[[i]] = lm(formula(temp_formula), df)
}

summary(model_poly[[1]])
out_bic = unlist(lapply(model_poly, BIC))
out_aic = unlist(lapply(model_poly, AIC))

out_bic
out_aic

pdf("./figures/T2_AIC_polynomial.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(out_aic, type = 'l',lwd=2,main="",
     xlab="Polynomial degree",ylab="AIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
dev.off()

# Order 1 is the best model 
par(mfrow=c(1,1))
plot(df$budget, df$profit, ylab='Profit', xlab='Budget')
abline(model_poly[[1]], col='red')


pdf("./figures/T2_profit-budgete-poly.pdf", width=7, height=4, pointsize=12)
plot(df$budget, df$profit, xlab = "Budget", ylab="Profit", main='',
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
lines(sort(df$budget), fitted(model_poly[[1]])[order(df$budget)], col='red') 
lines(sort(df$budget), fitted(model_poly[[3]])[order(df$budget)], col='green') 
lines(sort(df$budget), fitted(model_poly[[10]])[order(df$budget)], col='blue') 
legend(230, 350, c("Degree 1", "Degree 3", "Degree 10"), lty = 1, col = c("red", "green", "blue"))
dev.off()


t.pred <- predict(model_poly[[1]], se = TRUE)
t.upper <- t.pred$fit + qnorm(0.975) * t.pred$se.fit
t.lower <- t.pred$fit - qnorm(0.975) * t.pred$se.fit

pdf("./figures/T2_model_polynomial.pdf", width=7, height=4, pointsize=12)
plot(df$budget, df$profit, xlab = "Budget", ylab="Profit", main='',
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
lines(sort(df$budget), fitted(model_poly[[1]])[order(df$budget)], col='red') 
lines(sort(df$budget), t.upper[order(df$budget)], col = 'blue', lty='longdash')
lines(sort(df$budget), t.lower[order(df$budget)], col = 'blue', lty='longdash')
dev.off()

# Check nonlinearity (NOT NECESSARY)
df_nonlin <- df
df_nonlin$budget.class1 <- as.numeric(I(df$budget <= 50))
df_nonlin$budget.class2 <- as.numeric(I(df$budget > 50 & df$budget <= 100))
df_nonlin$budget.class3 <- as.numeric(I(df$budget > 100 & df$budget <= 150))
df_nonlin$budget.class4 <- as.numeric(I(df$budget > 150 & df$budget <= 200))
df_nonlin$budget.class5 <- as.numeric(I(df$budget <= 250))

model_discrete = lm(profit ~ budget.class1 + budget.class2 + budget.class3 + 
                      budget.class4 + budget.class5, data = df_nonlin)
summary(model_discrete)
plot(sort(df_nonlin$budget), predict(model_discrete)[order(df_nonlin$budget)], 
     type='l', ylab='Predicted profit', xlab='Budget')
#lines(sort(df$budget), predict(model_discrete)[order(df$budget)])


#  2.b Truncated polynomial splines of degree 2 (consider k=2, 3 and 5 knots)----
trun_splineA = get_truncated_spline(df$profit, df$budget, n_knots=2, poly_order=2)
trun_splineB = get_truncated_spline(df$profit, df$budget, n_knots=3, poly_order=2)
trun_splineC = get_truncated_spline(df$profit, df$budget, n_knots=5, poly_order=2)

AIC(trun_splineA[[3]], trun_splineB[[3]], trun_splineC[[3]])

pdf("./figures/T2_trunc_spline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(df$budget, df$profit, xlab="Profit",ylab="Budget",
     cex.lab=1.5,cex.axis=1.3,col="black",cex=1.3)
lines(trun_splineA[[1]], trun_splineA[[2]], lwd=2, col="blue", lty='solid')
lines(trun_splineB[[1]], trun_splineB[[2]], lwd=2, col="red", lty='longdash')
lines(trun_splineC[[1]], trun_splineC[[2]], lwd=2, col="darkgreen", lty='dashed')
legend('topright', c('2 knots', '3 knots', '5 knots'), 
       col=c('blue', 'red', 'darkgreen'), 
       lty=c('solid', 'longdash', 'dashed'))
dev.off()

#  2.c B-splines of degree 2 (consider m=3, 5 and 8 knots)----

my_splinesA = get_bsplines(df$budget, df$profit, nrknots=3)
my_splinesB = get_bsplines(df$budget, df$profit, nrknots=5)
my_splinesC = get_bsplines(df$budget, df$profit, nrknots=8)

my_splinesA[[3]];my_splinesB[[3]];my_splinesC[[3]]
my_splinesA[[4]];my_splinesB[[4]];my_splinesC[[4]]

pdf("./figures/T2_bspline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(df$budget, df$profit, xlab="Profit",ylab="Budget",
     cex.lab=1.5,cex.axis=1.3,col="black",cex=1.3)
lines(my_splinesA[[1]],my_splinesA[[2]],lwd=2,col="blue", lty='solid')
lines(my_splinesB[[1]],my_splinesB[[2]],lwd=2,col="red", lty='longdash')
lines(my_splinesC[[1]],my_splinesC[[2]],lwd=2,col="darkgreen", lty='dashed')
legend('topright', c('3 knots', '5 knots', '8 knots'), 
       col=c('blue', 'red', 'darkgreen'), 
       lty=c('solid', 'longdash', 'dashed'))
dev.off()



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

output_lambda = get_lambda(df$budget, df$profit, 20)

pdf("./figures/T2_GCV_pspline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(output_lambda[['lambda']],output_lambda[['gcv']],type="l",lwd=2,
     xlab="lambda",ylab="GCV",main="10 knots",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
dev.off()

pdf("./figures/T2_AIC_pspline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(output_lambda[['lambda']],output_lambda[['AIC']],type="l",lwd=2,main="",
     xlab="lambda",ylab="AIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
dev.off()

pdf("./figures/T2_BIC_pspline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(output_lambda[['lambda']],output_lambda[['BIC']],type="l",lwd=2,main="",
     xlab="lambda",ylab="BIC",
     cex.lab=1.5,cex.axis=1.3,col="red",cex=1.3)
dev.off()

lambdamingcv <- output_lambda[['lambda']][which(output_lambda[['gcv']]==min(output_lambda[['gcv']]))]
lambdaminaic <- output_lambda[['lambda']][which(output_lambda[['AIC']]==min(output_lambda[['AIC']]))]
lambdaminbic <- output_lambda[['lambda']][which(output_lambda[['BIC']]==min(output_lambda[['BIC']]))]

lambdamingcv;lambdaminaic;lambdaminbic

output_psplinesA = get_cubic_psplines(df$budget, df$profit, nrknots = 5, lambdamingcv)
output_psplinesB = get_cubic_psplines(df$budget, df$profit, nrknots = 8, lambdamingcv)
output_psplinesC = get_cubic_psplines(df$budget, df$profit, nrknots = 20, lambdamingcv)

output_psplinesA[[3]];output_psplinesB[[3]];output_psplinesC[[3]]
output_psplinesA[[4]];output_psplinesB[[4]];output_psplinesC[[4]]

pdf("./figures/T2_pspline.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(df$budget, df$profit, xlab="Budget", ylab="Profit", 
     cex.lab=1.5,cex.axis=1.3,col="black",cex=1.3)
lines(output_psplinesA[[1]], output_psplinesA[[2]], col="blue", lwd=2, lty='solid')
lines(output_psplinesB[[1]], output_psplinesB[[2]], col="red", lwd=2, lty='longdash')
lines(output_psplinesC[[1]], output_psplinesC[[2]], col="darkgreen", lwd=2, lty='dashed')
legend('topright', c('5 knots', '8 knots', '20 knots'), 
       col=c('blue', 'red', 'darkgreen'), 
       lty=c('solid', 'longdash', 'dashed'))
dev.off()



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


# 3. Model profit with covariates ----
# Determine which variables show an impact on the profit.

x <- df$budget
pdf("./figures/T2_loess_budget_profit.pdf", width=7, height=4, pointsize=12)
plot(x, df$profit,
     xlab="Budget",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,cex=1.3, col="black")
orderx <- order(x)
lines(x[orderx],predict(loess(df$profit ~ x))[orderx],
      lwd=2,col="red")
dev.off()

x <- df$director_facebook_likes
pdf("./figures/T2_loess_fblikes_profit.pdf", width=7, height=4, pointsize=12)
plot(x, df$profit,
     xlab="Director facebook likes",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,cex=1.3, col="black")
orderx <- order(x)
lines(x[orderx],predict(loess(df$profit ~ x))[orderx],
      lwd=2,col="red")
dev.off()

x <- df$content_rating
pdf("./figures/T2_loess_content_profit.pdf", width=7, height=4, pointsize=12)
plot(x, df$profit,
     xlab="Content rating",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()

x <- df$duration
pdf("./figures/T2_loess_duration_profit.pdf", width=7, height=4, pointsize=12)
plot(x, df$profit,
     xlab="Duration",ylab="Profit",
     cex.lab=1.5,cex.axis=1.3,cex=1.3, col="black")
orderx <- order(x)
lines(x[orderx],predict(loess(df$profit ~ x))[orderx],
      lwd=2,col="red")
dev.off()

# Null model
profit.gam.null <- gam(profit ~ 1, data=df)

# Naive model
profit.gam <- gam(profit ~ s(budget,bs="ps", k=10) + s(director_facebook_likes, bs="ps", k=10) +
                    s(duration,bs="ps", k=10) + content_rating, data=df)
summary(profit.gam)
gam.check(profit.gam)

pdf("./figures/T2_gam_residual_plot.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(profit.gam$linear.predictors, profit.gam$residuals, 
     lwd=2, cex.lab=1.5,cex.axis=1.3,cex=1.3,
     ylab = 'Deviance residuals', xlab = 'Linear predictors')
lines(sort(profit.gam$linear.predictors), 
      fitted(loess(profit.gam$residuals ~ profit.gam$linear.predictors))[order(profit.gam$linear.predictors)],
      lwd=2, col='red')
dev.off()

pdf("./figures/T2_gam_qqplot.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
qq.gam(profit.gam, lwd=2, cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()

pdf("./figures/T2_gam_resid_budget.pdf", width=7, height=4, pointsize=12)
par(mfrow = c(1,1))
plot(profit.gam,residuals=TRUE,col="red",shade=TRUE, cex = 2, select = 1)
dev.off()

pdf("./figures/T2_gam_resid_fblikes.pdf", width=7, height=4, pointsize=12)
par(mfrow = c(1,1))
plot(profit.gam,residuals=TRUE,col="red",shade=TRUE, cex = 2, select = 2)
dev.off()

pdf("./figures/T2_gam_resid_duration.pdf", width=7, height=4, pointsize=12)
par(mfrow = c(1,1))
plot(profit.gam,residuals=TRUE,col="red",shade=TRUE, cex = 2, select = 3)
dev.off()


#profit.gamA <- gam(profit ~ s(budget,bs="ps"), data=df)
#profit.gamB <- gam(profit ~ s(director_facebook_likes,bs="ps") + content_rating, data=df)
#profit.gamC <- gam(profit ~ s(duration,bs="ps"), data=df)
#profit.gamD <- gam(profit ~ content_rating, data=df)
#AIC(profit.gamA, profit.gamB, profit.gamC, profit.gamD, profit.gam.full)

# Step AIC procedure
df['logfb'] = log(df$director_facebook_likes+1)

profit.gam.full <- gam(profit ~ s(budget,bs="ps", k=10) + 
                         s(director_facebook_likes, bs="ps", k=10) +
                         s(duration,bs="ps", k=10) + 
                         content_rating, data=df)
#profit.gam.full <- gam(profit ~ s(budget,bs="ps", k=10) + 
#                         s(logfb, bs="ps", k=10) +
#                         s(duration,bs="ps", k=10) + 
#                         content_rating, data=df)
summary(profit.gam.full)

profit.gam.fullA <- gam(profit ~ s(director_facebook_likes, bs="ps", k=10)+
                          s(duration,bs="ps", k=10) + 
                          content_rating, data=df)
profit.gam.fullB <- gam(profit ~ s(budget,bs="ps", k=10) +
                          s(duration,bs="ps", k=10) + 
                          content_rating, data=df)
profit.gam.fullC <- gam(profit ~ s(budget,bs="ps", k=10) + 
                          s(director_facebook_likes, bs="ps", k=10) +
                          content_rating, data=df)
profit.gam.fullD <- gam(profit ~ s(budget,bs="ps", k=10) + 
                          s(director_facebook_likes, bs="ps", k=10) +
                          s(duration,bs="ps", k=10), data=df)

AIC(profit.gam.full, profit.gam.fullA, profit.gam.fullB, 
    profit.gam.fullC, profit.gam.fullD) 
# We prefer model D

profit.gam.fullDA <- gam(profit ~ s(director_facebook_likes, bs="ps", k=10) +
                           s(duration,bs="ps", k=10), data=df)
profit.gam.fullDB <- gam(profit ~ s(budget,bs="ps", k=10) +
                           s(duration,bs="ps", k=10), data=df)
profit.gam.fullDC <- gam(profit ~ s(budget,bs="ps", k=10) + 
                           s(director_facebook_likes, bs="ps", k=10), 
                         data=df)

AIC(profit.gam.full, profit.gam.fullD, profit.gam.fullDA, profit.gam.fullDB, profit.gam.fullDC) 
# We prefere model DC

profit.gam.fullDCA <- gam(profit ~ s(duration,bs="ps", k=10), data=df)
profit.gam.fullDCB <- gam(profit ~ s(director_facebook_likes, bs="ps", k=10), data=df)
AIC(profit.gam.full, profit.gam.fullD, profit.gam.fullDC, profit.gam.fullDCA, profit.gam.fullDCB) 
# We prefere model DC

summary(profit.gam.fullDC)
gam.check(profit.gam.fullDC)

pdf("./figures/T2_finalGAM_residual_plot.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(profit.gam.fullDC$linear.predictors, profit.gam.fullDC$residuals, 
     lwd=2, cex.lab=1.5,cex.axis=1.3,cex=1.3,
     ylab = 'Deviance residuals', xlab = 'Linear predictors')
lines(sort(profit.gam.fullDC$linear.predictors), 
      fitted(loess(profit.gam.fullDC$residuals ~ profit.gam.fullDC$linear.predictors))[order(profit.gam.fullDC$linear.predictors)],
      lwd=2, col='red')
dev.off()

pdf("./figures/T2_finalGAM_qqplot.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
qq.gam(profit.gam.fullDC, lwd=2, cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()

pdf("./figures/T2_finalGAM_resid_budget.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(profit.gam.fullDC,residuals=TRUE,col="red",shade=TRUE, select = 1,
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()

pdf("./figures/T2_finalGAM_resid_fblikes.pdf", width=7, height=4, pointsize=12)
par(mfrow=c(1,1))
plot(profit.gam.fullDC,residuals=TRUE,col="red",shade=TRUE, select = 2,
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()

hat.gam.full = influence.gam(profit.gam.full)
summary(hat.gam.full)
plot(hat.gam.full, type='l')
which(hat.gam.full == max(hat.gam.full))
# Outlier seems to be observation 169

hat.gam.fullDC = influence.gam(profit.gam.fullDC)
summary(hat.gam.fullDC)
which(hat.gam.fullDC == max(hat.gam.fullDC))

pdf("./figures/T2_finalGAM_hatmatrix.pdf", width=7, height=4, pointsize=12)
par(mfrow = c(1,1))
plot(hat.gam.fullDC, type='l', lwd=2, main="",
     xlab="Index",ylab="Influence",
     cex.lab=1.5,cex.axis=1.3,cex=1.3)
dev.off()
# Same outlier as in the full model

# Remove the outlier:
df_outlier = df[-169, ]

profit.gam.full.outlier <- gam(profit ~ s(budget,bs="ps", k=10) + 
                                 s(director_facebook_likes, bs="ps", k=10) +
                                 s(duration,bs="ps", k=10) + 
                                 content_rating, data=df_outlier)
summary(profit.gam.full)
#plot(profit.gam.full.outlier,residuals=TRUE,col="red",shade=TRUE, cex = 2)

profit.gam.fullA.outlier <- gam(profit ~ s(director_facebook_likes, bs="ps", k=10) +
                                s(duration,bs="ps", k=10) + 
                                content_rating, data=df_outlier)
profit.gam.fullB.outlier <- gam(profit ~ s(budget,bs="ps", k=10) +
                                s(duration,bs="ps", k=10) + 
                                content_rating, data=df_outlier)
profit.gam.fullC.outlier <- gam(profit ~ s(budget,bs="ps", k=10) + 
                                  s(director_facebook_likes, bs="ps", k=10) + 
                                  content_rating, data=df_outlier)
profit.gam.fullD.outlier <- gam(profit ~ s(budget,bs="ps", k=10) + 
                                  s(director_facebook_likes, bs="ps", k=10) +
                                  s(duration,bs="ps", k=10), data=df_outlier)

AIC(profit.gam.full.outlier, profit.gam.fullA.outlier, 
    profit.gam.fullB.outlier, profit.gam.fullC.outlier, 
    profit.gam.fullD.outlier) 
# We prefer model D

profit.gam.fullDA.outlier <- gam(profit ~ s(director_facebook_likes, bs="ps", k=10) +
                                 s(duration,bs="ps", k=10), data=df_outlier)
profit.gam.fullDB.outlier <- gam(profit ~ s(budget,bs="ps", k=10) +
                                 s(duration,bs="ps", k=10), data=df_outlier)
profit.gam.fullDC.outlier <- gam(profit ~ s(budget,bs="ps", k=10) + 
                                 s(director_facebook_likes, bs="ps", k=10), 
                                 data=df_outlier)

AIC(profit.gam.full.outlier, profit.gam.fullD.outlier, 
    profit.gam.fullDA.outlier, profit.gam.fullDB.outlier, 
    profit.gam.fullDC.outlier)
# We prefer model DC

summary(profit.gam.fullDC.outlier)
gam.check(profit.gam.fullDC.outlier)

pdf("./figures/T2_finalGAM_outlier_resid_budget.pdf", width=7, height=4, pointsize=12)
par(mfrow= c(1,1))
plot(profit.gam.fullDC.outlier,residuals=TRUE,col="red",
     shade=TRUE, cex = 2, select = 1)
dev.off()

pdf("./figures/T2_finalGAM_outlier_resid_fblikes.pdf", width=7, height=4, pointsize=12)
par(mfrow= c(1,1))
plot(profit.gam.fullDC.outlier,residuals=TRUE,col="red",
     shade=TRUE, cex = 2, select = 2)
dev.off()


hat.gam.fullD.outlier = influence.gam(profit.gam.fullD.outlier)
summary(hat.gam.fullD.outlier)
which(hat.gam.fullD.outlier == max(hat.gam.fullD.outlier))


pdf("./figures/T2_finalGAM_outlier_hatmatrix.pdf", width=7, height=4, pointsize=12)
par(mfrow = c(1,1))
plot(hat.gam.fullD.outlier, type='l')
dev.off()



# TODO: outlier and linear talk

profit.gam.full <- gam(profit ~ s(budget,bs="ps", k=10) + 
                         s(director_facebook_likes, bs="ps", k=10) +
                         s(duration,bs="ps", k=10) + content_rating, data=df)
profit.gam.full.linearA <- gam(profit ~ s(budget,bs="ps", k=10) + 
                          s(director_facebook_likes, bs="ps", k=10) +
                          duration + content_rating, data=df)
profit.gam.full.linearB <- gam(profit ~ s(budget,bs="ps", k=10) + 
                          director_facebook_likes +
                          s(duration,bs="ps", k=10) + content_rating, data=df)
profit.gam.full.linearC <- gam(profit ~ budget + 
                          s(director_facebook_likes, bs="ps", k=10) +
                          s(duration,bs="ps", k=10) + content_rating, data=df)
profit.gam.full.linear <- gam(profit ~ budget + director_facebook_likes +
                          duration + content_rating, data=df)

AIC(profit.gam.full, profit.gam.full.linearA, profit.gam.full.linearB, 
    profit.gam.full.linearC, profit.gam.full.linear) 

# No evidence that making terms linear improve the AIC.


profit.gam.fullDC <- gam(profit ~ s(budget,bs="ps", k=10) + 
                         s(director_facebook_likes, bs="ps", k=10), data=df)
profit.gam.full.linearA <- gam(profit ~ budget + 
                                 s(director_facebook_likes, bs="ps", k=10), 
                               data=df)
profit.gam.full.linearB <- gam(profit ~ s(budget,bs="ps", k=10) + 
                                 director_facebook_likes, data=df)
profit.gam.full.linear <- gam(profit ~ budget + 
                                director_facebook_likes, data=df)
profit.lm.fit <- lm(profit ~ budget + director_facebook_likes, data =df)

AIC(profit.gam.fullDC, profit.gam.full.linearA, profit.gam.full.linearB, 
    profit.gam.full.linear, profit.lm.fit) 

# No evidence that making terms linear improve the AIC.



#####################



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


# 4. Model profit as a binomial ----
# A movie is defined successful when the profit is positive. 
# Fit a model that relates the probability of success and 
#   the covariates considered above.

ggplot(df, aes(budget, profit) ) +
  geom_point() +
  stat_smooth()
ggplot(df, aes(duration, profit) ) +
  geom_point() +
  stat_smooth()
ggplot(df, aes(director_facebook_likes, profit) ) +
  geom_point() +
  stat_smooth()
ggplot(df, aes(content_rating, y=profit) ) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")
# /!\ All of them are flat /!\


# The model
# First we start with simple model, then increase by one variable at a time
# Two different tests are in use: LRT or score test (Rao)
logit.fit <- glm(I(profit>0) ~ budget, data = df, family = binomial) 
summary(logit.fit)
add1(logit.fit,~ . + director_facebook_likes + content_rating + duration, test="LRT")

# Then we start with fully specified model 
# Add or delete variables one by one
log.fit.1 <- glm(I(profit>0) ~ budget + 
                 duration +
                 director_facebook_likes + 
                 content_rating, 
                 family = binomial,
                 data = df)
summary(log.fit.2)


# option Chisq is the same here as LRT
anova(log.fit.2,test="Chisq")
anova(carieslogitmodel2,test="LRT")
drop1(carieslogitmodel2,test="Chisq")

# Apply GAM
logitgam1 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) + 
                 duration +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)

logitgam2 <- gam(I(profit > 0) ~ 
                 budget + 
                 s(duration, bs="ps", k=30) +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)

logitgam3 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) + 
                 s(duration, bs="ps", k=30) +
                 director_facebook_likes + 
                 content_rating, 
                 data=df, family = binomial)

logitgam4 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30),
                 data=df, family = binomial)

logitgam5 <- gam(I(profit > 0) ~ 
                 budget,
                 data=df, family = binomial)

logitgam6 <- gam(I(profit > 0) ~ 
                 s(duration, bs="ps", k=30),
                 data=df, family = binomial)

logitgam7 <- gam(I(profit > 0) ~ 
                 duration,
                 data=df, family = binomial)

logitgam8 <- gam(I(profit > 0) ~ 
                 s(budget, bs="ps", k=30) +
                 duration,
                 data=df, family = binomial)

AIC(log.fit.1, logitgam1, logitgam2, logitgam3, logitgam4, logitgam5, logitgam6, 
        logitgam7, logitgam8)

# Plot
plot(df$budget,df$profit.bin,pch="|",
     cex.lab=1.5,cex.axis=1.3)
x <- seq(min(birthweight),max(birthweight))
lines(x,myexpit(x,b0=lrmod1$coeff[1],b1=lrmod1$coeff[2]),
      lty=2,col="red",lwd=3)
lines(x,mycllit(x,b0=clmod1$coeff[1],b1=clmod1$coeff[2]),
      lty=3,col="steelblue",lwd=3)
legend("topright",legend=c("Logistic","Comp Log Log"),lty=2:3,
       bty="n",col=c("red","steelblue"),cex=1.5)

# Also check for model fit via residual deviance
attributes(summary(fit.logit))
dev <- summary(fit.logit)$deviance
df <- summary(fit.logit)$df.residual
1-pchisq(dev,df)

