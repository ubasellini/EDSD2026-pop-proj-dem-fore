## -------------------------------------------------- ##
## EDSD 2025-2026 
## Population Projections & Demographic Forecasting
##
## Lecture 4
## Lee-Carter model
##
## Date: 30/04/2026
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## ---- EXERCISE 1 ---

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(viridis)
library(fields)

## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/MORTSWE.Rdata")

## check data
range(MORT.SWE$Year)
range(MORT.SWE$Age)

## subsetting the data
my.df <- MORT.SWE %>% 
  filter(Sex=="Male",Year>=1950,Age<=100)

## extract objects of interest
x <- unique(my.df$Age)
t <- unique(my.df$Year)
m <- length(x)
n <- length(t)

## extract mortality data
Y <- matrix(my.df$Deaths,m,n)
E <- matrix(my.df$Exposures,m,n)
LMX <- matrix(my.df$logRates,m,n)
any(is.infinite(LMX))

## adjust death counts
Y1 <- Y
Y1[Y==0] <- 1
LMX <- log(Y1/E)

## plotting
matplot(x,LMX,t="l",lty=1,col=viridis(n))
image.plot(t,x,t(LMX))

## compute alpha
Alpha <- apply(LMX,1,sum)/n

matplot(x,LMX,t="l",lty=1,col="grey70")
lines(x,Alpha,col="darkgreen",lwd=2)

## Beta and Kappa

## centred matrix of log-rates
LMX.cent <- LMX-Alpha
image.plot(t,x,t(LMX),col=viridis(n))
image.plot(t,x,t(LMX.cent),col=viridis(n))

## svd
my.svd <- svd(LMX.cent)
Beta <- my.svd$u[,1]
Kappa <- my.svd$v[,1]
par(mfrow=c(1,2))
plot(x,Beta,t="l",lwd=2)
plot(t,Kappa,t="l",lwd=2)

LMX.cent.svd <- my.svd$d[1]*(Beta%*%t(Kappa))
LMX.cent.svd3 <- my.svd$d[1]*(Beta%*%t(Kappa)) +
  my.svd$d[2]*(my.svd$u[,2]%*%t(my.svd$v[,2])) +
  my.svd$d[3]*(my.svd$u[,3]%*%t(my.svd$v[,3])) 

par(mfrow=c(1,3))
image.plot(t,x,t(LMX.cent),col=viridis(n))
image.plot(t,x,t(LMX.cent.svd),col=viridis(n))
image.plot(t,x,t(LMX.cent.svd3),col=viridis(n))


## including the constraints
## constraint 1
sum.Beta <- sum(Beta)
Beta <- Beta/sum.Beta
sum(Beta)
## constraint 2
Kappa1 <- my.svd$d[1]*Kappa*sum.Beta
sum(Kappa1)
## plotting
plot(x,Beta,t="l",lwd=2)
plot(t,Kappa1,t="l",lwd=2)
par(mfrow=c(1,1))



##---- step 3: adjust KAPPA -----
## function to compute difference between observed and fitted LC deaths
koptim <- function(par,alpha,beta,sum.dx,Exp){
  kappa <- par[1]
  lmx.lc <- alpha+beta*kappa
  z.lc <- exp(lmx.lc)*Exp
  sum.z.lc <- sum(z.lc)
  diff.lc <- abs(sum.dx-sum.z.lc)
  return(diff.lc)
}
## adjust Kappa every year
Kappa <- numeric(n)
for (i in 1:n){
  KappaSecStep <- optimize(f=koptim,interval=c(-100,100),alpha=Alpha,
                           beta=Beta,sum.dx=sum(Y[,i]),Exp=E[,i])
  Kappa[i] <- KappaSecStep$minimum
}
## plotting
plot(t,Kappa1,ylim=range(Kappa1,Kappa),t="l",lwd=2)
lines(t,Kappa,col=4,lwd=2)
legend("topright",c("from SVD","second-step adjustment"),col=c(1,4),lwd=2)


## fitted log-mortality
Ones <- matrix(1,n)
ETAlc <- Alpha%*%t(Ones) + Beta%*%t(Kappa)
## basic plot
g <- my.df %>%
  mutate(logRates=case_when(
    is.infinite(logRates)~NA,
    TRUE~logRates),
    Fitted=c(ETAlc)) %>%
  ggplot(aes(x=Age,group=Year))+
  geom_point(aes(y=logRates,group=Year))+
  geom_line(aes(y=Fitted,group=Year),color="darkorange",linewidth=1.2)+
  theme_bw(base_size = 18) +
  labs(y= "Log Mortality Rate")
## animating with gganimate
library(gganimate)
gg <- g + transition_time(Year) +
  labs(title="Year {frame_time}")
animate(gg, fps=4)




