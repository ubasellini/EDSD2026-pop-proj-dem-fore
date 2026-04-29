## -------------------------------------------------- ##
## EDSD 2025-2026 
## Population Projections & Demographic Forecasting
##
## Lecture 2
## Matrix projections
##
## Date: 28/04/2026
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## ---- EXERCISE 1 ---

## cleaning the workspace
rm(list=ls(all=TRUE))

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading useful packages
library(tidyverse)

## loading data
load("data/Lecture2.Rdata")

## extract the objects of interest
x <- df.projection$Age
sFx <- df.projection$sFx
bFx <- df.projection$bFx
NFx <- df.projection$NFx
m <- length(x)

## change NAs to 0 for bFx
bFx[is.na(bFx)] <- 0

## drop last element of sFx
sFx <- sFx[!is.na(sFx)]

## create an empty Leslie matrix
L <- matrix(0,nrow = m,ncol = m)
colnames(L) <- x

## fill up L
L[1,] <- bFx
diag(L[-1,]) <- sFx
L[m,m] <- sFx[m-1]

## project population
NFx5.matrix <- as.vector(L%*%NFx)
NFx5.manual <- df.projection$NFx5
all.equal(NFx5.matrix,NFx5.manual)

plot(x,NFx5.matrix,lwd=2)
points(x,NFx5.manual,col=4,pch=4,lwd=2)


## create a function for projecting female population
## in the long term
pop.proj.F.fun <- function(age,agegroup,sFx,bFx,NFx,
                           n=1){
  ## dimension of data
  m <- length(age)
  ## create an empty Leslie matrix
  L <- matrix(0,nrow = m,ncol = m)
  ## fill up L
  L[1,] <- bFx
  diag(L[-1,]) <- sFx
  L[m,m] <- sFx[m-1]
  ## create a matrix of population vectors
  N <- matrix(0,nrow = m,ncol = n + 1)
  ## fill first column with the starting population
  N[,1] <- NFx
  ## for loop for the projection
  i <- 1
  for (i in 1:n){
    N[,i+1] <- L%*%N[,i]
  }
  ## create output of our function
  out <- cbind(data.frame(age=age,agegroup=agegroup),N)
  return(out)
}

## make projection
age <- df.projection$Age
agegroup <- df.projection$AgeGroup

my.proj1 <- pop.proj.F.fun(age=age,agegroup=agegroup,
                           sFx=sFx,bFx=bFx,NFx=NFx,
                           n=20)

## long data
dta.swe.l <- my.proj1 %>%
  pivot_longer(-c(age,agegroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## plotting
ggplot(dta.swe.l,aes(x=agegroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(name = "Year", values=c("#E69F00", "#56B4E9","#1C7C54"))


### ---- projecting the whole population ----

## create a function for projecting female population
## in the long term
pop.proj.fun <- function(age,agegroup,sFx,bFx,NFx,
                         sMx,bMx,NMx,
                         n=10){
  ## dimension of data
  m <- length(age)
  m2 <- 2*m
  ## create an empty Leslie matrix
  LF <- LM <- BM <- ZEROS <- matrix(0,nrow = m,ncol = m)
  ## fill up LF
  LF[1,] <- bFx
  diag(LF[-1,]) <- sFx
  LF[m,m] <- sFx[m-1]
  ## fill up LM and BM
  BM[1,] <- bMx
  diag(LM[-1,]) <- sMx
  LM[m,m] <- sMx[m-1]
  ## combine pieces to create Leslie matrix
  LUP <- cbind(LF,ZEROS)
  LDOWN <- cbind(BM,LM)
  L <- rbind(LUP,LDOWN)
  ## create a matrix of population vectors
  N <- matrix(0,nrow = m2,ncol = n + 1)
  ## fill first column with the starting population
  N[,1] <- c(NFx,NMx)
  ## for loop for the projection
  i <- 1
  for (i in 1:n){
    N[,i+1] <- L%*%N[,i]
  }
  ## create output of our function
  out <- cbind(data.frame(age=rep(age,2),
                          agegroup=rep(agegroup,2),
                          sex=rep(c("female","male"),each=m)),N)
  return(out)
}

## male objects
NMx <- df.projection$NMx
bMx <- df.projection$bMx
sMx <- df.projection$sMx

## change NAs to 0 for bMx
bMx[is.na(bMx)] <- 0

## drop last element of sMx
sMx <- sMx[!is.na(sMx)]

## make the projection for both sexes
my.proj2 <- pop.proj.fun(age=age,agegroup=agegroup,
                         sFx=sFx,bFx=bFx,NFx=NFx,
                         sMx=sMx,bMx=bMx,NMx=NMx,
                         n=20)

## long data
dta.swe.l <- my.proj2 %>%
  pivot_longer(-c(age,agegroup,sex),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year))

## plotting
ggplot(dta.swe.l,aes(x=agegroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  facet_wrap(.~sex)+
  ggtitle("Swedish female population") +
  scale_fill_manual(name = "Year", values=c("#E69F00", "#56B4E9","#1C7C54"))

## saving data for lecture 3
save(df.projection,my.proj1,my.proj2,file = "data/Lecture3.Rdata")


## END