## -------------------------------------------------- ##
## EDSD 2025-2026 
## Population Projections & Demographic Forecasting
##
## Lecture 3
## Matrix projections
##
## Date: 29/04/2026
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## ---- EXERCISE 1 ---

## cleaning the workspace
rm(list=ls(all=TRUE))

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading packages
library(tidyverse)
library(migest)
## check the fundamental RC parameters
rc_model_fund
my.pars <- rc_model_fund %>% 
  select(value) %>% pull()
## function to construct RC schedule
mxRG <- function(x,pars){
  t1 <- pars[1]*exp(-pars[2]*x)
  t2 <- pars[3]*exp(-pars[4]*(x-pars[5])-exp(-pars[6]*(x-pars[5])))
  mx <- t1+t2+pars[7]
  mx <- mx/sum(mx)
  return(mx)
}  
## five-year age groups (works well also with one-year)
x <- seq(0,85,5)
mx <- mxRG(x=x,pars=my.pars)
sum(mx)
plot(x, mx, type="o",pch=16)
## assume a total of 100000 net migration counts
I <- 1e5
Ix <- I*mx
sum(Ix)
plot(x, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 100,000 net migrants") 


## loading data
load("data/Lecture3.Rdata")

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



## END