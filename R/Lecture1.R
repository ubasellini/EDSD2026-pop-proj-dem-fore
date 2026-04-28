## -------------------------------------------------- ##
## EDSD 2025-2026 
## Population Projections & Demographic Forecasting
##
## Lecture 1
## Cohort Component Method
##
## Date: 27/04/2026
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
load("data/dta.swe.1993.Rdata")

## experiment with lead and lag
df.experiment <- dta.swe %>% 
  select(Age,AgeGroup,NFx,LFx) %>% 
  mutate(LFx.lead=lead(LFx),
         sFx=LFx.lead/LFx,
         NFx.lag = lag(NFx),
         NFx5=lag(NFx*sFx))

## actual projection
df.projection <- dta.swe %>% 
  mutate(sFx=lead(LFx)/LFx,
         NFx5=lag(NFx*sFx))

## with base R
age <- dta.swe$Age
m <- length(age)
LFx <- dta.swe$LFx
sFx <- LFx[-1]/LFx[-m]

## adjusting last group
tail(df.projection[,c(1:3,8,9)])

df.projection <- df.projection %>% 
  mutate(sFx=ifelse(Age==80,yes=lead(LFx)/(LFx+lead(LFx)),
                    no=sFx),
         NFx5=ifelse(Age==85,yes=lag(sFx)*(lag(NFx)+NFx),
                     no=NFx5))

## adjust the first age group
srb <- 1.05
fact.srb <- 1/(1+srb)
l0 <- 1e5
LF0 <- dta.swe$LFx[1]

df.projection <- df.projection %>% 
  mutate(bFx=fact.srb*LF0/(2*l0)*(Fx+sFx*lead(Fx)),
         NFx5=ifelse(Age==0,
                     yes=sum(bFx*NFx,na.rm = T),
                     no=NFx5))

## second approach
df.test <- df.projection %>%
  mutate(bFx=fact.srb * LF0 / (2*l0) * (Fx + sFx*lead(Fx)),
         Bx=Fx*5*(NFx+NFx5)/2,
         NFx5=ifelse(test = Age==0,
                     yes  = fact.srb * LF0 / (5*l0) * sum(Bx,na.rm = T),
                     no   = NFx5)
  )
df.test$NFx5[1]
df.projection$NFx5[1]


## useful packages
library(scales)

## long data
dta.swe.l <- df.projection %>%
  select(AgeGroup,NFx,NFx5) %>%
  rename('1993'=NFx,'1998'=NFx5) %>%
  pivot_longer(-AgeGroup,names_to = "year",values_to = "population")

## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=year)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))

## --- male projection -----

## useful objects
fact.srb.M <- srb/(1+srb)
LM0 <- dta.swe$LMx[dta.swe$Age==0]
## male projection
df.projection <- df.projection %>%
  mutate(sMx=lead(LMx)/LMx,
         NMx5=lag(NMx*sMx),
         sMx=ifelse(test = Age==80,
                    yes  = lead(LMx)/(LMx + lead(LMx)),
                    no   = sMx),
         NMx5=ifelse(test = Age==85,
                     yes  = (NMx+lag(NMx))*lag(sMx),
                     no   = NMx5),
         bMx=fact.srb.M * LM0 / (2*l0) * (Fx + sFx*lead(Fx)),
         NMx5=ifelse(test = Age == 0,
                     yes  = sum(bMx*NFx,na.rm = T),
                     no   = NMx5))

## long data
dta.swe.l <- df.projection %>%
  select(AgeGroup,NMx,NMx5) %>%
  rename('1993'=NMx,'1998'=NMx5) %>%
  pivot_longer(-AgeGroup,names_to = "year",values_to = "population")

## plotting
ggplot(dta.swe.l,aes(x=AgeGroup,y=population,fill=year)) +
  geom_bar(stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish male population") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))

## saving data for lecture 2
save(df.projection,file = "data/Lecture2.Rdata")

## END