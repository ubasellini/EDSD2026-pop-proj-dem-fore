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
I <- 25000
Ix <- I*mx
sum(Ix)
plot(x, Ix, type="o",pch=16,
     xlab = "Age group",ylab= "Net migrant counts",
     main="RC migration schedule for 25,000 net migrants") 


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

## create a function for projecting female population
## in the long term
pop.proj.F.Mig.fun <- function(age,agegroup,sFx,bFx,NFx,
                               Ix,n=1){
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
    N[,i+1] <- L%*%(N[,i]+Ix/2) + Ix/2
  }
  ## create output of our function
  out <- cbind(data.frame(age=age,agegroup=agegroup),N)
  return(out)
}

## make projection
age <- df.projection$Age
agegroup <- df.projection$AgeGroup

my.proj.Mig.1 <- pop.proj.F.Mig.fun(age=age,agegroup=agegroup,
                                sFx=sFx,bFx=bFx,NFx=NFx,
                                Ix=Ix,n=20)

## long data
dta.swe.l <- my.proj1 %>%
  pivot_longer(-c(age,agegroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         Assumption="No migration")
dta.swe.l.no.mig <- my.proj.Mig.1 %>%
  pivot_longer(-c(age,agegroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         Assumption="With migration")
my.df <- dta.swe.l %>% 
  bind_rows(dta.swe.l.no.mig)

ggplot(dta.swe.l.no.mig,aes(x=agegroup,y=population,fill=YearF)) +
  geom_bar(data = subset(dta.swe.l.no.mig, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle("Swedish female population") +
  scale_fill_manual(name = "Year", values=c("#E69F00", "#56B4E9","#1C7C54"))

my.df %>% 
  ggplot(aes(x=agegroup,y=population,fill=YearF)) +
  geom_bar(data = subset(my.df, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  facet_wrap(.~Assumption)+
  ggtitle("Swedish female population") +
  scale_fill_manual(name = "Year", values=c("#E69F00", "#56B4E9","#1C7C54"))

## plotting
ggplot(my.df,aes(x=agegroup,y=population,fill=Assumption)) +
  geom_bar(data = subset(my.df, period == 2),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle(paste("Swedish female population, year",subset(my.df, period == 2)$Year)) +
  scale_fill_manual(name = 'Projection', values=c("#E69F00", "#56B4E9"))


## ------ time-varying assumption

## take fertility rates
fx <- df.projection$Fx
plot(x,fx,t="l")
tfr1 <- 5*sum(fx)

## define projection period
n <- 20
tfr.seq <- seq(tfr1,1.5,length.out=n)
plot(1:n,tfr.seq)

## option 1
sum.fx <- sum(fx)
fx.stand <- fx/sum.fx
sum(fx.stand)
fx2 <- fx.stand*tfr.seq[20]/5
sum(fx2)
tfr2 <- 5*sum(fx2)
plot(x,fx,t="l")
lines(x,fx2,col=2)

## option 2
fx20 <- fx/tfr1 * tfr.seq[20]
5*sum(fx20)

FX <- matrix(fx,m,n)
for (i in 1:n){
  FX[,i] <- FX[,i]/tfr1 * tfr.seq[i]
}
5*apply(FX,2,sum)

## plotting fertility rates
matplot(x,FX,lty=1,col=rainbow(n),t="l")

## useful objects
srb <- 1.05
fact.srb <- 1/(1+srb)
l0 <- 1e5
LF0 <- df.projection$LFx[1]

bFx.test <- fact.srb * LF0/(2*l0)*(fx[-m]+sFx*fx[-1])
bFx.data <- df.projection$bFx[-m]
all.equal(bFx.data,bFx.test)


## create a matrix of time-varying BFX
BFX <- matrix(0,m,n)
i <- 1
for (i in 1:n){
  BFX[-m,i] <- fact.srb * LF0/(2*l0)*(FX[-m,i]+sFx*FX[-1,i])
}
matplot(x,BFX,lty=1,col=rainbow(n),t="l")


## create a function for projecting female population
## in the long term with time-varying TFR
pop.proj.F.TFR.fun <- function(age,agegroup,sFx,BFX,NFx,
                               n=1){
  ## dimension of data
  m <- length(age)
  ## create an empty Leslie matrix
  L <- matrix(0,nrow = m,ncol = m)
  ## fill up L
  diag(L[-1,]) <- sFx
  L[m,m] <- sFx[m-1]
  ## create a matrix of population vectors
  N <- matrix(0,nrow = m,ncol = n + 1)
  ## fill first column with the starting population
  N[,1] <- NFx
  ## for loop for the projection
  i <- 1
  for (i in 1:n){
    L[1,] <- BFX[,i]
    N[,i+1] <- L%*%N[,i]
  }
  ## create output of our function
  out <- cbind(data.frame(age=age,agegroup=agegroup),N)
  return(out)
}


my.proj.TFR.1 <- pop.proj.F.TFR.fun(age=age,agegroup=agegroup,
                                    sFx=sFx,BFX=BFX,NFx=NFx,
                                    n=20)

## long data
dta.swe.l <- my.proj1 %>%
  pivot_longer(-c(age,agegroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         Assumption="Constant fertility")
dta.swe.l.TFR <- my.proj.TFR.1 %>%
  pivot_longer(-c(age,agegroup),names_to = "period",values_to = "population") %>%
  mutate(period=as.numeric(period),
         Year=1993 + (period-1)*5,
         YearF=as.factor(Year),
         Assumption="Time-varying fertility")
my.df <- dta.swe.l %>% 
  bind_rows(dta.swe.l.TFR)

my.df %>% 
  ggplot(aes(x=agegroup,y=population,fill=YearF)) +
  geom_bar(data = subset(my.df, period %in% c(1,2,21)),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  facet_wrap(.~Assumption)+
  ggtitle("Swedish female population") +
  scale_fill_manual(name = "Year", values=c("#E69F00", "#56B4E9","#1C7C54"))

## plotting
ggplot(my.df,aes(x=agegroup,y=population,fill=Assumption)) +
  geom_bar(data = subset(my.df, period == 21),
           stat = "identity",position = "dodge",color = "black") +
  coord_flip() +
  theme_bw() +
  ggtitle(paste("Swedish female population, year",subset(my.df, period == 21)$Year)) +
  scale_fill_manual(name = 'Projection', values=c("#E69F00", "#56B4E9"))


## SHINY APP for dynamic visualization of your results
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "year", label = "Year", step = 5,
              value = min(dta.swe.l$Year), min = min(dta.swe.l$Year), max = max(dta.swe.l$Year)),
  column(12, plotOutput("plot_pyr1"))
)

server <- function(input, output){
  output$plot_pyr1 <- renderPlot({
    ## plotting pyramid
    ggplot(my.df,aes(x=agegroup,y=population,fill=Assumption)) +
      geom_bar(data = subset(my.df, Year == input$year),
               stat = "identity",position = "dodge",color = "black") +
      coord_flip() +
      theme_bw() +
      ggtitle(paste("Swedish female population, year",subset(my.df, Year == input$year)$Year)) +
      scale_fill_manual(name = "Projection", values=c("#E69F00", "#56B4E9")) +
      scale_y_continuous(limits = c(0, 350000), breaks = seq(0, 350000, 100000))
  })
}

shinyApp(ui = ui, server =  server)

## END


