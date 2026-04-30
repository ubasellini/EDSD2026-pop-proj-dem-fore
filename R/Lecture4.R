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
library(forecast)
library(viridis)
library(fields)

## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/MORTSWE.Rdata")

