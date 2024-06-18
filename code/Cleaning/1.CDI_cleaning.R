##------------------------------------------------------------##
### This is the analysis script for the data presented in annarozins first attempt in R ###

##load packages
library(readxl) 
library(plyr)
require(dplyr)
require(ggbeeswarm)
require(FSA)
require(ggplot2)
require(reshape)
require(tidyr)
library(magrittr)
library(knitr)
require(lsr)
require(pwr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(lmerTest)
library(car)
library(MuMIn)
library(sjPlot)
library(sjstats)
library(emmeans) 
library(multcomp)
library(afex)
require(parallel)

##------------------------------------------------------------##

### Set working directory and load data

# Clear previous data
rm(list = ls())

dat <- read.csv(file.path('source', 'CDI', 'Spanish', '1st.part.results-survey877885.csv'),
               header=TRUE,
               stringsAsFactors=FALSE)

##------------------------------------------------------------##
#### for Spanish CDI 

##### Renaming the columns ######
colnames(dat)[colnames(dat) == "startlanguage..Lenguaje.inicial"] <- "language"
colnames(dat)[colnames(dat) == "lastpage..Última.página"] <- "completed"
colnames(dat)[colnames(dat) == "IDENTIFICACION1..ID."] <- "ID_lab"
colnames(dat)[colnames(dat) == "token..Contraseña"] <- "token"
colnames(dat)[colnames(dat) == "id..ID.de.respuesta"] <- "ID"
colnames(dat)[colnames(dat) == "IDENTIFICACION2..Fecha.de.hoy"] <- "date"

##### Removing unnecessary columns #######
dat <- dat[, !colnames(dat) %in% "submitdate..Fecha.de.envío"]

# Identify columns containing "Time" in their names
time_columns <- grep("Time", colnames(dat))
dat <- dat[, -time_columns, drop = FALSE]
dat <- dat[, !colnames(dat) %in% "interviewtime..Tiempo.total"]
dat <- dat[, !colnames(dat) %in% "firstname..Nombre.s"]
dat <- dat[, !colnames(dat) %in% "ID.1"]

###### for basque CDI

##### Renaming the columns ######
colnames(dat)[colnames(dat) == "startlanguage..Start.language"] <- "language"
colnames(dat)[colnames(dat) == "lastpage..Last.page"] <- "completed"
colnames(dat)[colnames(dat) == "ID1..ID."] <- "ID_lab"
colnames(dat)[colnames(dat) == "token..Pasahitza"] <- "token"
colnames(dat)[colnames(dat) == "id..Response.ID"] <- "ID"
colnames(dat)[colnames(dat) == "ID2..Gaurko.data"] <- "date"

##### Removing unnecessary columns #######
dat <- dat[, !colnames(dat) %in% "submitdate..Date.submitted"]
# Identify columns containing "Time" in their names
time_columns <- grep("Time", colnames(dat))
#remove
dat <- dat[, -time_columns, drop = FALSE]
dat <- dat[, !colnames(dat) %in% "interviewtime..Total.time"]
dat <- dat[, !colnames(dat) %in% "firstname..First.name"]


#save as raw before transpose

write.csv(dat, 
          file.path('raw-csv', '1.S_CDI_nontransposed.csv'),
          row.names=FALSE)
