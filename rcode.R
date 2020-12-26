library(readxl)
wbagesexdata <- read_excel("DDW-1900C-13.xls", 
                           range = "E9:H109", col_names = FALSE)

colnames(wbagesexdata) <- c("age","total","male","female")
wbagesexdata = as.data.frame(wbagesexdata)
wbagesexdata$age = as.character(wbagesexdata$age)
wbagesexdata$age = factor(wbagesexdata$age, levels = unique(wbagesexdata$age))

#set.seed(123)
library(ggplot2)
library(reshape2)
mdf <- melt(wbagesexdata, id.vars="age")


### This is for plotting the multiple line diagram

linefig <- ggplot(mdf, aes( x=age, y=value, colour=variable, group=variable )) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("total"="black","male"="red","female"="orange")) +
  scale_linetype_manual(values=c("total"="dashed", "male"="solid","female"="solid")) +
  scale_x_discrete(breaks = wbagesexdata$age[c(T,F,F,F,F)]) +
  ggtitle("Age-Sex Population Distribution, \n West Bengal (2011)") +
  ylab("Population") + xlab("Age (lbd)") +
  theme(plot.title = element_text(hjust = 0.5))

linefig


# This plots the population pyramid

malefemale <- wbagesexdata[,c(3,4,1)]
library(pyramid)

pyramid(malefemale,Llab="Males", Rlab="Females", Clab="Age (lbd)",main = "Single-year Age Population Pyramid \n West Bengal, 2011", Cstep = 5)

# This calculates the Whipple's indices

attach(wbagesexdata)

age5 <- seq(25,60,5) +1
totpop5 <- sum(total[age=age5])

age0 <- seq(30,60,10) + 1
totpop0 <- sum(total[age=age0])

age2362 <- c(23:62) + 1
totpop2362  <- sum(total[age=age2362])

WI0 <- totpop0 / (totpop2362/10) * 100

WI5 <- totpop5 / (totpop2362/5) * 100
detach(wbagesexdata)


# This calculates the Myer's Index

data <- wbagesexdata[-101,c(1,2)]


# This needs some more investigation. I am out of patience and just want to submit this assignment. Calculating Myer's index in Excel for now. I need to refer to timriffe/AgeHeaping on GitHub later.

rm(list=ls())   #Purge all data from memory.



