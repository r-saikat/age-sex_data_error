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

linefig <- ggplot(mdf, aes( x=age, y=value, colour=variable, group=variable )) + 
  geom_line() +
  geom_point() +
  scale_color_manual(values=c("total"="black","male"="red","female"="orange")) +
  scale_linetype_manual(values=c("total"="dashed", "male"="solid","female"="solid")) +
  scale_x_discrete(breaks = wbagesexdata$age[c(T,F,F,F,F)]) +
  ggtitle("Age-Sex Population Distribution, \n West Bengal (2011)") +
  xlab("Population") + ylab("Age (lbd)") +
  theme(plot.title = element_text(hjust = 0.5))

linefig




malefemale <- wbagesexdata[,c(3,4,1)]
library(pyramid)

pyramid(malefemale,Llab="Males", Rlab="Females", Clab="Age (lbd)",main = "Single-year Age Population Pyramid \n West Bengal, 2011", Cstep = 5)

