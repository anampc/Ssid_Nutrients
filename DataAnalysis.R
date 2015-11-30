# -------------------------------------------------------------------------------------------------
# ANALYSIS

# Modify Ssid.csv with S:H ratios and save it with the day of last data added
# Import last qPCR data 

# -------------------------------------------------------------------------------------------------

getwd()

library(ggplot2)
library(plotly)

data<-read.csv("data/Ssid-11-26-15.csv")

      data$Community<-factor(as.character(data$Community), levels=c("C","DC","D"))
      data$Sample.Time <- as.factor(data$Sample.Time)
      data$Community<- as.factor(data$Community)
      data$Colony <- as.factor(data$Colony)
      data$Treatment <- as.factor(data$Treatment)
      data$Time <- as.factor(data$Time)
      data$Repli <- as.factor(data$Repli)
summary(data)

# Frecuency of dominant Symbioints
      data$Colony<-factor(as.character(data$Colony), levels=c("S1","S4","S3","S5","S6","S2"))

      Com <- ggplot(data = data, aes(x=Time)) 
      Com <- Com + geom_histogram(aes(fill=Community))
      Com <- Com + scale_fill_brewer(palette="Set3")
      
      ComTime <- Com + facet_wrap( ~ Colony, ncol=1)
      ComTime

      ComTreat <- Com + facet_wrap( ~ Treatment, ncol=1)
      ComTreat

    ColCD_1_4_3<-(data[(data$Colony=="S1" & data$Colony=="S4" & data$Colony=="S3"), ])
    ColD_5_6_2<-(data[(data$Colony=="S1" & data$Colony=="S4" & data$Colony=="S5"), ])
