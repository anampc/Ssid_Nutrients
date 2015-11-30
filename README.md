# Ssid_Nutrients
Nutients effect on coral S:H ratio


data$Community<-factor(as.character(data$Community), levels=c("C","DC","D"))
      data$Sample.Time <- as.factor(data$Sample.Time)
      data$Community<- as.factor(data$Community)
      data$Colony <- as.factor(data$Colony)
      data$Treatment <- as.factor(data$Treatment)
      data$Time <- as.factor(data$Time)
      data$Repli <- as.factor(data$Repli)
