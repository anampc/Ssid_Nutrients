# -------------------------------------------------------------------------------------------------
# LONG FORMAT DATA ANALYSES FOR SSID

# Modify Ssid.csv and save it with the day of last data added
# Import last data

# -------------------------------------------------------------------------------------------------

getwd()
library (lattice)
library(ggplot2)
library(nlme)


# 1. ----Data structure and order of levels in each factor

      data<-read.csv("data/Ssid-11-26-15.csv")
      data$Colony <- as.factor(data$Colony)
      data$Community<-factor(as.character(data$Community), 
                             levels=c("C","DC","D")) # DC = (0.1:0.9)Dporportion 
      data$Time <- as.factor(data$Time)
      data$Repli <- as.factor(data$Repli)
      data$Treatment <- factor (as.character(data$Treatment), 
                                levels=c("CO2","Cont","Dark","Fe", "N", "NP", "NPF"))
      data$D.Prop.t<-asin(sqrt(data$D.Prp))
      Summary<-summary(data)

# 2. EXPLORE DATA WITH GGPLOT 

      # A. Frecuency of dominant Symbionts by factors
      data$Colony<-factor(as.character(data$Colony), 
                          levels=c("S1","S4","S3","S5","S6","S2")) 
      # Due to D dominance order
            
            # Histograms of D dominates vs C dominates and DCmixed
            Com <- ggplot(data = data, aes(x=Time)) 
            Com <- Com + geom_histogram(aes(fill=Community))
            Com <- Com + scale_fill_brewer(palette="Set3")
            #       Few C dominated samples, pool Cdom and DC 
      
      # Figure 1: Hitogram - Colony Symbionts through Time
      Fig1 <- Com + facet_wrap( ~ Colony, ncol=1)
      
      # Figure 2: Hitogram -Treatment * Colony Symbionts through Time
      Fig2 <- Com + facet_wrap( ~ Treatment, ncol=1)
      
      # Maybe saperate data and run analysis for only D vs (C + DC)? 
      # New variable = Type ("Ddom">90%D, "DCmix"<90%D)
            data$Type<-c("Ddom", "DCmix") # Ddom.90% D
            data$Type[which(data$Colony=="S1" | data$Colony=="S4" | data$Colony=="S3")] <- "DCmix"
            data$Type[which(data$Colony=="S5" | data$Colony=="S6" | data$Colony=="S2")] <- "Ddom"
      
      
      # B. S:H and D proportions Ratio by factors through Time
      
      # Figure 3: Total SH by Treatmemnt*Time*Colony  
        logSHTreat <- ggplot(data, aes(factor(Treatment), logTot.SH))
      Fig3<-logSHTreat + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Colony~., margins=TRUE)
            # Very difficult to interpret, only Dark clearly increased
            # pool data by Type of Community or
            # Split total by C:H and D:H changes through time
            
      # Figure 4: Total SH by Treatmemnt*Time*TYPE of community 
      #           (Colonies pooled by D dominanance)  
      Fig4<-logSHTreat + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Type~., margins=TRUE)
      
      
      # Figure 5: Change in D proportion by Treatmemnt*Time*Colony 
         Dprp <- ggplot(data, aes(factor(Treatment), D.Prp))
      Fig5<-Dprp + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Type~., margin=T)  
      
      # Figure 6-7: Change in C:H ratio by Treatmemnt*Time*Colony or Type
       logCH <- ggplot(data, aes(factor(Treatment), logC.SH))
      Fig6<-logCH + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Colony~., margin=T)  
      # CH increased in Dark, N, Fe?, but not NP, or NPF
      # Colony S2 does not change at all!!! Check this data
      Fig7<-logCH + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Type~., margin=T)
      
      
      # Figure 8-9: Change in D:H ratio by Treatmemnt*Time*Colony or Type
          logDH <- ggplot(data, aes(factor(Treatment), logD.SH))
      Fig8<-logDH + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Colony~., margin=T)  
      # DH increased in Dark, unclar other paterns across colonies
      # Again, colony S2 does not change at all!!! 
      Fig9<-logDH + geom_boxplot(aes(fill=factor(Time)))+ facet_grid(Type~., margin=T)
      

# -------------------------------------------------------------------------------------------------
# ANALYSIS
# -------------------------------------------------------------------------------------------------
# Linear model with Time as repeated messurements
# Factors
#   Random: Time (T0, T1, T2, T3) - Core nested? Look at repeated messurements design
#           Colony? How to separate genotype from Symbiont community in a colony?
#           Replicate (R1,R2)
#   Fixed:  Treatment(Control, Dark, Fe, N, NP, NPF), discard CO2  
# Community? or Dprp? or CH_T0?

# ANOVA, not correct for repeated mess, but to have an idea
        aov1<-aov (logTot.SH ~ Time*Treatment*Type*Repli, data=data)
        summary(aov1) # Every factor and interaction (except Time:Repli and *) has an effect!
        plot(aov1) 

# LME Should be ok, but need criteria to eliminate non informative factors
      LME<- lme(logTot.SH ~ Time*Treatment*Colony, 
                random = ~ Time|Core, na.action = na.exclude, data=data)
      summary(LME)
      plot(LME)
