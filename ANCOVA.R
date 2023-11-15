library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

blossoms = read.csv("blossoms.csv")
blossoms <-na.omit(blossoms)

plt <- ggplot(blossoms) +
  geom_histogram(aes(x=GA))
print(plt)
colnames(blossoms)

blossoms_sum <- blossoms %>% group_by(pop) %>% 
                summarise(ASDm=mean(ASD), ASDsd=sd(ASD), 
                          GADm=mean(GAD), GADsd=sd(GAD),
                          GSDm=mean(GSD), GSDsd=sd(GSD),
                          LBLm=mean(LBL), LBLsd=sd(LBL),
                          LBWm=mean(LBW), LBWsd=sd(LBW),
                          UBLm=mean(UBL), UBLsd=sd(UBL),
                          UBWm=mean(UBW), UBWsd=sd(UBW),
                          GWm=mean(GW), GWsd=sd(GW),
                          GAm=mean(GA), GAsd=sd(GA))

plt2 <- ggplot(blossoms_sum,aes(x=pop,y=GAm)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=GAm-GAsd, ymax=GAm+GAsd))
print(plt2)

model = lm(blossoms$LBL ~ blossoms$GA*blossoms$UBL)

ga_df <- data.frame(blossoms$GA,blossoms$LBL)
ga_df <- pivot_longer(ga_df, blossoms.GA)

ubl_df <-data.frame(blossoms$UBL,blossoms$LBL)
ubl_df <- pivot_longer(ubl_df,blossoms.UBL)

df <- rbind(ubl_df,ga_df)

plt3 <- ggplot(df,aes(color=name)) +
  geom_point(aes(x=value, y=blossoms.LBL,group=name)) 
  #geom_point(aes(x=GA))
print(plt3)