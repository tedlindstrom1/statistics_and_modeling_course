library(ggplot2)
require(reshape2)

butterflies <- read.csv('butterflies.csv')
# Mark host for Larvae and Maternal
butterflies$MaternalHost = paste0(butterflies$MaternalHost, "M")
butterflies$LarvalHost = paste0(butterflies$LarvalHost, "L")

dev_time_m <- lm(butterflies$DevelopmentTime ~ (butterflies$LarvalHost * butterflies$MaternalHost))
grwt_rate_m <- lm(butterflies$GrowthRate ~ butterflies$LarvalHost * butterflies$MaternalHost)
adlt_weight_m <- lm(butterflies$AdultWeight ~ butterflies$LarvalHost * butterflies$MaternalHost)

#Development time

means = tapply(butterflies$DevelopmentTime, list(butterflies$MaternalHost, butterflies$LarvalHost), mean)
df <- data.frame(means)
df$group <- c("Barbarea","Berteroa")
# Test rotating data!!!!
dev_time_meanp <- ggplot(data=df, aes(x=df$group, color=df$group)) +
  geom_point(aes(y=df$BarbareaL)) +
  geom_line(aes(y=df$BarbareaL)) +
  geom_point(aes(y=df$BerteroaL)) +
  geom_line(aes(y=df$BerteroaL)) +
  labs(x="Host species", y="Mean development time") +
  scale_color_manual(name="Maternal host", breaks=c("Barbarea","Bartorea"), values=c("Barbarea" = "red","Bartorea"="blue"))
  
print(dev_time_meanp)

dev_time_an <- anova(dev_time_m)
tot_SS_dev <- sum(dev_time_an$Sum) # Total SS
dev_time_variance <- tot_SS_dev/(length(butterflies)-1)
dev_time_lhost_prop <- dev_time_an$`Sum Sq`[1]/tot_SS_dev
dev_time_mhost_prop <- dev_time_an$`Sum Sq`[2]/tot_SS_dev
dev_time_interact_prop <- dev_time_an$`Sum Sq`[3]/tot_SS_dev
dev_time_residual_prop <- dev_time_an$`Sum Sq`[4]/tot_SS_dev


#dev_time_df <- data.frame(butterflies$LarvalHost, butterflies$MaternalHost, butterflies$DevelopmentTime)
#dev_time_df <- setNames(dev_time_df,c("LarvalHost","MaternalHost","DevelopmentTime"))
#df_t <- melt(dev_time_df)



