library(ggplot2)
require(reshape2)

butterflies <- read.csv('butterflies.csv')

# Create models for each response variable
dev_time_m <- lm(butterflies$DevelopmentTime ~ (butterflies$LarvalHost * butterflies$MaternalHost))
grwt_rate_m <- lm(butterflies$GrowthRate ~ butterflies$LarvalHost * butterflies$MaternalHost)
adlt_weight_m <- lm(butterflies$AdultWeight ~ butterflies$LarvalHost * butterflies$MaternalHost)

# Create data frames based on each combination of host plant, for plotting
df_barL_berM <- subset(butterflies, LarvalHost == "Barbarea" & MaternalHost == "Berteroa")
df_barL_barM <- subset(butterflies, LarvalHost == "Barbarea" & MaternalHost == "Barbarea")
df_berL_barM <- subset(butterflies, LarvalHost == "Berteroa" & MaternalHost == "Barbarea")
df_berL_berM <- subset(butterflies, LarvalHost == "Berteroa" & MaternalHost == "Berteroa")


# ~~~~~~ Development time ~~~~~~
# Create a new data frame with the mean development time, larval and maternal host species
# and standard error
dev_times_means <- c(mean(df_barL_berM$DevelopmentTime),
               mean(df_barL_barM$DevelopmentTime),
               mean(df_berL_barM$DevelopmentTime),
               mean(df_berL_berM$DevelopmentTime))
dev_times_hostL <- c(df_barL_berM$LarvalHost[1],
                     df_barL_barM$LarvalHost[1],
                     df_berL_barM$LarvalHost[1],
                     df_berL_berM$LarvalHost[1])
dev_times_hostM <- c(df_barL_berM$MaternalHost[1],
                     df_barL_barM$MaternalHost[1],
                     df_berL_barM$MaternalHost[1],
                     df_berL_berM$MaternalHost[1])
dev_times_se <- c(sd(df_barL_berM$DevelopmentTime)/sqrt(length(df_barL_berM$DevelopmentTime)),
                  sd(df_barL_barM$DevelopmentTime)/sqrt(length(df_barL_barM$DevelopmentTime)),
                  sd(df_berL_barM$DevelopmentTime)/sqrt(length(df_berL_barM$DevelopmentTime)),
                  sd(df_berL_berM$DevelopmentTime)/sqrt(length(df_berL_berM$DevelopmentTime)))

# Calculate the upper and lower confidence interval based on the standard error
dev_upper_error <- NULL
dev_lower_error <- NULL
for (i in 1:length(dev_times_se)){
  dev_upper_error[i] <- (dev_times_means[i]+dev_times_se[i])
  dev_lower_error[i] <- (dev_times_means[i]-dev_times_se[i])
}

# Build the new data frame and rename the columns
dev_times_df <- data.frame(dev_times_hostL,dev_times_hostM,dev_times_means,
                           dev_times_se,dev_upper_error,dev_lower_error)
colnames(dev_times_df) <- c("LarvalHost","MaternalHost","MeanDevTime",
                            "StandardError","UpperError","LowerError")

# Create a dot plot with a line connecting the corresponding dots and 
# the calculated standard error as error bars

dev_time_plt <- ggplot(data=dev_times_df, aes(x=LarvalHost,color=MaternalHost)) +
  geom_point(aes(y=MeanDevTime)) +
  geom_line(aes(y=MeanDevTime, group=MaternalHost)) +
  geom_errorbar(aes(ymin=LowerError, ymax=UpperError),width=0.1) +
  theme_classic() +
  labs(x="Larval host species", y="Mean development time (days)", 
       color="Maternal host species") +
  theme(legend.position=c(0.15,0.85))
print(dev_time_plt)

# Do 2-way ANOVA on the development time model and calculate the proportion of variance
# arising from each factor and the interaction
dev_time_an <- anova(dev_time_m)
tot_SS_dev <- sum(dev_time_an$Sum) # Total SS
dev_time_variance <- tot_SS_dev/(length(butterflies)-1)
dev_time_lhost_prop <- dev_time_an$`Sum Sq`[1]/tot_SS_dev
dev_time_mhost_prop <- dev_time_an$`Sum Sq`[2]/tot_SS_dev
dev_time_interact_prop <- dev_time_an$`Sum Sq`[3]/tot_SS_dev
dev_time_residual_prop <- dev_time_an$`Sum Sq`[4]/tot_SS_dev

# ~~~~~~ Growth rate ~~~~~~

# Create new data frame with mean growth rate, as for development time above
grwt_rate_means <- c(mean(df_barL_berM$GrowthRate),
                     mean(df_barL_barM$GrowthRate),
                     mean(df_berL_barM$GrowthRate),
                     mean(df_berL_berM$GrowthRate))
grwt_rate_se <- c(sd(df_barL_berM$GrowthRate)/sqrt(length(df_barL_berM$GrowthRate)),
                  sd(df_barL_barM$GrowthRate)/sqrt(length(df_barL_barM$GrowthRate)),
                  sd(df_berL_barM$GrowthRate)/sqrt(length(df_berL_barM$GrowthRate)),
                  sd(df_berL_berM$GrowthRate)/sqrt(length(df_berL_berM$GrowthRate)))

# calculate +- standard error
grwt_upper_error <- NULL
grwt_lower_error <- NULL
for (i in 1:length(grwt_rate_means)){
  grwt_upper_error[i] <- (grwt_rate_means[i]+grwt_rate_se[i])
  grwt_lower_error[i] <- (grwt_rate_means[i]-grwt_rate_se[i])
}
# re-use the development time vectors for larval and maternal host species to save time

grwt_rate_df <- data.frame(dev_times_hostL,dev_times_hostM,grwt_rate_means,
                           grwt_rate_se,grwt_upper_error,grwt_lower_error)
colnames(grwt_rate_df) <- c("LarvalHost","MaternalHost","MeanGrowthTime",
                            "StandardError","UpperError","LowerError")
# Make growth rate plot

grwth_rate_plt <- ggplot(data=grwt_rate_df, aes(x=LarvalHost,color=MaternalHost)) +
  geom_point(aes(y=MeanGrowthTime)) +
  geom_line(aes(y=MeanGrowthTime, group=MaternalHost)) +
  geom_errorbar(aes(ymin=LowerError, ymax=UpperError),width=0.1) +
  theme_classic() +
  labs(x="Larval host species", y="Mean growth rate", 
       color="Maternal host species") +
  theme(legend.position=c(0.8,0.85))
print(grwth_rate_plt)

# 2-way ANOVA on growth rate
grwt_rate_an <- anova(grwt_rate_m)
tot_SS_grwt <- sum(grwt_rate_an$Sum) # Total SS
grwt_rate_variance <- tot_SS_grwt/(length(butterflies)-1)
grwt_rate_lhost_prop <- grwt_rate_an$`Sum Sq`[1]/tot_SS_grwt
grwt_rate_mhost_prop <- grwt_rate_an$`Sum Sq`[2]/tot_SS_grwt
grwt_rate_interact_prop <- grwt_rate_an$`Sum Sq`[3]/tot_SS_grwt
grwt_rate_residual_prop <- grwt_rate_an$`Sum Sq`[4]/tot_SS_grwt

# ~~~~~~ Adult weight ~~~~~~

# Create new data frames with mean weights and standard error, as above
adlt_wght_means <- c(mean(df_barL_berM$AdultWeight),
                     mean(df_barL_barM$AdultWeight),
                     mean(df_berL_barM$AdultWeight),
                     mean(df_berL_berM$AdultWeight))
adlt_wght_se <- c(sd(df_barL_berM$AdultWeight)/sqrt(length(df_barL_berM$AdultWeight)),
                  sd(df_barL_barM$AdultWeight)/sqrt(length(df_barL_barM$AdultWeight)),
                  sd(df_berL_barM$AdultWeight)/sqrt(length(df_berL_barM$AdultWeight)),
                  sd(df_berL_berM$AdultWeight)/sqrt(length(df_berL_berM$AdultWeight)))

wght_upper_error <- NULL
wght_lower_error <- NULL
for (i in 1:length(adlt_wght_means)){
  wght_upper_error[i] <- (adlt_wght_means[i]+adlt_wght_se[i])
  wght_lower_error[i] <- (adlt_wght_means[i]-adlt_wght_se[i])
}

adlt_wght_df <- data.frame(dev_times_hostL,dev_times_hostM,adlt_wght_means,
                           adlt_wght_se,wght_upper_error,wght_lower_error)
colnames(adlt_wght_df) <- c("LarvalHost","MaternalHost","AdultWeight",
                            "StandardError","UpperError","LowerError")
# Create new plot for adult weight

adlt_wght_plt <- ggplot(data=adlt_wght_df, aes(x=LarvalHost,color=MaternalHost)) +
  geom_point(aes(y=AdultWeight)) +
  geom_line(aes(y=AdultWeight, group=MaternalHost)) +
  geom_errorbar(aes(ymin=LowerError, ymax=UpperError),width=0.1) +
  theme_classic() +
  labs(x="Larval host species", y="Adult wright", 
       color="Maternal host species") +
  theme(legend.position=c(0.8,0.85))
print(adlt_wght_plt)

# 2-way ANOVA and proportion of variance from variables

adlt_wght_an <- anova(adlt_weight_m)
tot_SS_wght <- sum(adlt_wght_an$Sum) # Total SS
adlt_wght_variance <- tot_SS_wght/(length(butterflies)-1)
adlt_wght_lhost_prop <- adlt_wght_an$`Sum Sq`[1]/tot_SS_wght
adlt_wght_mhost_prop <- adlt_wght_an$`Sum Sq`[2]/tot_SS_wght
adlt_wght_interact_prop <- adlt_wght_an$`Sum Sq`[3]/tot_SS_wght
adlt_wght_residual_prop <- adlt_wght_an$`Sum Sq`[4]/tot_SS_wght

