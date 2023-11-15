library(dplyr)
#library(tidyr)
#library(tidyverse)
library(ggplot2)

plants = read.csv(file="alpineplants.csv")
measure_ids <- seq(1,length(plants$Carex.bigelowii),1)
plants$ID <- measure_ids
plants <- na.omit(plants) # throw out empty lines of the table

#plants_long <- pivot_longer(plants,!ID,names_to = "name",values_to = "value")

# Plot histograms of data and also plot scatter plot of counts vs the predictors
# independently

intro_bigelowii <- ggplot(plants, aes(x=Carex.bigelowii)) +
  geom_histogram()
print(intro_bigelowii)

intro_alpinum <- ggplot(plants, aes(x=Thalictrum.alpinum)) +
  geom_histogram()
print(intro_alpinum)

bigelowii_winter_temp_plt <- ggplot(plants, aes(y=Carex.bigelowii)) +
  geom_point(aes(x=max_T_summer))
print(bigelowii_winter_temp_plt)

# After analysis models were created, see rationale below:

model_alpinum <- lm(plants$Thalictrum.alpinum ~ plants$mean_T_winter+
                      plants$mean_T_summer + plants$light + plants$altitude)
model_bigelowii <- lm(plants$Carex.bigelowii ~ plants$mean_T_winter+
                        plants$mean_T_summer + plants$light + plants$altitude)

# For both, models were inferred by looking at scatter plot of all variables against counts
# from this the mean winter and summer temps and light and altitude were suspected to be
# the best descriptors. Model revealed however that for alpinum only the mean temperatures
# were significant, and the light/altitude slopes were almost 0
# Data was z-transformed below.

mean_winter_z <- (plants$mean_T_winter - mean(plants$mean_T_winter))/sd(plants$mean_T_winter)
mean_summer_z <- (plants$mean_T_summer-mean(plants$mean_T_summer))/sd(plants$mean_T_summer)
light_z <- (plants$light-mean(plants$light))/sd(plants$light)
altitude_z <- (plants$altitude-mean(plants$altitude))/sd(plants$altitude)

model_alpinum_z <- lm(plants$Thalictrum.alpinum ~ mean_winter_z + mean_summer_z + light_z + altitude_z)
model_bigelowii_z <- lm(plants$Carex.bigelowii ~ mean_winter_z + mean_summer_z + light_z + altitude_z)

full_model_bigelowii <- lm(plants$Carex.bigelowii ~ 
                             plants$mean_T_summer + plants$min_T_summer + plants$max_T_summer +
                             plants$mean_T_winter + plants$max_T_winter + plants$min_T_winter +
                             plants$soil + plants$light + plants$soil_moist + plants$altitude)


full_model_alpinum <- lm(plants$Thalictrum.alpinum ~ 
                           plants$mean_T_summer + plants$min_T_summer + plants$max_T_summer +
                           plants$mean_T_winter + plants$max_T_winter + plants$min_T_winter +
                           plants$soil + plants$light + plants$soil_moist + plants$altitude)                                                                plants$soil + plants$light + plants$soil_moist + plants$altitude)
# The biggest factors are mean summer and winter temperature but only the mean, min and max
# summer temperatures are significant for alpinum

full_model_alpinum2 <- lm(plants$Thalictrum.alpinum ~ 
                               plants$mean_T_summer + plants$min_T_summer + plants$max_T_summer +
                               plants$mean_T_winter)
# At this point all values were significant. Suspected colinearity between summer temperatures

x1 <- lm(plants$mean_T_summer ~ plants$mean_T_winter)

# Variance inflation was calculated using below model, and was found to be >3 for mean and max
# summer temperature.

full_model_alpinum3 <- lm(plants$Thalictrum.alpinum ~ 
                            plants$mean_T_summer + plants$min_T_summer +
                            plants$mean_T_winter)
# now the min summer temp is no longer significant. final model becomes:

full_model_alpinum_final <- lm(plants$Thalictrum.alpinum ~ plants$mean_T_summer + plants$mean_T_winter)
