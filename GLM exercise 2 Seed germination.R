library(ggplot2)

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))

dat <- read.csv("dormancy.csv")

# Data exploration
cat('~~~~~~~~ Data set ~~~~~~~~\n')
print(dat)

cat('\n~~~~~~~~ Summary ~~~~~~~~\n')
print(summary(dat))

cat('\n~~~~~~~~ Unique values in columns ~~~~~~~~\n')
un_val <- sapply(dat, function(x) unique(x))
print(un_val)

cat('\n~~~~~~~~ Missing values in columns ~~~~~~~~\n')
missing_vals <- colSums(is.na(dat))
print(missing_vals)

# Make histograms
# Total population
explore_his_tot <- ggplot(dat,aes(x=germ2)) +
  geom_histogram() +
  theme_bw() +
  labs(x="Germination proportion",y="Count", title = "Histogram of total set")
print(explore_his_tot)
# Per population
explore_his_pop <- ggplot(dat,aes(x=germ2,color=pop,fill=pop))+
  geom_histogram()+
  facet_wrap("pop") +
  theme_bw() +
  labs(x="Germination proportion (fraction)",y="Count", title = "Histograms per population")
print(explore_his_pop)

# Plot parameters by population
# Time to sowing
explore_his_pop_time <- ggplot(dat,aes(x=timetosowing,color=pop,fill=pop))+
  geom_histogram()+
  facet_wrap("pop") +
  theme_bw() +
  labs(x="Time to watering (days)",y="Count", title = "Histograms per population")
print(explore_his_pop_time)

# Seed weight
explore_his_pop_wght <- ggplot(dat,aes(x=MCseed,color=pop,fill=pop))+
  geom_histogram()+
  facet_wrap("pop") +
  theme_bw() +
  labs(x="Mean centered seed weight (mg)",y="Count", title = "Histograms per population")
print(explore_his_pop_wght)

# Number of seeds
explore_his_pop_nseed <- ggplot(dat,aes(x=nseed,color=pop,fill=pop))+
  geom_histogram()+
  facet_wrap("pop") +
  theme_bw() +
  labs(x="Number of seeds",y="Count", title = "Histograms per population")
print(explore_his_pop_nseed)

# Data exploration seem to indicate there are differences between populations
# Investigate differences in populations before other factors

# Create new data frames divided up by population

dat_cc <- dat[dat$pop=="CC",]
dat_lm <- dat[dat$pop=="LM",]
dat_pm <- dat[dat$pop=="PM",]
dat_t <- dat[dat$pop=="T",]

# Begin with CC
mod_cc_time = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=dat_cc)
co_cc_time <- coef(mod_cc_time)
logit0_cc_time <- -co_cc_time[1]/co_cc_time[2]

cc_time_x_range <- seq(min(dat_cc$timetosowing),max(dat_cc$timetosowing),0.1)
cc_time_y_pred <- co_cc_time[1] + co_cc_time[2]*cc_time_x_range
cc_time_p_pred <- invlogit(cc_time_y_pred)
cc_time_prob_line <- data.frame(cbind(cc_time_p_pred,cc_time_x_range))


cc_time_plt <- ggplot(dat_cc, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=cc_time_prob_line,aes(x=cc_time_x_range,y=cc_time_p_pred)) +
  geom_vline(xintercept = logit0_cc_time, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: CC") +
  xlim(0,170)
print(cc_time_plt)

# LM
mod_lm_time = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=dat_lm)
co_lm_time <- coef(mod_lm_time)
logit0_lm_time <- -co_lm_time[1]/co_lm_time[2]

lm_time_x_range <- seq(min(dat_lm$timetosowing),max(dat_lm$timetosowing),0.1)
lm_time_y_pred <- co_lm_time[1] + co_lm_time[2]*lm_time_x_range
lm_time_p_pred <- invlogit(lm_time_y_pred)
lm_time_prob_line <- data.frame(cbind(lm_time_p_pred,lm_time_x_range))


lm_time_plt <- ggplot(dat_lm, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=lm_time_prob_line,aes(x=lm_time_x_range,y=lm_time_p_pred)) +
  geom_vline(xintercept = logit0_lm_time, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: LM") +
  xlim(0,170)
print(lm_time_plt)

# PM
mod_pm_time = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=dat_pm)
co_pm_time <- coef(mod_pm_time)
logit0_pm_time <- -co_pm_time[1]/co_pm_time[2]

pm_time_x_range <- seq(min(dat_pm$timetosowing),max(dat_pm$timetosowing),0.1)
pm_time_y_pred <- co_pm_time[1] + co_pm_time[2]*pm_time_x_range
pm_time_p_pred <- invlogit(pm_time_y_pred)
pm_time_prob_line <- data.frame(cbind(pm_time_p_pred,pm_time_x_range))


pm_time_plt <- ggplot(dat_pm, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=pm_time_prob_line,aes(x=pm_time_x_range,y=pm_time_p_pred)) +
  geom_vline(xintercept = logit0_pm_time, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: PM") +
  xlim(0,170)
print(pm_time_plt)

# T
mod_t_time = glm(germ2 ~ timetosowing, "binomial", weights=nseed, data=dat_t)
co_t_time <- coef(mod_t_time)
logit0_t_time <- -co_t_time[1]/co_t_time[2]

t_time_x_range <- seq(min(dat_t$timetosowing),max(dat_t$timetosowing),0.1)
t_time_y_pred <- co_t_time[1] + co_t_time[2]*t_time_x_range
t_time_p_pred <- invlogit(t_time_y_pred)
t_time_prob_line <- data.frame(cbind(t_time_p_pred,t_time_x_range))


t_time_plt <- ggplot(dat_t, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=t_time_prob_line,aes(x=t_time_x_range,y=t_time_p_pred)) +
  geom_vline(xintercept = logit0_t_time, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: T") +
  xlim(0,170)
print(t_time_plt)

# All populations aggregated:
mod_time = glm(germ2 ~ timetosowing, family=binomial(link="logit"), weights=nseed, data=dat)
co_time <- coef(mod_time)
logit0_time <- -co_time[1]/co_time[2]

time_x_range <- seq(min(dat$timetosowing),max(dat$timetosowing),0.1)
time_y_pred <- co_time[1] + co_time[2]*time_x_range
time_p_pred <- invlogit(time_y_pred)
time_prob_line <- data.frame(cbind(time_p_pred,time_x_range))


time_plt <- ggplot(dat, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=time_prob_line,aes(x=time_x_range,y=time_p_pred)) +
  geom_vline(xintercept = logit0_time, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: Full set") +
  xlim(0,170)
print(time_plt)

# Model analysis
print("\n############################### Model: Full~Time ###############################")
print(summary(mod_time))

print("\n############################### Model: CC~Time ###############################")
print(summary(mod_cc_time))

print("\n############################### Model: LM~Time ###############################")
print(summary(mod_lm_time))

print("\n############################### Model: PM~Time ###############################")
print(summary(mod_pm_time))

print("\n############################### Model: T~Time ###############################")
print(summary(mod_t_time))

# Compute D values
# CC
y_hat_cc <- co_cc_time[1] + co_cc_time[2]*cc_time_x_range
p_hat_cc = invlogit(y_hat_cc)
D_cc <- mean(p_hat_cc[which(dat_cc$germ2==1)]) - mean(p_hat_cc[which(dat_cc$germ2==0)])
# LM
y_hat_lm <- co_lm_time[1] + co_lm_time[2]*lm_time_x_range
p_hat_lm = invlogit(y_hat_lm)
D_lm <- mean(p_hat_lm[which(dat_lm$germ2==1)]) - mean(p_hat_lm[which(dat_lm$germ2==0)])
# PC
y_hat_pm <- co_pm_time[1] + co_pm_time[2]*pm_time_x_range
p_hat_pm = invlogit(y_hat_pm)
D_pm <- mean(p_hat_pm[which(dat_pm$germ2==1)]) - mean(p_hat_pm[which(dat_pm$germ2==0)])
# T
y_hat_t <- co_t_time[1] + co_t_time[2]*t_time_x_range
p_hat_t = invlogit(y_hat_t)
D_t <- mean(p_hat_t[which(dat_t$germ2==1)]) - mean(p_hat_t[which(dat_t$germ2==0)])

# Effect on full model including seed weight
# By defining both terms, you pick which to be "additive" by taking the other as x-axis
# In this case the model fit both timetosowing and MCseed
# But in the plot later i define x as timetosowing, meaning that MCseed will be invisible
# but added in as a range +- 1 SD of MCseed to show its impact.

mod_time_weight <- glm(germ2 ~ timetosowing + MCseed, family=binomial(link="logit"), weights=nseed,data=dat )
co_time_weight <- coef(mod_time_weight)
logit0_time_weight <- -co_time_weight[1]/co_time_weight[2]

print(summary(mod_time_weight))
# In this case, the model is showing a negative effect of seed weight, meaning larger seeds
# germinated slower. 

#Calculate predicted values for trend line

time_weight_x_range <- seq(min(dat$timetosowing),max(dat$timetosowing),0.1)
time_weight_y_pred <- co_time_weight[1] + co_time_weight[2]*time_weight_x_range
time_weight_p_pred <- invlogit(time_weight_y_pred)
time_weight_prob_line <- data.frame(cbind(time_weight_p_pred,time_weight_x_range))

time_weight_y_pred_2 <- co_time_weight[1] + co_time_weight[2]*time_weight_x_range + co_time_weight[3]*sd(dat$MCseed)
time_weight_p_pred_2 <- invlogit(time_weight_y_pred_2)
time_weight_prob_line_2 <- data.frame(cbind(time_weight_p_pred_2,time_weight_x_range))

time_weight_y_pred_3 <- co_time_weight[1] + co_time_weight[2]*time_weight_x_range - co_time_weight[3]*sd(dat$MCseed)
time_weight_p_pred_3 <- invlogit(time_weight_y_pred_3)
time_weight_prob_line_3 <- data.frame(cbind(time_weight_p_pred_3,time_weight_x_range))

# Calculate new 0 points for the 50 % line for the +- SD of MCweight

logit0_time_weight_pos <- -(co_time_weight[1] + co_time_weight[3]*sd(dat$MCseed))/co_time_weight[2]
logit0_time_weight_neg <- -(co_time_weight[1] - co_time_weight[3]*sd(dat$MCseed))/co_time_weight[2]
#pos = 128.5847 days
#neg = 105.1757 days

time_weight_plt <- ggplot(dat, aes(x=timetosowing,y=germ2)) +
  geom_point() +
  geom_line(data=time_weight_prob_line,aes(x=time_weight_x_range,y=time_weight_p_pred)) +
  geom_line(data=time_weight_prob_line_2,aes(x=time_weight_x_range,y=time_weight_p_pred_2), linetype = "dashed") +
  geom_line(data=time_weight_prob_line_3,aes(x=time_weight_x_range,y=time_weight_p_pred_3), linetype = "dashed") +
  geom_vline(xintercept = logit0_time_weight, linetype="dashed") +
  geom_vline(xintercept = logit0_time_weight_pos, linetype="dashed") +
  geom_vline(xintercept = logit0_time_weight_neg, linetype="dashed") +
  labs(x="Time to sowing (days)", t="Germination (proportion)", title = "Population: Full set, Afterripening and seed weight") +
  xlim(0,170)
print(time_weight_plt)
