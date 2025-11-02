library(ggplot2)
library(dplyr)
library(lme4)
library(DHARMa)

phase1_fitness <- read.csv("C:/Users/nikki/OneDrive - University of Toronto/PhD Research/Parasitism Content/Distributed Experiment/Distributed Experiment/Distributed_Experiment/Phase1_fitness_raw.csv")

phase1_clean <- read.csv("C:/Users/nikki/OneDrive - University of Toronto/PhD Research/Parasitism Content/Distributed Experiment/Distributed_Experiment/Phase1_fitness_Notonecta.undulata.csv")

phase1_clean <- phase1_clean %>%
  mutate(Temp.treatment = case_when(Temp.value == 16 ~ "low", Temp.value == 26 ~ "opt", Temp.value == 30 ~ "high"))
write.csv(phase1_clean, "Phase1_fitness_Notonecta.undulata.csv")

# Temp on Mortality -------------------------------------------------------
  #treatment is classed as integer rather than as a factor
#making mesocosm unique across treatments
phase1_fitness$Mesocosm <- with(phase1_fitness, paste(Block, Mesocosm, sep = "."))

mortality <- glmer(Survivorship ~ Treatment + (1|Block/Mesocosm), data = phase1_fitness, family = binomial(link="logit"))
  #the model is being overfit but cannot reduce any terms 
summary(mortality) #temp has a significant effect on survivorship (p < 0.001)
drop1(mortality, test = 'Chisq')

exp(-0.22474) / (1 + exp(-0.22474))

# predict survival
nd <- data.frame(unique(subset(phase1_fitness, select =c(Treatment))), "Mesocosm"=NA)

mySumm_mod <- function(.){
  predict(., newdata=nd, re.form=~0)
}

boot <- bootMer(mortality, mySumm_mod, nsim = 200, use.u=F, type="parametric")

sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

est_mort <- sumBoot(boot)

p_mort <- exp(est_mort) / (1 + exp(est_mort))

result_mort <- cbind(nd, p_mort);result_mort

surv <- result_mort %>% 
  group_by(Treatment) %>%
  summarise_at(vars(fit), list(Surv_Prob = mean))


plot <- ggplot(result_mort, aes(x = Treatment, y = fit))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.5, col=NA)+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Survival Probability", limits = c(0:1), breaks = c(0, 0.25, 0.5, 0.75, 1.0))+
  theme_bw(base_size=12)+ 
  theme(element_text = 18)+
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
plot

# Temp on Body Condition --------------------------------------------------

survived <- subset(phase1_fitness, Survivorship == 1)
survived$Body_Condition <- as.numeric(survived$Body_Condition)
survived$Treatment <- as.numeric(survived$Treatment)

#Making mesocosm unique across blocks
survived$Mesocosm <- with(survived, paste(Block, Mesocosm, sep = "."))

  #won't let me include block in the model bc for block 2 only 1 individual in 30 deg treatment
    #error - must have >1 sampled 
condition <- lmer(Body_Condition ~ Treatment + (1|Mesocosm), data = survived)
summary(condition)
drop1(condition, test = 'Chisq') #No significant effect of treatment on body condition


plot <- ggplot(survived, aes(x = Treatment, y = Body_Condition))+
  geom_smooth(method="lm", col = "black")+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Body Condition")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
plot



# Temp on Avg Egg Production ----------------------------------------------
#Using thermal performance curve for this 
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(purrr)

head(phase1_fitness)

df <- phase1_clean

df$logFitness <- log(df$Fitness)
df$logFitness[is.infinite(df$logFitness)] <- 0

avgegg_mod <- lmer(log1p(Fitness) ~ poly(Temp.value,2) + (1|Block/Mesocosm), data = df)
summary(avgegg_mod)
drop1(avgegg_mod, test = "Chisq")


eggplot <- ggplot(df, aes(x = Temp.value, y = log1p(Fitness)))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Total Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
eggplot

eggbar <- ggplot(df, aes(x = as.factor(Temp.value), y = Total_Egg))+
  geom_bar(stat = "identity")+
  labs(x = "Temperature Treatment", y = "Total Egg Production")
eggbar

#visualization of points
ggplot(df, aes(Temp.value, logFitness)) +
  geom_point() +
  geom_smooth()+
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Average Daily Egg Production')

#visualization at barplot
ggplot(df, aes(x = Temp.value, y = logFitness))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Egg Production")+
  theme_bw()+
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))+
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(2)), axis.title.y=element_text(size=rel(2)))


#Model 1
model <- nls_multstart(logFitness~sharpeschoolhigh_1981(temp = Temp.value, r_tref,e,eh,th, tref = 15),
                      data = df,
                      iter = 500,
                      start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'sharpeschoolhigh_1981') - 10,
                      start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'sharpeschoolhigh_1981') + 10,
                      lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'sharpeschoolhigh_1981'),
                      upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'sharpeschoolhigh_1981'),
                      supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model)
calc_params(model) %>%
  mutate_all(round, 2)
  #topt = 27.28

# Generate predicted values
df$predicted <- predict(model)

t <- as.data.frame(table(df$Temp.value, df$predicted))
t <- subset(t, Freq > 0)
colnames(t) <- c("Temp.value", "Fitness", "Freq")
t$Temp.value <- as.integer(as.character(t$Temp.value))
t$logFitness <- as.numeric(as.character(t$logFitness))

ggplot(df, aes(x = Temp.value, y = logFitness)) +
  geom_point() +
  geom_line(data = t, aes(Temp.value, Fitness), col = 'blue') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


#Model 2
model2 <- nls_multstart(Fitness~beta_2012(temp = Temp.value, a,b,c,d,e),
                       data = df,
                       iter = c(6,6,6,6,6),
                       start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'beta_2012') - 10,
                       start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'beta_2012') + 10,
                       lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'beta_2012'),
                       upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'beta_2012'),
                       supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model2)
calc_params(model2) %>%
  mutate_all(round, 2)
  #topt = 29.53

# Generate predicted values
df$predicted2 <- predict(model2)

t2 <- as.data.frame(table(df$Temp.value, df$predicted2))
t2 <- subset(t2, Freq > 0)
colnames(t2) <- c("Temp.value", "Fitness", "Freq")
t2$Temp.value <- as.integer(as.character(t2$Temp.value))
t2$Fitness <- as.numeric(as.character(t2$Fitness))

ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t2, aes(Temp.value, Fitness), col = 'green') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


#Model 3
model3 <- nls_multstart(Fitness~boatman_2017(temp = Temp.value, rmax, tmin, tmax, a,b),
                        data = df,
                        iter = c(4,4,4,4,4),
                        start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'boatman_2017') - 10,
                        start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'boatman_2017') + 10,
                        lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'boatman_2017'),
                        upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'boatman_2017'),
                        supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model3)
calc_params(model3) %>%
  mutate_all(round, 2)
#topt = 28.26

# Generate predicted values
df$predicted3 <- predict(model3)

t3 <- as.data.frame(table(df$Temp.value, df$predicted3))
t3 <- subset(t3, Freq > 0)
colnames(t3) <- c("Temp.value", "Fitness", "Freq")
t3$Temp.value <- as.integer(as.character(t3$Temp.value))
t3$Fitness <- as.numeric(as.character(t3$Fitness))

ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t3, aes(Temp.value, Fitness), col = 'red') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


#Model 4
model4 <- nls_multstart(Fitness~gaussian_1987(temp = Temp.value, rmax, topt, a),
                        data = df,
                        iter = c(4,4,4),
                        start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'gaussian_1987') - 10,
                        start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'gaussian_1987') + 10,
                        lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'gaussian_1987'),
                        upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'gaussian_1987'),
                        supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model4)
calc_params(model4) %>%
  mutate_all(round, 2)
#topt = 29.68

# Generate predicted values
df$predicted4 <- predict(model4)

t4 <- as.data.frame(table(df$Temp.value, df$predicted4))
t4 <- subset(t4, Freq > 0)
colnames(t4) <- c("Temp.value", "Fitness", "Freq")
t4$Temp.value <- as.integer(as.character(t4$Temp.value))
t4$Fitness <- as.numeric(as.character(t4$Fitness))

ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t4, aes(Temp.value, Fitness), col = 'gold') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


#Model 5
model5 <- nls_multstart(Fitness~briere2_1999(temp = Temp.value, tmin, tmax, a, b),
                        data = df,
                        iter = c(4,4,4,4),
                        start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'briere2_1999') - 10,
                        start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'briere2_1999') + 10,
                        lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'briere2_1999'),
                        upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'briere2_1999'),
                        supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model5)
calc_params(model5) %>%
  mutate_all(round, 2)
#topt = 28.03

# Generate predicted values
df$predicted5 <- predict(model5)

t5 <- as.data.frame(table(df$Temp.value, df$predicted5))
t5 <- subset(t5, Freq > 0)
colnames(t5) <- c("Temp.value", "Fitness", "Freq")
t5$Temp.value <- as.integer(as.character(t5$Temp.value))
t5$Fitness <- as.numeric(as.character(t5$Fitness))

ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t5, aes(Temp.value, Fitness), col = 'purple') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))

#Model 6
model6 <- nls_multstart(Fitness~kamykowski_1985(temp = Temp.value, tmin, tmax, a, b, c),
                        data = df,
                        iter = c(3,3,3,3,3),
                        start_lower = get_start_vals(df$Temp.value, df$Fitness, model_name = 'kamykowski_1985') - 10,
                        start_upper = get_start_vals(df$Temp.value, df$Fitness, model_name = 'kamykowski_1985') + 10,
                        lower = get_lower_lims(df$Temp.value, df$Fitness, model_name = 'kamykowski_1985'),
                        upper = get_upper_lims(df$Temp.value, df$Fitness, model_name = 'kamykowski_1985'),
                        supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model6)
calc_params(model6) %>%
  mutate_all(round, 2)
#topt = 29.54

# Generate predicted values
df$predicted6 <- predict(model6)

t6 <- as.data.frame(table(df$Temp.value, df$predicted6))
t6 <- subset(t6, Freq > 0)
colnames(t6) <- c("Temp.value", "Fitness", "Freq")
t6$Temp.value <- as.integer(as.character(t6$Temp.value))
t6$Fitness <- as.numeric(as.character(t6$Fitness))

ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t6, aes(Temp.value, Fitness), col = 'hotpink') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))



#Overall plot
ggplot(df, aes(x = Temp.value, y = Fitness)) +
  geom_point() +
  geom_line(data = t, aes(Temp.value, Fitness, col = 'sharpeschoolhigh_1981')) +
  geom_line(data = t2, aes(Temp.value, Fitness, col = 'beta_2012')) +
  geom_line(data = t3, aes(Temp.value, Fitness, col = 'boatman_2017')) +
  geom_line(data = t4, aes(Temp.value, Fitness, col = 'gaussian_1987')) +
  geom_line(data = t5, aes(Temp.value, Fitness, col = 'briere2_1999')) +
  geom_line(data = t6, aes(Temp.value, Fitness, col = 'kamykowski_1985')) +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Average Daily Egg Production")+
  scale_colour_manual(name = 'Model Name', 
                      breaks = c('sharpeschoolhigh_1981','beta_2012', 'boatman_2017',
                                                     'gaussian_1987', 'briere2_1999', 'kamykowski_1985'),
                      values=c('sharpeschoolhigh_1981'='blue', 'beta_2012'='green', 'boatman_2017'='red',
                               'gaussian_1987'='gold', 'briere2_1999'='purple', 'kamykowski_1985'='hotpink'))+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


tab <- rbind(t,t2,t3,t4,t5,t6)
tavg <- tab %>% 
  group_by(Temp.value) %>%
  summarise_at(vars(Fitness), list(Mean_Egg = mean))

surv <- result_mort %>% 
  group_by(Temp.value) %>%
  summarise_at(vars(fit), list(Surv_Prob = mean))

fittab <- cbind(tavg, surv$Surv_Prob)
fittab$Fitness <- fittab$Mean_Egg*fittab$`surv$Surv_Prob`

ggplot(fittab, aes(x = Temp.value, y = Fitness, col = 'Overall'))+
  geom_point()+
  geom_line()+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  geom_line(data = fittab, aes(x = Temp.value, y = `surv$Surv_Prob`, col = 'Survival'))+
  geom_line(data = fittab, aes(x = Temp.value, y = Mean_Egg, col = 'EggProduction'))+
    scale_y_continuous("Fitness")+
    scale_colour_manual(name = 'Fitness metric', 
                        breaks = c('Overall','Survival', 'EggProduction'),
                        values=c('Overall'='red', 'Survival'='blue', 'EggProduction'='green'))+
      scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
    theme_bw(base_size=12)+ 
    theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
  
  

# Temp on Total Egg Production --------------------------------------------

df <- phase1_fitness

# Converting egg production NA values to 0
df <- df %>%                       
  replace(is.na(.), 0)  

df <- df%>% 
  mutate(TotalEgg = rowSums(df[c(7:13)])) 

#finding the mean total egg production  by treatment
df1 <- df %>% 
  group_by(Treatment) %>%
  summarise_at(vars(TotalEgg), list(AvgTotal = mean))
df1
#finding the bootstrapped mean egg production by treatment
library(Hmisc)
df1<- df %>% 
  select(TotalEgg, Treatment) %>% 
  group_by(Treatment) %>%
  group_map(~ smean.cl.boot(., conf.int = .95, B = 1000, na.rm = TRUE)) %>%
  bind_rows()
df1
  
  
  summarise(data = list(smean.cl.boot(cur_data(), conf.int = .95, B = 1000, na.rm = TRUE))) %>%
  tidyr::unnest_wider(data)


library(boot)

mean_egg <- function(data, indices) {
  treatment_means <- tapply(data[indices, "TotalEgg"], data[indices, "Treatment"], mean)
  return(treatment_means)
}

# Assuming your dataframe is called 'df' and has columns 'treatment' and 'egg'
# Perform bootstrap resampling
boot_results <- boot(df, mean_egg, R = 1000)
print(boot_results)

boot_results <- boot(df$TotalEgg, statistic=mean, R = 1000)

# Get the bootstrapped mean of 'egg' for each treatment
boot_means <- boot_results$t

# Print the bootstrapped means
print(boot_means)

boot_means <- boot_results$t

# Calculate summary statistics
summary_stats <- apply(boot_means, 1, mean)

# Print the bootstrapped mean
print(summary_stats)

#Model 1
model <- nls_multstart(TotalEgg~sharpeschoolhigh_1981(temp = Treatment, r_tref,e,eh,th, tref = 15),
                       data = df,
                       iter = 500,
                       start_lower = get_start_vals(df$Treatment, df$TotalEgg, model_name = 'sharpeschoolhigh_1981') - 10,
                       start_upper = get_start_vals(df$Treatment, df$TotalEgg, model_name = 'sharpeschoolhigh_1981') + 10,
                       lower = get_lower_lims(df$Treatment, df$TotalEgg, model_name = 'sharpeschoolhigh_1981'),
                       upper = get_upper_lims(df$Treatment, df$TotalEgg, model_name = 'sharpeschoolhigh_1981'),
                       supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model)
calc_params(model) %>%
  mutate_all(round, 2)
#topt = 28.09

# Generate predicted values
df$predicted <- predict(model)

t <- as.data.frame(table(df$Treatment, df$predicted))
t <- subset(t, Freq > 0)
colnames(t) <- c("Treatment", "TotalEgg", "Freq")
t$Treatment <- as.integer(as.character(t$Treatment))
t$TotalEgg <- as.numeric(as.character(t$TotalEgg))

ggplot(df, aes(x = Treatment, y = TotalEgg)) +
  geom_point() +
  geom_line(data = t, aes(Treatment, TotalEgg), col = 'blue') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Total Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


#Model 2
model2 <- nls_multstart(TotalEgg~beta_2012(temp = Treatment, a,b,c,d,e),
                        data = df,
                        iter = c(6,6,6,6,6),
                        start_lower = get_start_vals(df$Treatment, df$TotalEgg, model_name = 'beta_2012') - 10,
                        start_upper = get_start_vals(df$Treatment, df$TotalEgg, model_name = 'beta_2012') + 10,
                        lower = get_lower_lims(df$Treatment, df$TotalEgg, model_name = 'beta_2012'),
                        upper = get_upper_lims(df$Treatment, df$TotalEgg, model_name = 'beta_2012'),
                        supp_errors = 'Y')


# Extract parameter estimates
params <- coef(model2)
calc_params(model2) %>%
  mutate_all(round, 2)
#topt = 29.53

# Generate predicted values
df$predicted2 <- predict(model2)

t2 <- as.data.frame(table(df$Treatment, df$predicted2))
t2 <- subset(t2, Freq > 0)
colnames(t2) <- c("Treatment", "TotalEgg", "Freq")
t2$Treatment <- as.integer(as.character(t2$Treatment))
t2$TotalEgg <- as.numeric(as.character(t2$TotalEgg))

ggplot(df, aes(x = Treatment, y = TotalEgg)) +
  geom_point() +
  geom_line(data = t2, aes(Treatment, TotalEgg), col = 'green') +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Total Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))


ggplot(df, aes(x = Treatment, y = TotalEgg)) +
  geom_point() +
  geom_line(data = t, aes(Treatment, TotalEgg, col = 'sharpeschoolhigh_1981')) +
  geom_line(data = t2, aes(Treatment, TotalEgg, col = 'beta_2012')) +
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Total Egg Production")+
  scale_colour_manual(name = 'Model Name', 
                      breaks = c('sharpeschoolhigh_1981','beta_2012'),
                      values=c('sharpeschoolhigh_1981'='blue', 'beta_2012'='green'))+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))





totalegg <- glmer(Total_Egg ~ Temp.value + (1|Block/Mesocosm), data = df, family = poisson(link = "log"))
summary(totalegg)


eggplot <- ggplot(df, aes(x = Temp.value, y = Total_Egg))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = poisson))+
  scale_x_continuous("Temperature", breaks = c(16, 22, 24, 26, 28, 30))+
  scale_y_continuous("Total Egg Production")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
eggplot

eggbar <- ggplot(df, aes(x = as.factor(Temp.value), y = Total_Egg))+
  geom_bar(stat = "identity")+
  labs(x = "Temperature Treatment", y = "Total Egg Production")
eggbar

