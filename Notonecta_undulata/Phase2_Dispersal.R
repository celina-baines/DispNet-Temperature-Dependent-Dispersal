library(ggplot2)
library(dplyr)
library(lme4)
library(DHARMa)

p2sa <- read.csv("phase2_survivalanalysis.csv")

phase2 <- read.csv("Phase2_dispersal_Notonecta.undulata.csv")

phase2_clean <- phase2%>%
  mutate(Temp.treatment = case_when(Temp.value == 16 ~ "low", Temp.value == 26 ~ "opt", Temp.value == 30 ~ "high"))
write.csv(phase2_clean, "Phase2_dispersal_Notonecta.undulata.csv")

p2summary <- phase2 %>%
  group_by(Mesocosn, Treatment) %>%
  summarise

#ensuring date columns are dates
phase2 <-  phase2 %>%
  mutate(across(c(Death_Date, Disp_Date), ~ as.Date(., format("%d-%h-%y"))))

#adding a new column that specifies experiment day
start_date <- as.Date("2024-08-20")
phase2 <- phase2 %>%
  mutate(Death_Day  = as.numeric(Death_Date - start_date) + 1, Disp_Day  = as.numeric(Disp_Date - start_date) + 1)



# Creating survival analysis df -------------------------------------------

#These are the days between the blocks where dispersal data was assessed
p2rep <- phase2[rep(seq_len(nrow(phase2)), each = 14), ]
days <- seq(1:14)
p2rep$Day = rep(days, times = nrow(p2rep)/14)

#survival analysis for dispersal
p2rep$DispFate <- ifelse(p2rep$Survival == 1 & p2rep$Dispersal == 0, 0, #If an individual survives but doesn't disperse, then 0
                             ifelse(p2rep$Survival == 1 & p2rep$Dispersal == 1, #If an individual survives and does disperse, then...
                                    ifelse(p2rep$Day < p2rep$Disp_Day, 0, #All days before they disperse are 0
                                           ifelse(p2rep$Day == p2rep$Disp_Day, 1, NA)), #The day they actually disperse is 1, days after dispersal are NA
                                    ifelse(p2rep$Survival == 0 & p2rep$Day < p2rep$Death_Day, 0, NA))) #If an individual dies, days before they die dispersal is 0 but the day they die and days following are all NA

#survival analysis for survival

p2rep$SurvivalFate <- ifelse(p2rep$Survival == 1 & p2rep$Dispersal == 0, 1, #If survived and didn't disperse then survived entire time
                                ifelse(p2rep$Survival == 0 & p2rep$Day < p2rep$Death_Day, 1, #if they ultimately die, still 1 before day of death
                                       ifelse(p2rep$Day == p2rep$Death_Day, 0, NA))) #if cannibalism is COD then 1 on day they die and after that the individual is NA

write.csv(p2rep, "phase2_survivalanalysis.csv")

# Regulating mesocosm temps -----------------------------------------------

#Overall Averages by Treatment
tempregulate<- read.csv("C:/Users/nikki/OneDrive - University of Toronto/PhD Research/Parasitism Content/Distributed Experiment/Phase 2/phase2_tempregulation.csv")

#tempregulate$Date <- as.Date(tempregulate$Date)

tempregulate <- tempregulate %>%
  mutate(across(c(Date), ~ as.Date(., format("%d-%h-%y")))) %>%
  filter(Date >= "2024-08-12" & Date <= "2024-08-16")
  

tempregulate <- na.omit(tempregulate)
tempregulate$Treatment <- as.factor(tempregulate$Treatment)

sdtemp <- tempregulate %>% 
  group_by(Treatment) %>%
  summarise_at(vars(Temp), list(StandardDeviation = sd))

avgtemp <- tempregulate %>% 
  group_by(Treatment) %>%
  summarise_at(vars(Temp), list(Mean_Temp = mean))

tempsummary <- as.data.frame(cbind(avgtemp, sdtemp$StandardDeviation))
colnames(tempsummary) <- c("Treatment", "Mean_Temp", "sd")

tempplot <- ggplot(tempsummary, aes(x = Treatment, y = Mean_Temp))+
  geom_point(data = tempregulate, aes(x = Treatment, y = Temp, col = "gray"), alpha= 0.5)+
  geom_errorbar(aes(ymin=Mean_Temp-sd, ymax=Mean_Temp+sd), width=.1)+
  geom_point()+
  scale_x_discrete("Treatment")+
  scale_y_continuous("Temperature (°C)", breaks=seq(14,33,2))+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
tempplot


#Temperature by mesocosm 
tempregulate <- na.omit(tempregulate)
tempregulate$Treatment <- as.factor(tempregulate$Treatment)

sdmes <- tempregulate %>% 
  group_by(Mesocosm, Treatment) %>%
  summarise_at(vars(Temp), list(StandardDeviation = sd))

avgmes <- tempregulate %>% 
  group_by(Mesocosm, Treatment) %>%
  summarise_at(vars(Temp), list(Mean_Temp = mean))


tempmes <- as.data.frame(cbind(avgmes, sdmes$StandardDeviation))
colnames(tempmes) <- c("Mesocosm","Treatment", "Mean_Temp", "sd")

tempmes$Mesocosm <- as.factor(tempmes$Mesocosm)

mesplot <- ggplot(tempmes, aes(x = Mesocosm, y = Mean_Temp, col = Treatment))+
  geom_errorbar(aes(ymin=Mean_Temp-sd, ymax=Mean_Temp+sd), width=.1)+
  geom_point()+
  facet_wrap(~ Treatment, scales ="free")+
  scale_x_discrete("Mesocosm")+
  scale_y_continuous("Temperature (°C)", limits = c(14,32), breaks=seq(14,32,2))+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
mesplot



#Temperature fluctuations throughout the day

sdtime <- tempregulate %>% 
  group_by(Mesocosm, Time, Treatment) %>%
  summarise_at(vars(Temp), list(StandardDeviation = sd))

avgtime <- tempregulate %>% 
  group_by(Mesocosm, Time, Treatment) %>%
  summarise_at(vars(Temp), list(Mean_Temp = mean))

temptime <- as.data.frame(cbind(avgtime, sdtime$StandardDeviation))
colnames(temptime) <- c("Mesocosm", "Time", "Treatment", "Mean_Temp", "sd")

temptime$Mesocosm <- as.factor(temptime$Mesocosm)
temptime$Treatment <- as.factor(temptime$Treatment)


#putting time in the correct order
unique(temptime$Time)
temptime$Time <- ordered(temptime$Time, levels = c("9:46", "9:47", "9:49", "9:50", "9:52", "10:00", "10:17", "10:55", "11:06", "11:17", "11:20", "11:30", "11:47", "12:00", "12:14", "12:17", "12:28","12:45","12:50",
                                                   "1:10","1:15", "2:17", "2:23", "2:28", "2:56", "3:19", "3:20", "3:32",
                                                   "4:25", "4:41", "4:49", "5:19"))

temptime <- subset(temptime, select = -c(5))
temptime <- na.omit(temptime)

timeplot <- ggplot(temptime, aes(x = Time, y = Mean_Temp, group = Mesocosm, color = Treatment))+
  #geom_errorbar(aes(ymin=Mean_Temp-sd, ymax=Mean_Temp+sd), width=.1)+
  geom_line()+
  geom_point()+
  geom_hline(yintercept=15.4, linetype="dashed", color = "red")+
  geom_hline(yintercept=25.5, linetype="dashed", color = "green")+
  geom_hline(yintercept=30.4, linetype="dashed", color = "blue")+
  scale_y_continuous("Temperature (°C)", breaks=seq(15,32,1))+
  scale_x_discrete("Time of Day")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))+
  theme(axis.text.x = element_text(size=rel(1)), axis.text.y = element_text(size=rel(1.5)),
        axis.title.x = element_text(size=rel(1)),axis.title.y = element_text(size=rel(1)), title = element_text(size=rel(2)))

timeplot






# Temp on Dispersal -------------------------------------------------------
dispersal <- glmer(Dispersal ~ Treatment + (1|Tank), data = phase2, family = binomial (link='logit'))
summary(dispersal)
drop1(dispersal, test = 'Chisq') #p = 0.4448

#checking model assumptions using the DHARMa package
library(DHARMa)
testOutliers(dispersal)
testDispersion(dispersal)

exp(0.01985) / (1 + exp(0.01985))


# predict survival
nd <- data.frame(unique(subset(phase2, select =c(Treatment))), "Tank"=NA)

mySumm_mod <- function(.){
  predict(., newdata=nd, re.form=~0)
}

boot <- bootMer(dispersal, mySumm_mod, nsim = 200, use.u=F, type="parametric")

sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

est_disp <- sumBoot(boot)

p_disp <- exp(est_disp) / (1 + exp(est_disp))

result_disp <- cbind(nd, p_disp);result_disp

dispersal_plot <- ggplot(result_disp, aes(x = Treatment, y = fit))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.5, col=NA)+
  scale_x_continuous("Temperature (°C)", limits = c(16,30), breaks = c(16,26,30))+
  scale_y_continuous("Dispersal Probability")+
  #labs(tag= 'A')+
  theme_bw(base_size=12)+ 
  theme(text = element_text(size = 18))+
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
dispersal_plot

#excluding dead backswimmers
dispersal <- glmer(Dispersal ~ Treatment + (1|Tank), data = subset(phase2, Survival == 1), family = binomial (link='logit'))
drop1(dispersal, test = 'Chisq') #p = 0.4448
summary(dispersal)

exp(0.06337) / (1 + exp(0.06337))

# Temp on Survival --------------------------------------------------------
#removing dispersers to only assess survival of those that stayed 
nodisp <- subset(phase2, Dispersal == 0)

survival <- glmer(Survival ~ Treatment + (1|Tank), data = nodisp, family = binomial (link='logit'))
drop1(survival, test = 'Chisq') #p = 0.0038, chi = 8.36
summary(survival)

exp(-0.15333) / (1 + exp(-0.15333))


ggplot(phase2, aes(x = Treatment, y = Survival)) + 
  geom_point()+
  geom_jitter(width = 0.25, height = 0.2)+
  stat_smooth(method = "glm", se = FALSE, method.args = list(family=binomial))

# predict survival
nd <- data.frame(unique(subset(nodisp, select =c(Treatment))), "Tank"=NA)

mySumm_mod <- function(.){
  predict(., newdata=nd, re.form=~0)
}

boot <- bootMer(survival, mySumm_mod, nsim = 200, use.u=F, type="parametric")

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


survival_plot <- ggplot(result_mort, aes(x = Treatment, y = fit))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.5, col=NA)+
  scale_x_continuous("Temperature (C)", limits = c(16,30), breaks = c(16,26,30))+
  scale_y_continuous("Survival Probability")+
  labs(tag = 'B')+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
survival_plot

ggarrange(dispersal_plot, survival_plot)

# Temp on Dispersal - Survival Analysis -----------------------------------
disp_sa <- glmer(DispFate ~ Day*Treatment + (1|Tank), data = p2sa, family = binomial(link='logit'))

drop1(disp_sa, test = 'Chisq')
summary(disp_sa)

#checking model assumptions using the DHARMa package
library(DHARMa)
testOutliers(disp_sa)
testDispersion(disp_sa)




# Temp on Survival - Survival Analysis ------------------------------------

death_sa <- glmer(SurvivalFate ~ Day + Treatment + (1|Tank), data = p2sa, family = binomial(link='logit'))

drop1(death_sa, test = 'Chisq')
summary(death_sa)

#checking model assumptions using the DHARMa package
library(DHARMa)
testOutliers(disp_sa)
testDispersion(disp_sa)

nd <- data.frame(unique(subset(p2sa, select =c(Treatment, Day))), "Tank"=NA)

mySumm_mod <- function(.){
  predict(., newdata=nd, re.form=~0)
}

boot <- bootMer(death_sa, mySumm_mod, nsim = 200, use.u=F, type="parametric")

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


survival_plot <- ggplot(result_mort, aes(x = as.factor(Treatment), y = fit))+
  geom_point(size=2)+
  #geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.5, col=NA)+
  #scale_x_continuous("Treatment", limits = c(16,30), breaks = c(16,26,30))+
  scale_y_continuous("Survival Probability")+
  theme_bw(base_size=12)+ 
  theme(panel.border = element_rect(color="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(colour = "black"))
survival_plot

ggplot(surv, aes(x = Treatment, y = Surv_Prob))+
  geom_point()
