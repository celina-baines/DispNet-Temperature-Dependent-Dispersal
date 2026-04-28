# DispNet temperature dependent dispersal

# Additional analyses

# NOTES
## Tetranychus evansi still has contr = "control"; need to fix that

# 1) Check for influence of model structure: Same meta-analysis but with all species using same simplest model and/or fit one GLMM in same model - have the data and can do this myself

# Load libraries
library(tidyverse)
library(lme4)
library(ggplot2)
library(emmeans)
library(DHARMa)

# load dataset with sample sizes
d_samplesize <- read.csv(here::here("./outputs/Sample_sizes.csv"))


# Model formulae

## Formula for dispersal: 

formulae <- read.csv(here::here("./outputs/fitness.dispersal.formulas.csv"))


# Analysis of all dispersal datasets with same formula/model structure

## Dealing with mortality in dispersal assay

filter.dead = TRUE/FALSE # choose FALSE if this does not apply to your species


## load datasets and run model

d_modoutput <- NULL

for(i in c(1:nrow(formulae))){
  Genus <- formulae$Genus[i]
  species_st <- formulae$species_st[i]
  # load dataset
  d_phase2 <- read.csv(here::here(paste("./raw_data/Phase2_dispersal_", Genus, ".", species_st, ".csv", sep = "")))
  d_phase2 <- within(d_phase2, Temp.treatment <- relevel(as.factor(Temp.treatment), ref = "opt"))
  d_mods <- read.table(here::here(paste("./outputs/moderators_", Genus, ".", species_st, ".txt", sep = "")))
  # filter out dead individuals where appropriate
  #d_phase2 <- d_phase2 %>%
  #  { if (filter.dead) filter(., Survived == "Y") else . }
  
  # set model formula: all the same model structure but slightly different syntax
  dispersal.formula <- formulae$dispersal.formula.simple[i]
  
  # run model
  m_dispersal <- glm(dispersal.formula, family = "binomial", data = d_phase2)
  
  # save output object
  output <- summary(m_dispersal)
  
  # add to dataframe
  d_modoutput <- rbind(d_modoutput, 
                       data.frame("study" = formulae$study[i], 
                                  "Genus" = Genus, 
                                  "species" = formulae$species[i], 
                                  "Taxonomic.group" = d_mods$Taxonomic.group[1],
                                  "treatment" = rownames(output$coefficients)[2], 
                                  "OR" = output$coefficients[rownames(output$coefficients)[2], "Estimate"],
                                  "se" = output$coefficients[rownames(output$coefficients)[2], "Std. Error"],
                                  "totalN" = subset(d_samplesize, study == formulae$study[i])$Dispersal.totalN[1]),
                       data.frame("study" = formulae$study[i], 
                                  "Genus" = Genus, 
                                  "species" = formulae$species[i], 
                                  "Taxonomic.group" = d_mods$Taxonomic.group[1], 
                                  "treatment" = rownames(output$coefficients)[3], 
                                  "OR" = output$coefficients[rownames(output$coefficients)[3], "Estimate"],
                                  "se" = output$coefficients[rownames(output$coefficients)[3], "Std. Error"],
                                  "totalN" = subset(d_samplesize, study == formulae$study[i])$Dispersal.totalN[1])
                       )
  
}

# create new column (contr) to make treatment more readable
d_modoutput$contr <- str_remove(d_modoutput$treatment, ".*Temp.treatment")
d_modoutput$V <- with(d_modoutput, I(se^2))

# meta-analytical model

## opt vs hot
m_OH_meta <- rma.mv(yi = OR, V = V, random = list(~ 1 | study, ~1|Taxonomic.group/species), data = subset(d_modoutput, contr == "high" | contr == "medium.high"))

summary(m_OH_meta)
anova(m_OH_meta)

## opt vs cold
m_OL_meta <- rma.mv(yi = OR, V = V, random = list(~ 1 | study, ~1|Taxonomic.group/species), data = subset(d_modoutput, contr == "low" | contr == "medium.low"))

summary(m_OL_meta)
anova(m_OL_meta)


# need to compare to similar model in main analysis - effects of temperature on dispersal

# 1b) Including all species in same GLMM - dispersal ~ temperature
## note: could do the same for fitness possibly but it makes much less sense because the fitness metrics are so different

dispersal_rawdata_list <- list.files(path = "./raw_data", pattern = "Phase2_dispersal_")

d_phase2_allspp <- NULL
for(file in dispersal_rawdata_list){
  # load files
  
  df <- read.csv(paste("./raw_data/", file, sep = ""))
  name <- gsub(".csv", "", file)
  study <- str_sub(name, 18)
  
  Genus <- sub("\\..*", "", study)
  d_mods <- read.table(here::here(paste("./outputs/moderators_", study, ".txt", sep = "")))
  
  # convert dispersed -> num.disp, num.nondisp
  if (subset(formulae, study == str_sub(name, 18))$dispersal.formula.simple == "Dispersed ~ Temp.treatment") {
    df$Num.disp <- df$Dispersed
    df$total.N <- 1
  }
  
  if (!("Num.nondisp" %in% names(df))) {
    df$Num.nondisp <- df$total.N - df$Num.disp
  }
  
  df <- subset(df, select = c(Temp.treatment, Temp.value, Num.disp, Num.nondisp))
  df$Taxonomic.group <- d_mods$Taxonomic.group[1]
  df$species <- sub("_.*", "", study)
  df$study <- study
  
  # add to full dataset
  d_phase2_allspp <- bind_rows(d_phase2_allspp, df)
}

d_phase2_allspp <- within(d_phase2_allspp, Temp.treatment <- relevel(as.factor(Temp.treatment), ref = "opt"))
d_phase2_allspp$Temp.treatment[d_phase2_allspp$Temp.treatment == "control"] <- "low"

m_trt <- glmer(cbind(Num.disp, Num.nondisp) ~ Temp.treatment + (1|study) + (1|Taxonomic.group/species), family = binomial, data = d_phase2_allspp)
summary(m_trt)
drop1(m_trt, test = "Chisq")

predictedy <- as.data.frame(emmeans(m_trt, specs = ~ Temp.treatment, type = "response"))

ggplot(predictedy, aes(x = Temp.treatment, y = prob))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL))+
  scale_x_discrete(limits = c("low", "medium.low", "opt", "medium.high", "high"))+
  theme_classic()


# 2) overall effect of temp on dispersal: how much does dispersal change per degree C? Overall analysis and/or separate hot and cold again
# as a function of degree C
m_tempC <- glmer(cbind(Num.disp, Num.nondisp) ~ Temp.value + (1|study) + (1|Taxonomic.group/species), family = binomial, data = d_phase2_allspp)
summary(m_tempC)
drop1(m_trt, test = "Chisq")

ggplot(d_phase2_allspp, aes(x = Temp.value, y = Num.disp/(Num.disp + Num.nondisp)))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.8)+
  theme_classic()

# as a function of diff from opt
opt_temp <- unique(subset(d_phase2_allspp, Temp.treatment == "opt", select = c(study, Temp.value)))
opt_temp <- opt_temp %>% 
  rename(optC = Temp.value)

d_phase2_allspp <- left_join(d_phase2_allspp, opt_temp, relationship = "many-to-many")
d_phase2_allspp$Tdiff <- with(d_phase2_allspp, Temp.value - optC)

m_tempTdiff <- glmer(cbind(Num.disp, Num.nondisp) ~ poly(Tdiff, 2) + (1|study) + (1|Taxonomic.group/species), family = binomial, data = d_phase2_allspp)
summary(m_tempTdiff)
drop1(m_tempTdiff, test = "Chisq")

predictedy_Tdiff <- as.data.frame(emmeans(m_tempTdiff, specs = ~ Tdiff, at = list(Tdiff = seq(min(d_phase2_allspp$Tdiff), max(d_phase2_allspp$Tdiff), 0.1)), type = "response"))

ggplot(d_phase2_allspp, aes(x = Tdiff, y = Num.disp/(Num.disp + Num.nondisp)))+
  geom_point()+
  geom_smooth(method = "loess", span = 0.8)+
  geom_line(data = predictedy_Tdiff, aes(x = Tdiff, y = prob))+
  theme_classic()

