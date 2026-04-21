# DispNet temperature dependent dispersal

# Additional analyses

# NOTES
## Tetranychus evansi still has contr = "control"; need to fix that
## still need to do 2) below - how changing degrees C changes dispersal

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


# 2) overall effect of temp on dispersal: how much does dispersal change per degree C? Overall analysis and/or separate hot and cold again
  