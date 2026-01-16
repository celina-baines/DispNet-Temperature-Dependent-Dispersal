library(boot)
library(lme4)

## list summary datasets
dispersal_chisquare_list <- list.files(path = "./raw_data", pattern = "Phase2_dispersal_")

mySumm <- function(.) {
  return(drop1(., test = "Chisq")$LRT[2])
}


d_formulae <- read.csv("./outputs/fitness.dispersal.formulas.csv", header = T)
# Trichogramma has an observation level random effect but it isn't 

## estimate chi-square with st.error
d_chi <- NULL # dataframe of chi square values
nR = 10 #number of simulations for bootstrapping

for(file in dispersal_chisquare_list){
  # get species name and dispersal formula
  name <- gsub(".csv", "", file)
  sp <- str_sub(name, 18)
  d_sub <- subset(d_formulae, species == sp)
  dispersal.formula <- d_sub$dispersal.formula[1]

  # read data frame
  d <- read.csv(paste("./raw_data/", file, sep = ""), header = T) #read all files in list
  
  # mixed effects model
  if(d_sub$dispersal.fixed.mixed[1] == "mixed"){
  m_boot <- glmer(dispersal.formula, family = "binomial", data = d)
  boot_results <- bootMer(m_boot, mySumm, nsim = nR, use.u = F, type = "parametric")
  d_chi <- rbind(d_chi, data.frame("species" = sp, "chisq" = mean(boot_results$t), "se" = sd(boot_results$t)))
  
  }
  # fixed effects model
  if(d_sub$dispersal.fixed.mixed[1] == "fixed"){
    glm.fun <- function(dat, inds) {
      # Fit a logistic regression model using the resampled data subset
      fit <- glm(dispersal.formula, family = "binomial", data = dat[inds,])
      # Return the coefficients
      return(drop1(fit, test = "Chisq")$LRT[2])
    }
    boot_results <- boot(data = d, statistic = glm.fun, R = nR)
    d_chi <- rbind(d_chi, data.frame("species" = sp, "chisq" = mean(boot_results$t), "se" = sd(boot_results$t)))
    
  }  
}

write.csv(d_chi, "outputs/overall_dispersal_chisq_boot.csv", row.names = F)
