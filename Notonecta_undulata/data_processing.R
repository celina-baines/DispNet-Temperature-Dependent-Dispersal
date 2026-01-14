# phase 1

d_phase1 <- read.csv("phase1_fitness_assay.csv", header = TRUE)

# normality of fitness proxy

with(d_phase1, hist(Total_eggs))
with(d_phase1, hist(log(Total_eggs+1)))
nrow(subset(d_phase1, Total_eggs == 0))

with(d_phase1, table(Death_Day, Treatment))

nrow(subset(d_phase1, Death_Day != 0 & Death_Day != 1 & Total_eggs == 0))


with(subset(d_phase1, Death_Day != 0 & Death_Day != 1), hist(log(Total_eggs+1)))

with(d_phase1, boxplot(Total_eggs ~ Treatment))
with(subset(d_phase1, Death_Day != 0 & Death_Day != 1 & Treatment == 16 | Treatment == 26 | Treatment == 30), boxplot(Total_eggs ~ Treatment))
with(subset(d_phase1, Death_Day != 0 & Death_Day != 1 & Treatment == 16 | Treatment == 26 | Treatment == 30), boxplot(log(Total_eggs+1) ~ Treatment))

# NB: For backswimmer data, remove individuals that died on day 0 or 1. These individuals are distributed evenly across temperature treatments, and can be assumed to be caused by shock/handling rather than treatment.
# additionally, they represent a lot of the zeros in the dataset that make it difficult to transform to make the data close to normal.

# tidy the dataset

d_phase1 <- subset(d_phase1, select = c(Treatment, Block, Mesocosm, Total_eggs, Survivorship), Treatment == 16 | Treatment == 26 | Treatment == 30 & Death_Day != 0 & Death_Day != 1)

d_phase1$Temp.treatment <- with(d_phase1, ifelse(Treatment == 30, "high", ifelse(Treatment == 26, "opt", "low")))

with(d_phase1, table(Temp.treatment, Treatment))

d_phase1$Mesocosm <- with(d_phase1, paste(Block, Mesocosm, sep = "."))

d_phase1$Temp.value <- d_phase1$Treatment
d_phase1$Fitness <- d_phase1$Total_eggs

d_phase1 <- subset(d_phase1, select = c(Temp.treatment, Temp.value, Block, Mesocosm, Fitness))


#####################################
# phase 2

d_phase2 <- read.csv("phase_2_dispersal.csv", header = TRUE)
head(d_phase2)
table(d_phase2$Survival)
table(d_phase2$Treatment)

# tidy the dataset
d_phase2 <- subset(d_phase2, select = c(Treatment, Tank, Dispersal, Survival))

d_phase2$Temp.treatment <- with(d_phase2, ifelse(Treatment == 30, "high", ifelse(Treatment == 26, "opt", "low")))

with(d_phase2, table(Temp.treatment, Treatment))

d_phase2$Mesocosm <- d_phase2$Tank

d_phase2$Temp.value <- d_phase2$Treatment

d_phase2$Dispersed <- d_phase2$Dispersal

d_phase2$Survived <- with(d_phase2, ifelse(Survival == 1, "Y", "N"))

d_phase2 <- subset(d_phase2, select = c(Temp.treatment, Temp.value, Mesocosm, Dispersed, Survived))

##########################

# write datasets
setwd("C:/Users/Celina Baines/Dropbox/Ongoing Research/DispNet/DispNet-Temperature-Dependent-Dispersal/Notonecta_undulata")
write.csv(d_phase1, "Phase1_fitness_Notonecta.undulata.csv", row.names = F)

write.csv(d_phase2, "Phase2_dispersal_Notonecta.undulata.csv")

