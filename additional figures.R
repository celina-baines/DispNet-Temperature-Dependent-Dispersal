library(ggplot2)
d_opt <- data.frame("study" = c(na.omit(unique(d_g_fitness$study))), es = 0, Temp.contr = "OO")

d_tpc <- bind_rows(d_g_fitness, d_opt)

ggplot(subset(d_tpc, Temp.contr != "OMH" & Temp.contr != "OML" & !is.na(study)), aes(x = Temp.contr, y = -es, group = study, col = study))+
  geom_point(size = 2)+
  geom_line()+
  #geom_errorbar(aes(ymax = -es + se_fitness, ymin = -es - se_fitness), width = 0.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_y_continuous("Relative fitness")+
  scale_x_discrete("Temperature treatment", limits = c("OL", "OO", "OH"), labels = c("cool", "opt", "hot"))+
  theme_classic(base_size = 20)


ggplot(subset(d_tpc, species == "Folsomia.candida"), aes(x = Temp.contr, y = -es, group = species, col = species))+
  geom_point(size = 2)+
  geom_line()+
  #geom_errorbar(aes(ymax = -es + se_fitness, ymin = -es - se_fitness), width = 0.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_y_continuous("Relative fitness")+
  scale_x_discrete("Temperature treatment", limits = c("OL", "OML", "OO"), labels = c("low", "opt", "high"))+
  theme_classic(base_size = 20)


###########
# dispersal on TPC figure

# Define the quadratic TPC function manually for simulation
quadratic_tpc <- function(temp, Tmin, Tmax, a, m) {
  P = a * temp * (temp - Tmin) * ((Tmax - temp)^(1/m))
}
# Define parameters (example for a hypothetical organism)
p_quad <- data.frame(a = 0.005, m = 2, Tmin = 10, Tmax = 35, Topt = 25, Thigh = 30)

# Generate a range of temperatures
temperatures <- seq(5, 45, by = 0.5)

# Simulate performance values
performance <- sapply(temperatures, quadratic_tpc,
                      a = p_quad$a, m = p_quad$m, Tmin = p_quad$Tmin, Tmax = p_quad$Tmax)

# Create a data frame
simulated_data <- data.frame(temp = temperatures, performance = performance)

# dispersal data

dispersal_func <- function(temp, Topt, Thigh) {
  if (temp < Topt) return(0.2)
  if (temp > Topt & temp < Thigh) return(0.2*temp +0.2)
  return(-0.2*temp + 1)
}

dispersal_func <- function(temp) {
  results <- ifelse(temp >= 30, 0, 0.5)
  return(results)
}

disp_frame <- sapply(temperatures, dispersal_func);disp_frame

disp_d <- data.frame(temp = temperatures, disp = disp.prob)

ggplot(simulated_data, aes(x = temp, y = performance)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Performance") +
  theme_classic()
