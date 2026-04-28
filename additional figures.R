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
p_quad <- data.frame(a = 0.005, m = 2, Tmin = 10, Tmax = 35, Topt = 29.25, Thigh = 32)

# Generate a range of temperatures
temperatures <- seq(5, 45, by = 0.25)

# Simulate performance values
performance <- sapply(temperatures, quadratic_tpc,
                      a = p_quad$a, m = p_quad$m, Tmin = p_quad$Tmin, Tmax = p_quad$Tmax)

# Create a data frame
simulated_data <- data.frame(temp = temperatures, performance = performance)

ggplot(simulated_data, aes(x = temp, y = performance)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Temperature (°C)",
       y = "Performance") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 8))+
  geom_vline(aes(xintercept = 29.25), lty = 2, col = "blue", linewidth = 1.2)+
  geom_vline(aes(xintercept = 31.9), lty = 2, col = "red", linewidth = 1.2)+
  geom_vline(aes(xintercept = 35), lty = 2, col = "darkred", linewidth = 1.2)+
  theme_classic(base_size = 20)



# dispersal data


dispersal_func <- function(temp) {
  results <- ifelse(temp < p_quad$Thigh, 0.1*temp - 2.725,
                           -0.2*temp + 6.85)
  return(results)
}

disp_frame <- sapply(temperatures, dispersal_func);disp_frame

disp_d <- data.frame(temp = temperatures, disp = disp_frame)
disp_d <- rbind(disp_d, data.frame(temp = 34.2, disp = 0))

ggplot(disp_d, aes(x = temp, y = disp)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Temperature (°C)",
       y = "Dispersal probability") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0, 1))+
  geom_vline(aes(xintercept = 29.25), lty = 2, col = "blue", linewidth = 1.2)+
  geom_vline(aes(xintercept = 31.9), lty = 2, col = "red", linewidth = 1.2)+
  geom_vline(aes(xintercept = 35), lty = 2, col = "darkred", linewidth = 1.2)+
  theme_classic(base_size = 20)








# plot dispersal on TPC
data <- left_join(simulated_data, disp_d)

ggplot(data, aes(x=temp)) +
  geom_line( aes(y=performance), linewidth = 1.5) + 
  geom_line( aes(y=disp*20), col = "purple", linewidth = 1.5) +
  scale_x_continuous("Temperature", limits = c(10, 35))+
  scale_y_continuous(
    # Features of the first axis
    name = "Performance",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~. /20, name="Dispersal probability"), limits = c(-0.1, 10), expand = c(0,0)
  )+
  geom_vline(aes(xintercept = 29.25), lty = 2, col = "blue")+
  geom_vline(aes(xintercept = 31.9), lty = 2, col = "red")+
  theme(base_size = 30, axis.text = element_blank(), axis.title = element_text(size = 15), axis.title.y.right = element_text(color = "purple"), panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "black"))

####
# forest plots including species with low dispersal

m_dispersalOH <- rma(yi = OR, sei = se, data = d_dispmeta_OH)
m_dispersalOL <- rma(yi = OR, sei = se, data = d_dispmeta_OL)

forest(m_dispersalOH, slab = d_OR_dispersal_OH$study, addcred = TRUE, main = "Dispersal: Opt vs Hot")
forest(m_dispersalOL, slab = d_OR_dispersal_OL$study, addcred = TRUE, main = "Dispersal: Opt vs Cool")
