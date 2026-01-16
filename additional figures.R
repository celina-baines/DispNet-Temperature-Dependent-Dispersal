d_opt <- data.frame("species" = c(na.omit(unique(d_g_fitness$species))), es = 0, Temp.contr = "OO")

d_tpc <- bind_rows(d_g_fitness, d_opt)

ggplot(subset(d_tpc, Temp.contr != "OMH" & Temp.contr != "OML" & !is.na(species)), aes(x = Temp.contr, y = -es, group = species, col = species))+
  geom_point(size = 2)+
  geom_line()+
  #geom_errorbar(aes(ymax = -es + se_fitness, ymin = -es - se_fitness), width = 0.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_y_continuous("Relative fitness")+
  scale_x_discrete("Temperature treatment", limits = c("OL", "OO", "OH"), labels = c("cool", "opt", "hot"))+
  theme_classic(base_size = 20)


ggplot(subset(d_tpc, species == "Gryllus.campestris"), aes(x = Temp.contr, y = -es, group = species, col = species))+
  geom_point(size = 2)+
  geom_line()+
  #geom_errorbar(aes(ymax = -es + se_fitness, ymin = -es - se_fitness), width = 0.1)+
  geom_hline(yintercept = 0, lty = 2)+
  scale_y_continuous("Relative fitness")+
  scale_x_discrete("Temperature treatment", limits = c("OL", "OML", "OO"), labels = c("cool", "mediumcool", "opt"))+
  theme_classic(base_size = 20)
