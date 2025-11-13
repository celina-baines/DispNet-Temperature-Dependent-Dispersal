Genus = "Tetranychus" #Insert Genus here in quotes
species = "cinnabarinus" #Insert species here in quotes
setwd("C:/Users/svwunnik/OneDrive - UGent/UGent-PC/Svwunnik/Desktop/Dokter Siebe/De papers van ikke/4_Anna_Gaelle_paper/DispNet-Temperature-Dependent-Dispersal/Tetranychus_cinnabarinus")

rmarkdown::render(
  input = paste(Genus, "_", species, ".Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", species, ".docx", sep = "")
)

