Genus = "Armadillidium" #Insert Genus here in quotes
species = "vulgare" #Insert species here in quotes

rmarkdown::render(
  input = paste(Genus, "_", species, "/", Genus, "_", species, ".Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", species, "/", Genus, "_", species, ".docx", sep = "")
)

