Genus = "Notonecta"
species = "undulata"

rmarkdown::render(
  input = paste(Genus, "_", species, "/", Genus, "_", species, ".Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", species, "/", Genus, "_", species, ".docx", sep = "")
)

