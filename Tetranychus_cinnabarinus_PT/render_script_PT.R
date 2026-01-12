Genus = "Tetranychus" #Insert Genus here in quotes
species = "cinnabarinus_PT" #Insert species here in quotes


rmarkdown::render(
  input = paste(Genus, "_", species, "/", Genus, "_", species, ".Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", "cinnabarinus", "/", Genus, "_", species, ".docx", sep = "")
)
