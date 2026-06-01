Genus = "Aphanomyces" 
species = "euteiches" 

rmarkdown::render(
  input = paste(Genus, "_", species, "/", Genus, "_", species, "_multiple_strains.Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", species, "/", Genus, "_", species, "_multiple_strains.docx", sep = "")
)

