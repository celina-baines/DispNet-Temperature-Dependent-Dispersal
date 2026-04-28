Genus = "Callosobruchus" #Insert Genus here in quotes
species = "maculatus" #Insert species here in quotes
sex = "male"

rmarkdown::render(
  input = paste(Genus, "_", species, "/", Genus, "_", species, "_", sex, ".Rmd", sep = ""),
  #input = paste(Genus, "_", species, ".Rmd", sep = ""),
  output_dir = "Knitted_Markdowns",
  output_file = paste(Genus, "_", species, "/", Genus, "_", species, "_", sex,".docx", sep = "")
)

