load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ","),
         tsv = vroom::vroom(path, delim = "\t"),
         jison = jisonreader::read_jison_file(path),  # Suponiendo que "jisonreader" es la biblioteca adecuada
         validate("Invalid file; Please upload a .csv, .tsv, or .jison file")
  )
}
