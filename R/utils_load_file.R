load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ","),
         tsv = vroom::vroom(path, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}
