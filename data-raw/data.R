ISD_documentation <- read.csv("data-raw/ISD_documentation.csv", 
                              stringsAsFactors = FALSE)
for(i in c("MIN", "MAX", "MISSING")){
  ISD_documentation[[i]] <- sub("c", "", ISD_documentation[[i]])
}
ISD_lookup <- read.csv("data-raw/ISD_lookup.csv", stringsAsFactors = FALSE)

devtools::use_data(ISD_documentation, ISD_lookup, overwrite = TRUE)
devtools::use_data(ISD_documentation, ISD_lookup, internal = TRUE, overwrite = TRUE)
