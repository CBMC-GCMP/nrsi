#1. This function assigns standard column names to the original input

standardize.db <- function(database,
                           old=c("Reef",
                           "Depth",
                           "Transect",
                           "Species",
                         "Abundance",
                         "Size")
){
  library(data.table)

  
  db <- setnames(database, old = old, 
           new = c("reef",
                   "depth",
                   "transect",
                   "species",
                   "abundance",
                   "size"))

}



