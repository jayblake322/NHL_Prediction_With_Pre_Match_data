
download_html_data <- function(website) {

  # This function takes a website containing an html table, downloads the data and returns it 
  # only designed for a simple website 
  # further processing of the dataframe is needed for users specific use
  # has library dependencies
  
  library(xml2)
  library(rvest)
  library(dplyr)

  webpage <- read_html(website)

  table <- html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  
  return(table)

}



