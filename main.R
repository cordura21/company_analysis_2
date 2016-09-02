
# Load data

# Function that gets and melts file data ----------------------------------


get_data <- function(company) {
  
  library(yaml)
  library(readxl)
  library(dplyr)
  my.data <- readxl::read_excel(file.path('data',paste0(company,'.xlsx')))
  names(my.data)[1] <- 'variable'
  
  
  names(my.data) <- as.character(my.data[26,])
  my.data <- my.data[27:NROW(my.data),]
  my.data['NA'] <- NULL
  names(my.data)[1] <- 'variable'
  my.data <- my.data[,1:which(names(my.data) == 'TTM/current ')]
  library(tidyr)
  x <- gather(data = my.data,key = date, value = value, -variable, na.rm = TRUE) %>% unique()
  x$value <- round(as.numeric(x$value),1)
  return(x)
}


get_table <- function(x,table.name, melt = FALSE, reverse = TRUE){
  require(dplyr)
  x <- x %>% filter(variable %in% table.name)
  if(melt == FALSE){
    require(tidyr)
    x <- x %>% spread(key = date,value = value)
  }
  if(reverse){
    x <- x[,c(1,NCOL(x),2:NCOL(x),1)]
  }
  return(x)
}

curr_comp <- 'some Company'
params <- yaml::yaml.load_file(file.path('companies', paste0(curr_comp,'.yml')))

company.data <- get_data(curr_comp)

company.data %>% spread(date,value)

get_table(company.data,params$charts$summary,melt = FALSE) %>% View()
