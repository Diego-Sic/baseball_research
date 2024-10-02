
library(DT)
require(data.table)
library(stringr)
library(tidyr)
library(grid)
library(gridExtra)
library(mosaic)
library(shinydashboard)
library(tidyverse)
library(fs)
library(readr)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(ggplot2)
library(sportyR)
library(plotly)
library(mgcv)
library(splitstackshape)
library(scales)
library(htmlwidgets)
library(baseballr)
library(ggforce)
library(shinythemes)



# sample data for max.temp vector
max.temp <- c(Sun = 22, Mon = 27, Tue = 26, Wen = 24, Thu = 23, Fri = 26, Sat = 28)

# create a new plotting window and set the plotting area into a 1*2 array
par(mfrow = c(1, 2))

# plot a bar chart for max.temp
barplot(max.temp, main = "Barplot", names.arg = names(max.temp))

# plot a pie chart for max.temp
pie(max.temp, main = "Piechart", radius = 1, labels = names(max.temp))


###
library(dplyr)  # Assuming you have dplyr loaded

# Define a function to merge TM and PP files within a directory
merge_tm_pp_files <- function(dir_path) {
  # Get TM and PP file paths
  tm_files <- list.files(dir_path, pattern = ".csv$")[grepl("TM", .)]
  pp_files <- list.files(dir_path, pattern = ".csv$")[grepl("PP", .)]
  
  # Check if TM and PP files exist
  if (length(tm_files) != length(pp_files)) {
    stop("Unequal number of TM and PP files found!")
  }
  
  # Loop through corresponding TM and PP files
  merged_data <- lapply(seq_along(tm_files), function(i) {
    tm_file <- tm_files[i]
    pp_file <- pp_files[i]
    tm_data <- read.csv(paste0(dir_path, "/", tm_file))
    pp_data <- read.csv(paste0(dir_path, "/", pp_file))
    
    # Merge TM and PP data (assuming they have a common identifier)
    merged <- dplyr::inner_join(tm_data, pp_data, by = "common_id")  # Replace "common_id" with the actual column for joining
    return(merged)
  })
  
  # Combine merged data from all files
  do.call(rbind, merged_data)
}

# Example usage: assuming "TrackmanData/Spring24/Games" is your directory
merged_tm_pp_data <- merge_tm_pp_files("TrackmanData/Spring24/Games")








###

CCBData_TM <- fs::dir_ls("TrackmanData/Spring24/Games") %>%
  keep(~ !grepl("playerpositioning", .)) %>% #Uses purrr and grepl to not read the files containing string "playerpositioning"
  map_df(read_csv)

CCBData_TM <- data.table(CCBData_TM)
CCBData_TM<- CCBData_TM[ , Counter_1 := 1:.N , by = c("Date") ]
CCBData_TM <- data.frame(CCBData_TM)
print(names(CCBData_TM))


CCBData_PP <- fs::dir_ls("TrackmanData/Spring24/Games") %>%
  keep(~ grepl("playerpositioning", .)) %>% #Uses purrr and grepl to not read the files containing string "playerpositioning"
  map_df(read_csv)

CCBData_PP <- data.table(CCBData_PP)
#CCBData_PP<- CCBData_PP[ , Counter_1 := 1:.N , by = c("Date") ]
CCBData_PP <- data.frame(CCBData_PP)
print(names(CCBData_PP))

##
CCBData <- dplyr::inner_join(CCBData_TM, CCBData_PP, by = c("Date"))
CCBData <- CCBData[,Counter_1 := 1:.N ,by = c("Date" , "Pitcher")]
