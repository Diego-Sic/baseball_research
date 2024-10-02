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

directory_path <- "TrackmanData/Fall24"
all_files <- list.files(path = directory_path, pattern = ".csv$") # list names of all the files in the folder
TM_files <- all_files[!grepl("PP", all_files)]  # lists only TM files out of all
PP_files <- all_files[grepl("PP", all_files)]     # lists only Player Positioning(PP) files out of all


# this will merge same game's PP and TM file for all the games and put them in another folder with some modification
for (TM_file in TM_files) {
  TM_file_path <-  paste0("TrackmanData/Fall24/",TM_file)
  TM_data <- read.csv(TM_file_path)
  for(PP_file in PP_files)
  {
    PP_file_path <-  paste0("TrackmanData/Fall24/",PP_file)
    PP_data <- read.csv(PP_file_path)
    
    
    if(PP_data[1, "GameUID"]== TM_data[1,"GameUID"])
    {
      CCBData <- dplyr::inner_join(TM_data, PP_data, by = c("Date","Time","PitchNo","PitcherTeam","BatterTeam","GameUID","PitchUID","PlayID","PitchCall","PlayResult",
                                                            "LocalDateTime","UTCDate","UTCTime","UTCDateTime" ))
      CCBData <- data.table(CCBData)
      CCBData<- CCBData[ , Counter_1 := 1:.N , by = c("Date","Pitcher") ]
      
      #####
      
      
      CCBData <- CCBData %>%
        group_by(GameID, AwayTeam, HomeTeam) %>%
        mutate(date_str = sub("^(\\d{8}).*", "\\1", GameID),
               game_date = as.Date(date_str, format = "%Y%m%d"),
               formatted_date = format(game_date, "%m-%d-%y"),                  #This code creates a game variable for game input dropdown 
               GameNumber = sub("^\\d{8}-\\w+-(?:\\w+-)?(\\d+)$", "\\1", GameID),         #It also sorts the games chronologically and puts them in a format shown below 
               Games = paste(AwayTeam, "vs", HomeTeam, "on", formatted_date, ", Game", GameNumber)) %>%
        ungroup() %>%
        arrange(game_date)
      
      CCBData <- CCBData %>% filter(!is.na(TaggedPitchType) & TaggedPitchType != "Undefined") #Removes all undefined columns in the TaggedPitchType data 
      
      #CCBData$game_date <- as.Date(CCBData$Date)
      CCBData<-concat.split.multiple(data = CCBData, split.cols = c("Tilt"), seps = ":")
      CCBData$Count <- with(CCBData, ifelse(Balls==0 & Strikes==0, "0-0",
                                            ifelse(Balls==0 & Strikes==1, "0-1",
                                                   ifelse(Balls==0 & Strikes==2, "0-2",
                                                          ifelse(Balls==1 & Strikes==0, "1-0",
                                                                 ifelse(Balls==2 & Strikes==0, "2-0",
                                                                        ifelse(Balls==3 & Strikes==0, "3-0",
                                                                               ifelse(Balls==3 & Strikes==1, "3-1",
                                                                                      ifelse(Balls==3 & Strikes==2, "3-2",
                                                                                             ifelse(Balls==1 & Strikes==1, "1-1",
                                                                                                    ifelse(Balls==1 & Strikes==2, "1-2",
                                                                                                           ifelse(Balls==2 & Strikes==1, "2-1",
                                                                                                                  ifelse(Balls==2 & Strikes==2, "2-2","Ball"
                                                                                                                  )))))))))))))
      
      
      #Opponents <- subset(CCBData, PitcherTeam != "CEN_COL")
      #games <- unique(paste(Opponents$PitcherTeam, Opponents$Date, sep = ", ")) 
      #Teams <- paste(CCBData$PitcherTeam, CCBData$BatterTeam, sep = "-")
      #games <- unique(CCBData$Teams, CCBData$Date, sep = ", ") #This creates a games variable that is populated in the format (opponent team, date)
      #games <- paste(unique(CCBData$PitcherTeam), "vs", unique(CCBData$BatterTeam), unique(CCBData$Date), sep = " ") 
      #Creates a variable in a Centre vs Opponent Date format to use for an input 
      
      
      # Defines borders of Kzone, used in many charts
      topKzone <- 3.67
      botKzone <- 1.5
      inKzone <- -0.833
      outKzone <- 0.833
      
      # Defines borders of Kzone sub-boxes 1-9 and Lzone boxes L1-L4
      Kzone_9_x_vals <- c(inKzone, inKzone/3, outKzone/3, outKzone)
      Kzone_9_y_vals <- c(botKzone, (topKzone - botKzone)/3 + botKzone, topKzone - (topKzone-botKzone)/3, topKzone)
      Lzone_x_vals <-   c(-1.3, inKzone, 0, outKzone, 1.3)
      Lzone_y_vals <-   c(1.25, botKzone, botKzone + (topKzone - botKzone)/2, topKzone, 3.85)
      
      # These data frames are used to create the Kzone and Lzone boxes in their respective charts
      kZone <- data.frame(x = c(inKzone, inKzone, outKzone, outKzone, inKzone),
                          y = c(botKzone, topKzone, topKzone, botKzone, botKzone))
      kZone_9 <- data.frame(x = c(inKzone, outKzone, outKzone, inKzone, inKzone, Kzone_9_x_vals[2], Kzone_9_x_vals[2], Kzone_9_x_vals[3], Kzone_9_x_vals[3]),
                            y = c(Kzone_9_y_vals[2], Kzone_9_y_vals[2], Kzone_9_y_vals[3], Kzone_9_y_vals[3], topKzone, topKzone, botKzone, botKzone, topKzone))
      LZone_box <- data.frame(x = c(Lzone_x_vals[1], Lzone_x_vals[1], Lzone_x_vals[5], Lzone_x_vals[5], Lzone_x_vals[1]),
                              y = c(Lzone_y_vals[1], Lzone_y_vals[5], Lzone_y_vals[5], Lzone_y_vals[1], Lzone_y_vals[1]))
      
      Home_Plate <- data.frame(x = c(-0.7083, -0.7083, 0, 0.7083, 0.7083, -0.7083),
                               y = c(0, 0.25, 0.5, 0.25, 0, 0))
      
      PitchCall_Contact <- c("InPlay", "FoulBall")
      PitchCall_Swinging <- c("StrikeSwinging", "FoulBall", "InPlay")
      
      
      
      CCBData$Zone_1 <- with(CCBData, ifelse(PlateLocSide > inKzone           & PlateLocSide   < Kzone_9_x_vals[2] & 
                                               PlateLocHeight > Kzone_9_y_vals[3] & PlateLocHeight < topKzone, 1, 0))
      
      CCBData$Zone_2 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[2] & PlateLocSide   < Kzone_9_x_vals[3] & 
                                               PlateLocHeight > Kzone_9_y_vals[3] & PlateLocHeight < topKzone, 1, 0))
      
      CCBData$Zone_3 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[3] & PlateLocSide   < outKzone          & 
                                               PlateLocHeight > Kzone_9_y_vals[3] & PlateLocHeight < topKzone, 1, 0))
      
      CCBData$Zone_4 <- with(CCBData, ifelse(PlateLocSide > inKzone           & PlateLocSide   < Kzone_9_x_vals[2] & 
                                               PlateLocHeight > Kzone_9_y_vals[2] & PlateLocHeight < Kzone_9_y_vals[3], 1, 0))
      
      CCBData$Zone_5 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[2] & PlateLocSide   < Kzone_9_x_vals[3] & 
                                               PlateLocHeight > Kzone_9_y_vals[2] & PlateLocHeight < Kzone_9_y_vals[3], 1, 0))
      
      CCBData$Zone_6 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[3] & PlateLocSide   < outKzone          & 
                                               PlateLocHeight > Kzone_9_y_vals[2] & PlateLocHeight < Kzone_9_y_vals[3], 1, 0))
      
      CCBData$Zone_7 <- with(CCBData, ifelse(PlateLocSide > inKzone           & PlateLocSide   < Kzone_9_x_vals[2] & 
                                               PlateLocHeight > botKzone          & PlateLocHeight < Kzone_9_y_vals[2], 1, 0))
      
      CCBData$Zone_8 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[2] & PlateLocSide   < Kzone_9_x_vals[3] & 
                                               PlateLocHeight > botKzone          & PlateLocHeight < Kzone_9_y_vals[2], 1, 0))
      
      CCBData$Zone_9 <- with(CCBData, ifelse(PlateLocSide > Kzone_9_x_vals[3] & PlateLocSide   < outKzone          & 
                                               PlateLocHeight > botKzone          & PlateLocHeight < Kzone_9_y_vals[2], 1, 0))
      
      CCBData$Zone_L1 <- with(CCBData, ifelse((PlateLocSide > Lzone_x_vals[1] & PlateLocSide   < inKzone & 
                                                 PlateLocHeight > Lzone_y_vals[3] & PlateLocHeight < Lzone_y_vals[5])|
                                                (PlateLocSide > Lzone_x_vals[1] & PlateLocSide   < Lzone_x_vals[3] & 
                                                   PlateLocHeight > topKzone        & PlateLocHeight < Lzone_y_vals[5]), 1, 0))
      
      CCBData$Zone_L2 <- with(CCBData, ifelse((PlateLocSide > outKzone        & PlateLocSide   < Lzone_x_vals[5] & 
                                                 PlateLocHeight > Lzone_y_vals[3] & PlateLocHeight < Lzone_y_vals[5])|
                                                (PlateLocSide > Lzone_x_vals[3] & PlateLocSide   < Lzone_x_vals[5] & 
                                                   PlateLocHeight > topKzone        & PlateLocHeight < Lzone_y_vals[5]), 1, 0))
      
      CCBData$Zone_L3 <- with(CCBData, ifelse((PlateLocSide > Lzone_x_vals[1] & PlateLocSide   < inKzone & 
                                                 PlateLocHeight < Lzone_y_vals[3] & PlateLocHeight > Lzone_y_vals[1])|
                                                (PlateLocSide < Lzone_x_vals[3] & PlateLocSide   > Lzone_x_vals[1] & 
                                                   PlateLocHeight > Lzone_y_vals[1] & PlateLocHeight < botKzone), 1, 0))
      
      CCBData$Zone_L4 <- with(CCBData, ifelse((PlateLocSide > outKzone        & PlateLocSide   < Lzone_x_vals[5] & 
                                                 PlateLocHeight < Lzone_y_vals[3] & PlateLocHeight > Lzone_y_vals[1])|
                                                (PlateLocSide > Lzone_x_vals[3] & PlateLocSide   < Lzone_x_vals[5] & 
                                                   PlateLocHeight > Lzone_y_vals[1] & PlateLocHeight < botKzone), 1, 0))
      
      CCBData$Zone_Name <- with(CCBData, ifelse(Zone_1 == 1, "1", 
                                                ifelse(Zone_2 == 1, "2", 
                                                       ifelse(Zone_3 == 1, "3", 
                                                              ifelse(Zone_4 == 1, "4", 
                                                                     ifelse(Zone_5 == 1, "5", 
                                                                            ifelse(Zone_6 == 1, "6", 
                                                                                   ifelse(Zone_7 == 1, "7", 
                                                                                          ifelse(Zone_8 == 1, "8", 
                                                                                                 ifelse(Zone_9 == 1, "9",
                                                                                                        ifelse(Zone_L1 == 1, "L1", 
                                                                                                               ifelse(Zone_L2 == 1, "L2", 
                                                                                                                      ifelse(Zone_L3 == 1, "L3", 
                                                                                                                             ifelse(Zone_L4 == 1, "L4", "NA"))))))))))))))
      
      CCBData$AB <- with(CCBData, ifelse((PlayResult == "Out")|
                                           (PlayResult == "Single")|(PlayResult == "Double")|(PlayResult == "Triple")|
                                           (PlayResult =="HomeRun")|(PlayResult == "Error")|(KorBB == "Strikeout"), 1, 0))
      
      CCBData$SLG_VAL <- with(CCBData, ifelse(PlayResult == "Single", 1,
                                              ifelse(PlayResult == "Double", 2,
                                                     ifelse(PlayResult == "Triple", 3,
                                                            ifelse(PlayResult == "HomeRun", 4, 0))))) 
      
      CCBData$Out_Zone_NotSwinging <- with(CCBData, ifelse(PitchCall %in% c("StrikeCalled", "BallCalled") & (PlateLocSide > outKzone | PlateLocSide < inKzone 
                                                                                                             | PlateLocHeight > topKzone | PlateLocHeight < botKzone), 1, 0))
      
      CCBData$In_Zone_NotSwinging <- with(CCBData, ifelse(PitchCall %in% c("StrikeCalled", "BallCalled") & PlateLocSide < outKzone & PlateLocSide > inKzone 
                                                          & PlateLocHeight < topKzone & PlateLocHeight > botKzone, 1, 0))
      
      CCBData$Out_Zone_Swinging <- with(CCBData, ifelse(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay") & 
                                                          (PlateLocSide > outKzone | PlateLocSide < inKzone | PlateLocHeight > topKzone | PlateLocHeight < botKzone), 1, 0))
      
      CCBData$In_Zone_Swinging <- with(CCBData, ifelse(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay") &
                                                         PlateLocSide < outKzone & PlateLocSide > inKzone & PlateLocHeight < topKzone & PlateLocHeight > botKzone, 1, 0))
      
      # This is not what we want....
      #CCBData$Strike_Awarded <- with(CCBData, ifelse(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), 1, 
      #                                            ifelse(PitchCall == "FoulBall" & Strikes != 2, 1, 0)))
      
      CCBData$Strike_Awarded <- with(CCBData, ifelse(PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall", "InPlay"), 1, 0))
      
      CCBData$SacrificeFly <- with(CCBData, ifelse(CCBData$TaggedHitType == "FlyBall" & PlayResult == "Out" & CCBData$RunsScored > 0, 1, 0))
      
      
      ## this will calculate the cordinates of where the ball landed but for the groundballs it is and estimate of where it was feilded
      ## stored in columns hc_x and hc_y
      CCBData$hc_x <- with(CCBData,ifelse(TaggedHitType=="GroundBall",sin(Bearing*pi/180)*ExitSpeed*1.5 ,sin(Bearing*pi/180)*Distance))
      
      CCBData$hc_y <- with(CCBData,ifelse(TaggedHitType=="GroundBall",cos(Bearing*pi/180)*ExitSpeed*1.5,cos(Bearing*pi/180)*Distance))
      
      CCBData$location_x <- with(CCBData, 2.5*(hc_x - 125.42))
      CCBData$location_y <- with(CCBData, 2.5*(198.27 - hc_y))
      # adds a column 'season'
      CCBData$season <- "fall24"
      
      # defining the boundries for buffer zone in strikezone
      topBufZone <- 3.85
      botBufZone <- 1.25
      inBufZone <- -1.3
      outBufZone <- 1.3
      
      
      ## adding a column that will tell us if the ball actually went in strikezone or not
      CCBData <- mutate(CCBData,
                        strikeZone = ifelse(is.na(PlateLocSide), NA,  # Check for NA in "PlateLocSide"
                                            ifelse(PlateLocSide >= inKzone & PlateLocSide <= outKzone &
                                                     PlateLocHeight >= botKzone & PlateLocHeight <= topKzone
                                                   , "in", ifelse(PlateLocSide >= inBufZone & PlateLocSide <= outBufZone &
                                                                    PlateLocHeight >= botBufZone & PlateLocHeight <= topBufZone,"buffer","out")))  # Check value and assign "out" or "in"
      )
      
      
      
      
      # relabel all one-seam fastballs and two-seam fastballs as "fastballs"
      CCBData <- CCBData %>% mutate(TaggedPitchType = recode(TaggedPitchType, 'OneSeamFastBall' = 'Fastball', 'TwoSeamFastBall' = 'Fastball'))
      
      ## naming the folder
      gameNo <- 1
      file_name <- paste0(CCBData[2,"Date"],".",CCBData[1,"AwayTeam"], ".Game ",as.character(gameNo),".csv")
      
      
      while(any(grepl(substr(file_name,11,nchar(file_name)), list.files(path = "TrackmanData/Spring24/Clean", pattern = ".csv$"))))
      { 
        gameNo <- gameNo+1
        file_name <- paste0(substr(file_name, 1, nchar(file_name) - 5),as.character(gameNo),".csv")
      }
      ## storing the file in clean folder
      write.csv(CCBData, file = paste0("TrackmanData/Spring24/Clean/",file_name))
      break
    }
    
  }
  
}


