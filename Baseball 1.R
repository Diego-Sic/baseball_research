
# This application was built by Robbie Harper and Kevin Sivakumar and was further developed by Davis Jones

# Last update: February 12

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

#update.packages("ggplot2")

#CCBData <- read.csv("TrackmanFinal_1.csv")

# CCBData <-  fs::dir_ls("Games") %>%   #This is the code that lets you set file directory; make sure the folder name
#   lapply(read_csv) %>%                 #is in quotes and is contained in the same larger folder that contains the rfile 
#  bind_rows                             #e.g: BaseballTrackman (parent folder) --> (BaseballFeb22.R (script) + Practice (game folder))
#print(fs::dir_ls(".."))

#CCBData <- fs::dir_ls("OneDrive_1_6-5-2023/TrackmanData/Spring23/Games") %>%
CCBData <- fs::dir_ls("TrackmanData/Spring24/Games") %>%
  keep(~ !grepl("playerpositioning", .)) %>% #Uses purrr and grepl to not read the files containing string "playerpositioning"
  map_df(read_csv)

dt <- data.table(CCBData)
dt<- dt[ , Counter_1 := 1:.N , by = c("Date" , "Pitcher") ]
CCBData <- data.frame(dt)

CCBData <- CCBData %>%
  group_by(GameID, AwayTeam, HomeTeam) %>%
  mutate(date_str = sub("^(\\d{8}).*", "\\1", GameID),
         game_date = as.Date(date_str, format = "%Y%m%d"),
         formatted_date = format(game_date, "%m-%d-%y"),                  #This code creates a game variable for game input dropdown 
         GameNumber = sub("^\\d{8}-\\w+-(\\d+)$", "\\1", GameID),         #It also sorts the games chronologically and puts them in a format shown below 
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

CCBData$hc_x <- with(CCBData, sin(Bearing*pi/180)*Distance)

CCBData$hc_y <- with(CCBData, cos(Bearing*pi/180)*Distance)

CCBData$location_x <- with(CCBData, 2.5*(hc_x - 125.42))
CCBData$location_y <- with(CCBData, 2.5*(198.27 - hc_y))

# relabel all one-seam fastballs and two-seam fastballs as "fastballs"
CCBData <- CCBData %>% mutate(TaggedPitchType = recode(TaggedPitchType, 'OneSeamFastBall' = 'Fastball', 'TwoSeamFastBall' = 'Fastball'))


# To see a subset of data
# Luc_Morgan <- filter(CCBData, Pitcher == "Morgan, Luc", Distance != "NA") %>% 
#    subset(select = c(Distance, Bearing, hc_x, hc_y, PitchCall, TaggedHitType, PlayResult))
# view(Luc_Morgan)

image_ratio <- 248/327
image_width <- 45


ui = {navbarPage(
  img(src = "CentreCTransparent.png", alt = "Cannot find image", width = image_width, height = image_width*image_ratio), 
  theme = shinytheme("sandstone"),
  tabPanel("Pitcher Charts",
           sidebarLayout(
             sidebarPanel(width = 3,
                          fluidRow(
                            column(6, offset = 0, align = "center",
                                   pickerInput(
                                     inputId = "SeasonInput", label = "Select Season",
                                     choices = NULL, selected = NULL,
                                     options = list(`actions-box` = T), multiple = T
                                   )),
                            # The choices for these inputs are created within the server
                            column(6, offset = 0, align = "center",
                                   pickerInput(
                                     inputId = "PitcherTeamInput", label = "Select Team",
                                     choices = c(sort(unique(CCBData$PitcherTeam))), selected = "CEN_COL",
                                     options = list(`actions-box` = T), multiple = T
                                   ))
                          ),
                          selectizeInput(
                            inputId = "PitcherInput", label = "Select Pitcher",
                            choices = NULL, selected = NULL
                          ),
                          pickerInput(
                            inputId = "PitcherGameInput", label = "Select Game",
                            multiple = T, choices = NULL, selected = NULL,
                            options = list(`actions-box` = T)
                          ),
                          fluidRow(column(6, offset = 0, align = "center",
                                          pickerInput(
                                            inputId = "PitcherCountInput", label = "Select Count",
                                            multiple = T, choices = c(unique(CCBData$Count)), selected = c(unique(CCBData$Count)),
                                            options = list(`actions-box` = T)
                                          )),
                                   column(6, offset = 0, align = "center",
                                          pickerInput(
                                            inputId = "PitcherPitchType", label = "Select Pitch Type",
                                            choices = NULL, selected = NULL,
                                            options = list(`actions-box` = T), multiple = T
                                          ))
                          ),
                          fluidRow(column(6, offset = 0, align = "center",
                                          switchInput(inputId = "BattersideSwitch", label = "Compare L/R", size = "small")),
                                   column(6, offset = 0, align = "center",
                                          switchInput(inputId = "PitcherUndefinedSwitch", label = "Remove 'Undefined'", 
                                                      size = "small", value = F))
                          ),
                          # Adds Centre Baseball logo to sidebar
                          # Instructions: make sure image file is PNG/PDF/JPEG in a file called 'www' in same directory as the application
                          #               and make sure 'src = "NameOfFile.png"'
                          img(src = "logoTransparent.png", alt = "Cannot find image", width = "100%", height = "100%")
             ),
             mainPanel(
               navbarPage("Pitcher Charts", id = "PitcherCharts",
                          navbarMenu("Other Charts", 
                                     tabPanel("Pitching Stats Table",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Pitching Stats Table"), hr(style="border-color: black;"),
                                                              dataTableOutput("PitcherTable"))
                                              )),
                                     tabPanel("PV Trend",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("PV Trend"), hr(style="border-color: black;"),
                                                              plotOutput("PVTrendlinesPlot"))
                                              )),
                                     tabPanel("Release Point",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Release Point"), hr(style="border-color: black;"),
                                                              plotOutput("ReleasePointPlot"))
                                              )),
                                     tabPanel("Velocity by Pitch Count",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Velocity by Pitch Count"), hr(style="border-color: black;"),
                                                              plotOutput("VelocityPitchCountPlot"))
                                              )),
                                     tabPanel("Vert/Horz Break: Scatter Plot",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Vert/Horz Break: Scatter Plot"), hr(style="border-color: black;"),
                                                              plotOutput("VertHorzBreakScatterPlot"))
                                              )),
                                     tabPanel("Spray Chart 1",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Spray Chart 1"), hr(style="border-color: black;"),
                                                              plotOutput("SprayChartPlot1"))
                                              ),
                                              fluidRow(
                                                column(4, offset = 0.5,
                                                       selectInput(inputId = "SprayChartRange", label = "Show:",
                                                                   choices = c("Infield + Outfield", "Infield Only"), 
                                                                   selected = "Infield + Outfield")),
                                                column(4, offset = 0.5,
                                                       selectInput(inputId = "SprayChartArcs", label = "Distance Arcs:",
                                                                   choices = c("None", "Every 100ft", "Every 50ft"), selected = "Every 100ft")),
                                                column(4, offset = 0.5,
                                                       selectInput(inputId = "SprayChartColor", label = "Color Code By:",
                                                                   choices = c("Pitch Type", "Play Result"), selected = "Hit Type"))
                                              )),
                                     tabPanel("Spray Chart 2",
                                              fluidRow(column(10,offset = 0, align = "center",
                                                              h3("Spray Chart 2"), hr(style="border-color: black;"),
                                                              plotlyOutput("SprayChartPlot2")),
                                                       column(2, offset = 0, align = "center",
                                                              h3("Pitch Kzone"), hr(style="border-color: black;"),
                                                              plotOutput("PitchKzone",height = 250, width = 250))
                                              ),
                                              fluidRow(
                                                column(4, offset = 0.5,
                                                       selectInput(inputId = "SprayChartRange2", label = "Show:",
                                                                   choices = c("Infield + Outfield", "Infield Only", "Zoom Home Plate"), 
                                                                   selected = "Infield + Outfield"))
                                              )),
                          ),
                          navbarMenu("Kzone Charts",
                                     tabPanel("Pitch Locations",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("Pitch Locations"), hr(style="border-color: black;"),
                                                              plotOutput("PitchLocationPlot"))
                                              )),
                                     tabPanel("KZone/OZone Pitch Result",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("KZone/OZone Pitch Result"), hr(style="border-color: black;"),
                                                              plotOutput("PitchCallPlot"))
                                              )),
                                     tabPanel("Effective Velocity",
                                              fluidRow(
                                                column(12, offset = 0, align = "center", 
                                                       h3("Effective Velocity"), hr(style="border-color: black;"),
                                                       fluidRow(
                                                         column(6, offset = 0, align = "left",
                                                                plotOutput("EffectiveVelocityPlot1")),
                                                         column(6, offset = 0, align = "right",
                                                                plotOutput("EffectiveVelocityPlot2")),
                                                         # Creates the option to compare two games on the same screen
                                                         fluidRow(
                                                           column(3, offset = 0.5, align = "left",
                                                                  selectInput(inputId = "EVComparison", label = "Compare Two Games?",
                                                                              choices = c("No", "Yes"), selected = "No")),
                                                           column(4, offset = 0.25,
                                                                  selectizeInput(
                                                                    inputId = "ConditionalPitcherGameInput1", label = "Select Game 1",
                                                                    multiple = F, choices = unique(CCBData$Games), selected = NULL
                                                                  )),
                                                           column(4, offset = 0.25,
                                                                  selectizeInput(
                                                                    inputId = "ConditionalPitcherGameInput2", label = "Select Game 2",
                                                                    multiple = F, choices = unique(CCBData$Games), selected = NULL
                                                                  ))
                                                         )
                                                       )))),
                                     tabPanel("KZone/OZone Outcome",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("KZone/OZone Outcome"), hr(style="border-color: black;"),
                                                              plotOutput("PlayResultPlot")),
                                              )),
                                     tabPanel("StrikeZone Heatmap",
                                              fluidRow(column(12, offset = 0, align = "center",
                                                              h3("StrikeZone Heatmap"), hr(style="border-color: black;"),
                                                              plotOutput("StrikezoneHeatmapPlot"))
                                              )),
                                     tabPanel("Pitcher Grids",
                                              fluidRow(
                                                column(12, offset = 0, align = "center", 
                                                       h3("Pitcher Grids"), hr(style="border-color: black;"),
                                                       fluidRow(
                                                         column(6, offset = 0, align = "left",
                                                                plotOutput("PitcherGridsPlot1")),
                                                         column(6, offset = 0, align = "right",
                                                                plotOutput("PitcherGridsPlot2"))
                                                       ))
                                              ))
                          )
               ))
           )),
  tabPanel("Batter Charts",
           sidebarLayout(
             sidebarPanel(width = 3,
                          pickerInput(
                            inputId = "BatterTeamInput", label = "Select Team",
                            choices = c(sort(unique(CCBData$BatterTeam))), selected = "CEN_COL",
                            options = list(`actions-box` = T), multiple = T
                          ),
                          selectizeInput(
                            inputId = "BatterInput", label = "Select Batter",
                            choices = NULL, selected = NULL
                          ),
                          pickerInput(
                            inputId = "BatterGameInput", label = "Select Game",
                            multiple = T, choices = NULL, selected = NULL,
                            options = list(`actions-box` = T)
                          ),
                          pickerInput(
                            inputId = "BatterCountInput", label = "Select Count",
                            multiple = T, choices = c(unique(CCBData$Count)), selected = c(unique(CCBData$Count)),
                            options = list(`actions-box` = T)
                          ),
                          pickerInput(
                            inputId = "BatterPitchType", label = "Select Pitch Type",
                            choices = NULL, selected = NULL,
                            options = list(`actions-box` = T), multiple = T
                          ),
                          fluidRow(column(6, offset = 0, align = "center",
                                          switchInput(inputId = "PitcherThrowsSwitch", label = "Compare L/R", size = "small")),
                                   column(6, offset = 0, align = "center",
                                          switchInput(inputId = "BatterUndefinedSwitch", label = "Remove 'Undefined'", 
                                                      size = "small", value = F))
                          ),
                          img(src = "logoTransparent.png", alt = "Cannot find image", width = "100%", height = "100%")
             ),
             mainPanel(
               navbarPage("Batter Charts",
                          navbarMenu("Select Chart",
                                     tabPanel("Batter Statistics Table",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("Batter Statistics Table"), hr(style="border-color: black;"),
                                                       dataTableOutput("BatterTable"))
                                              )),
                                     tabPanel("Batter Contact: Result",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("Batter Contact: Result"), hr(style="border-color: black;"),
                                                       plotOutput("BatterContactResultPlot"))
                                              )),
                                     tabPanel("Batter: Exit Speed",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("Batter: Exit Speed"), hr(style="border-color: black;"),
                                                       plotOutput("BatterExitSpeedPlot"))
                                              )),
                                     tabPanel("SLG Grids",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("SLG Grids"), hr(style="border-color: black;"),
                                                       plotOutput("SLGGridsPlot1"),
                                                       plotOutput("SLGGridsPlot2"))
                                              )),
                                     tabPanel("Average Exit Velocity Grids",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("Average Exit Velocity Grids"), hr(style="border-color: black;"),
                                                       plotOutput("AvgExitVeloPlot1"),
                                                       plotOutput("AvgExitVeloPlot2"))
                                              )),
                                     tabPanel("Hitter vs L/R Pitcher",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       h3("Hitter vs L/R Pitcher"), hr(style="border-color: black;"),
                                                       plotOutput("BatterHandedPitcherPlot"))
                                              ))
                          )))
           )),
  tabPanel("Umpire Charts",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h3("Select Info"),
                          pickerInput(
                            inputId = "UmpireTeamInput", label = "Select Team",
                            choices = c(sort(unique(CCBData$BatterTeam))), selected = "CEN_COL",
                            options = list(`actions-box` = T), multiple = T
                          ),
                          pickerInput(
                            inputId = "UmpireGameInput", label = "Select Game",
                            multiple = T, choices = unique(CCBData$Games), selected = unique(CCBData$Games[1]),
                            options = list(`actions-box` = T)
                          ),
             ),
             mainPanel(
               navbarPage("Umpire Charts",
                          navbarMenu("Select Chart",
                                     tabPanel("Separate Team",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       plotOutput("SeparateTeamPlot"))
                                              )),
                                     tabPanel("All Teams",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       plotOutput("AllTeamsPlot"))
                                              )),
                                     tabPanel("Heatmaps",
                                              fluidRow(
                                                column(12, offset = 0, align = "center",
                                                       plotOutput("UmpireHeatmapPlot"))
                                              ))
                          )))
           ))
)}



server <- function(input, output, session) {
  
  # PITCHER DATA
  # Creates reactive data table filtered by team input
  Pitcher_Team_Input_Data <- reactive({
    input$PitcherTeamInput
    Filtered_Data <- CCBData[CCBData$PitcherTeam %in% input$PitcherTeamInput, ]
  })
  # Updates Pitcher Input choices based on team input
  observeEvent(input$PitcherTeamInput, {
    pitcher_choices <- sort(unique(Pitcher_Team_Input_Data()$Pitcher))
    updateSelectizeInput(session, "PitcherInput", 
                         choices = pitcher_choices, selected = pitcher_choices[1]
    )
  })
  # Creates reactive data table  filtered by pitcher input
  Pitcher_Input_Data <- reactive({
    input$PitcherInput
    Filtered_Data <- Pitcher_Team_Input_Data()[Pitcher_Team_Input_Data()$Pitcher == input$PitcherInput, ]
  })
  # Updates Pitcher Game Input and Pitcher Pitch Type choices based on pitcher input
  observeEvent(input$PitcherInput, {
    pitcher_games <- sort(unique(Pitcher_Input_Data()$Games))
    updatePickerInput(session, "PitcherGameInput", 
                      choices = pitcher_games, selected = pitcher_games
    )
    updatePickerInput(session, "PitcherPitchType",
                      choices = unique(Pitcher_Input_Data()$TaggedPitchType),
                      selected = unique(Pitcher_Input_Data()$TaggedPitchType)
    )
    updateSelectizeInput(session, "ConditionalPitcherGameInput1",
                         choices = pitcher_games, selected = pitcher_games[1]
    )
    updateSelectizeInput(session, "ConditionalPitcherGameInput2",
                         choices = pitcher_games, selected = pitcher_games[2]
    )
  })
  
  # PITCHER DATA FILTER: reacts based on Game, Count, and Pitch Type inputs
  Pitcher_Data <- reactive({
    list(input$PitcherGameInput, input$PitcherCountInput, input$PitcherPitchType)
    Filtered_Data_table <- Pitcher_Input_Data() %>%
      filter(Games %in% input$PitcherGameInput,
             Count %in% input$PitcherCountInput,
             TaggedPitchType %in% input$PitcherPitchType
      )
    if(input$PitcherUndefinedSwitch == T) {
      Filtered_Data_table <- Filtered_Data_table %>% filter(BatterSide != "Undefined")
    }
    Filtered_Data_table
  })
  
  
  # BATTER DATA
  # Creates reactive data table filtered by team input
  Batter_Team_Input_Data <- reactive({
    input$BatterTeamInput
    Filtered_Data <- CCBData[CCBData$BatterTeam %in% input$BatterTeamInput, ]
  })
  # Updates Batter Input choices based on team input
  observeEvent(input$BatterTeamInput, {
    batter_choices <- sort(unique(Batter_Team_Input_Data()$Batter))
    updateSelectizeInput(session, "BatterInput", 
                         choices = batter_choices, selected = batter_choices[1]
    )
  })
  # Creates reactive data table  filtered by batter input
  Batter_Input_Data <- reactive({
    input$BatterInput
    Filtered_Data <- Batter_Team_Input_Data()[Batter_Team_Input_Data()$Batter == input$BatterInput, ]
  })
  # Updates Batter Game Input and Batter Pitch Type choices based on batter input
  observeEvent(input$BatterInput, {
    batter_games <- sort(unique(Batter_Input_Data()$Games))
    updatePickerInput(session, "BatterGameInput", 
                      choices = batter_games, selected = batter_games
    )
    updatePickerInput(session, "BatterPitchType",
                      choices = unique(Batter_Input_Data()$TaggedPitchType),
                      selected = unique(Batter_Input_Data()$TaggedPitchType)
    )
  })
  
  # BATTER DATA FILTER: reacts based on Game, Count, and Pitch Type inputs
  Batter_Data <- reactive({
    list(input$BatterGameInput, input$BatterCountInput, input$BatterPitchType)
    Filtered_Data_table <- Batter_Input_Data() %>%
      filter(Games %in% input$BatterGameInput,
             Count %in% input$BatterCountInput,
             TaggedPitchType %in% input$BatterPitchType)
    if(input$BatterUndefinedSwitch == T) {
      Filtered_Data_table <- Filtered_Data_table %>% filter(PitcherThrows != "Undefined")
    }
    Filtered_Data_table
  })
  
  
  # Disables the L/R switch for charts that it does not apply to
  observeEvent(input$PitcherCharts, {
    if(input$PitcherCharts %in% 
       c("Pitching Stats Table", "Pitch Locations", "KZone/OZone Pitch Result", "Effective Velocity", 
         "KZone/OZone Outcome", "Pitcher vs L/R Handed Batter", "StrikeZone Heatmap", "Pitcher Grids")) {
      updateSwitchInput(session, inputId = "BattersideSwitch", disabled = F)
      updateSwitchInput(session, inputId = "PitcherUndefinedSwitch", disabled = F)
    }
    else {
      updateSwitchInput(session, inputId = "BattersideSwitch", disabled = T)
      updateSwitchInput(session, inputId = "PitcherUndefinedSwitch", disabled = T, value = F)}
  })
  
  # Function that sets up Kzone charts
  Kzone_Chart_Setup <- function(plotname, pitch_or_bat) {
    # This function draws the kzone box and home plate, sets the x and y ranges, creates and styles 
    # the title and axes, and controls L/R comparison facet wrapping for all kzone charts except grids
    
    plotname <- plotname +
      geom_path(data = kZone, mapping = aes(x, y), col = "black", lwd = 1.5) +
      geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1) +
      coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) + xlab("") + ylab("") + 
      theme(plot.title = element_text(face = "bold", size = 24), strip.text = element_text(size = 10))
    
    if(pitch_or_bat == "pitch") {
      plotname <- plotname +
        {if(input$BattersideSwitch == T) facet_wrap(~BatterSide)} +
        labs(title = paste("Pitcher:", input$PitcherInput))
      
    } else if(pitch_or_bat == "bat") {
      plotname <- plotname +
        {if(input$PitcherThrowsSwitch == T) facet_wrap(~PitcherThrows)} +
        labs(title = paste("Batter:", input$BatterInput))
      
    } else {}
  }
  
  # Grids Functions
  ({
    # These functions are used in the setup for Grids charts
    
    # Function that draws the Kzone grid and the Lzone boxes 
    Grids_Border_Fnct <- function(plotname) {
      plotname <- plotname +
        geom_path(data = kZone, mapping = aes(x, y),  col = "black") +
        geom_path(data = kZone_9, mapping = aes(x, y),  col = "black") +
        geom_path(data = LZone_box, mapping = aes(x, y),  col = "black") +
        geom_segment(aes(x = inKzone,         y = Lzone_y_vals[3], xend = Lzone_x_vals[1], yend = Lzone_y_vals[3]),  color="black") +
        geom_segment(aes(x = outKzone,        y = Lzone_y_vals[3], xend = Lzone_x_vals[5], yend = Lzone_y_vals[3]),  color="black") +
        geom_segment(aes(x = Lzone_x_vals[3], y = botKzone,        xend = Lzone_x_vals[3], yend = Lzone_y_vals[1]),  color="black") +
        geom_segment(aes(x = Lzone_x_vals[3], y = topKzone,        xend = Lzone_x_vals[3], yend = Lzone_y_vals[5]),  color="black")
    }
    
    # Function that creates a vector of zone sums
    Grids_Sum_Fnct <- function(data) {
      Sum_Zones_Vec <- c(sum(data$Zone_1, na.rm = T), sum(data$Zone_2, na.rm = T), sum(data$Zone_3, na.rm = T), sum(data$Zone_4, na.rm = T), 
                         sum(data$Zone_5, na.rm = T), sum(data$Zone_6, na.rm = T), sum(data$Zone_7, na.rm = T), sum(data$Zone_8, na.rm = T), 
                         sum(data$Zone_9, na.rm = T), sum(data$Zone_L1, na.rm = T), sum(data$Zone_L2, na.rm = T), sum(data$Zone_L3, na.rm = T),
                         sum(data$Zone_L4, na.rm = T))
    }
    
    # Function that assigns either the color red or blue to each zone 
    Grids_Color_Fnct <- function(zone_sum_vector, sum_total) {
      Colors_Vec <- c()
      for(i in 1:13) {
        if(zone_sum_vector[i]/sum_total <= ((1 - (zone_sum_vector[i]/sum_total))/12)) {
          Colors_Vec <- c(Colors_Vec, "blue")}
        else{Colors_Vec <- c(Colors_Vec, "red")}
      }
      Colors_Vec
    }
    
    # Function that creates zone text
    Grids_Text_Fnct <- function(plotname, text_vec) {
      text_x_vec <- c(inKzone*2/3, 0, outKzone*2/3, -1.05, 1.05)
      text_y_vec <- c((topKzone-botKzone)*5/6+botKzone, (topKzone-botKzone)/2+botKzone, (topKzone-botKzone)*1/6+botKzone, 3.675, 1.425)  
      Grids_Text_Data_Frame <- data.frame(x = c(rep(c(text_x_vec[1], text_x_vec[2], text_x_vec[3]), times = 3), rep(c(text_x_vec[4], text_x_vec[5]), times = 2)),
                                          y = c(rep(c(text_y_vec[1], text_y_vec[2], text_y_vec[3]), each = 3), rep(c(text_y_vec[4], text_y_vec[5]), each = 2)),
                                          z = text_vec
      )
      plotname <- plotname +
        geom_label(data = Grids_Text_Data_Frame, mapping = aes(x,y, label = z))
    }
    
    # Function that creates zone color fill
    Grids_ColorFill_Fnct <- function(plotname, Alpha_Vec, zone_color_vector) {
      
      # These create a table of values for the Kzone for loop to refer to
      Color_Table_K <- data.table(x = zone_color_vector[1:3], y = zone_color_vector[4:6], z = zone_color_vector[7:9]) %>% t()
      Alpha_Table_K <- data.table(x = Alpha_Vec[1:3], y = Alpha_Vec[4:6], z = Alpha_Vec[7:9]) %>% t()
      
      # Color fill in Kzone (zones 1-9)
      for(i in 1:3) {
        for(j in 1:3) {
          plotname <- plotname +
            annotate("rect", xmin = Kzone_9_x_vals[j], xmax = Kzone_9_x_vals[j+1],
                     ymin = Kzone_9_y_vals[4-i], ymax = Kzone_9_y_vals[5-i],
                     alpha = Alpha_Table_K[i, j], fill = Color_Table_K[i, j] )
        }}
      # Color fill in L1 and L3
      for(i in 1:4) {
        plotname <- plotname +
          annotate("rect", xmin = Lzone_x_vals[1], xmax = Lzone_x_vals[ifelse(i == 1, 3, ifelse(i == 2, 2, i-1))], 
                   ymin = Lzone_y_vals[i], ymax = Lzone_y_vals[i+1],
                   alpha = Alpha_Vec[ifelse(i <= 2, 12, 10)], fill = zone_color_vector[ifelse(i <= 2, 12, 10)] )
      }
      # Color fill in L2 and L4
      for(i in 1:4) {
        plotname <- plotname +
          annotate("rect", xmin = Lzone_x_vals[ifelse(i == 3, 4, ifelse(i == 4, 3, i+2))], xmax = Lzone_x_vals[5], 
                   ymin = Lzone_y_vals[i], ymax = Lzone_y_vals[i+1],
                   alpha = Alpha_Vec[ifelse(i <= 2, 13, 11)], fill = zone_color_vector[ifelse(i <= 2, 13, 11)] )
      }
      plotname
    }
  })
  
  # Function that sets the height of Kzone plots
  Plot_Height <- reactive({
    list(input$PitcherCharts, input$BatterCharts, input$UmpireCharts)
    if(input$BattersideSwitch == F) {
      HeightOfPlot <- 600
    } else if(input$BattersideSwitch == T & input$PitcherUndefinedSwitch == F) {
      HeightOfPlot <- 400
    } else if(input$BattersideSwitch == T & input$PitcherUndefinedSwitch == T) {
      HeightOfPlot <- 550
    }
    HeightOfPlot
  })
  # Function that sets the width of Kzone plots
  Plot_Width <- reactive({
    list(input$PitcherCharts, input$BatterCharts, input$UmpireCharts)
    if(input$BattersideSwitch == F) {
      WidthOfPlot <- 800
    } else if(input$BattersideSwitch == T & input$PitcherUndefinedSwitch == F) {
      WidthOfPlot <- 1000
    } else if(input$BattersideSwitch == T & input$PitcherUndefinedSwitch == T) {
      WidthOfPlot <- 800
    }
    WidthOfPlot
  })
  
  
  
  # *** CHARTS ***
  
  # ** PITCHING CHARTS **
  
  # * NON-KZONE PITCHING CHARTS *
  
  # PITCHING STATISTICS TABLE: displays variety of pitching statistics summarized by pitch type. Data table. Landing spot.
  output$PitcherTable <- renderDataTable({
    filtered_table <- Pitcher_Data()
    
    # Creates two new data tables filtered by batter side
    filtered_table_left <- filter(Pitcher_Data(), BatterSide == "Left")
    filtered_table_right <- filter(Pitcher_Data(), BatterSide == "Right")
    
    # Creates a function that runs the calculations for the different statistics
    pitch_stats_function <- function(data) {
      data %>% summarize(
        'No.' = n(),
        'Usage %' = (n() / nrow(data)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'Max Velo' = max(RelSpeed, na.rm = T) %>% 
          round(1) %>% format(nsmall = 1),
        'Avg Velo' = mean(RelSpeed, na.rm = T) %>% 
          round(1) %>% format(nsmall = 1),
        'Avg Spin' = mean(SpinRate, na.rm = T) %>% 
          round(0) %>% format(nsmall = 0),
        'Slug%' = (sum(SLG_VAL, na.rm = T) / sum(AB, na.rm = T)) %>% 
          round(3) %>% format(nsmall = 3),
        '90% Exit Velo' = quantile(ExitSpeed, probs = 0.9, na.rm = T) %>% 
          round(1) %>% format(nsmall = 1),
        'Strike%' = (sum(Strike_Awarded) / n()*100) %>% 
          round(1) %>% format(nsmall = 1),
        'Whiff%' = (sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% PitchCall_Swinging)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'Contact%' = (sum(PitchCall %in% PitchCall_Contact, na.rm = T) / sum(PitchCall %in% PitchCall_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'OSwing%' = ((sum(Out_Zone_Swinging, na.rm = T) / sum(Out_Zone_NotSwinging + Out_Zone_Swinging, na.rm = T))*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing%' = ((sum(In_Zone_Swinging, na.rm = T) / sum(In_Zone_NotSwinging + In_Zone_Swinging, na.rm = T))*100) %>% 
          round(1) %>% format(nsmall = 1),
        'OSwing Whiff%' = (sum(Out_Zone_Swinging == 1 & PitchCall == "StrikeSwinging", na.rm = T) / sum(Out_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'OSwing Contact%' = (sum(Out_Zone_Swinging == 1 & PitchCall %in% c("FoulBall", "InPlay"), na.rm = T) / sum(Out_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing Whiff%' = (sum(In_Zone_Swinging == 1 & PitchCall == "StrikeSwinging", na.rm = T) / sum(In_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing Contact%' = (sum(In_Zone_Swinging == 1 & PitchCall %in% c("FoulBall", "InPlay"), na.rm = T) / sum(In_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1)
      )
    }
    
    table <- filtered_table %>% group_by('Pitch' = TaggedPitchType) %>% pitch_stats_function()
    totals_table <- filtered_table %>% pitch_stats_function()
    
    table <- rbind(table, totals_table %>% mutate(Pitch = "TOTALS"))
    
    # Creates statistics tables filtered by baterside
    table_left <- filtered_table_left %>% group_by('Pitch' = TaggedPitchType) %>% pitch_stats_function()
    table_right <- filtered_table_right %>% group_by('Pitch' = TaggedPitchType) %>% pitch_stats_function()
    
    # Modifies the "Pitch" column to indicate batter side
    table_left$Pitch <- paste(table_left$Pitch, "(L)")
    table_right$Pitch <- paste(table_right$Pitch, "(R)")
    
    # Creates a row that shows the totals for the left and right batter side stats  
    totals_table_left <- filtered_table_left %>% pitch_stats_function()
    totals_table_right <- filtered_table_right %>% pitch_stats_function()
    
    
    # Creates a pitch statistics table that separates L and R batter side statistics
    if(input$BattersideSwitch == T) {
      # Creates a table without a totals row if only one pitch type is selected
      if(nrow(table_left) <= 1) {
        table <- rbind(table_left, table_right)
        rownames(table) <- table$Pitch
      } else {
        table <- rbind(table_left, totals_table_left %>% mutate(Pitch = "TOTALS (L)"), table_right, totals_table_right %>% mutate(Pitch = "TOTALS (R)"))
        rownames(table) <- table$Pitch
      }
    }
    
    Pitch_Stats_Table <- datatable(table[2:ncol(table)], extensions = "FixedColumns", rownames = table$Pitch,
                                   options = list(searching = F, paging = F, info = F, scrollX = T, fixedColumns = list(leftColumns = 1)
                                   ),
                                   # changes the datatable title
                                   caption = htmltools::tags$caption(
                                     htmltools::tags$span("Pitcher:    ", style="color:orange; text-align: left; font-size:200%; font-weight:bold"), 
                                     htmltools::tags$span(input$PitcherInput, style="color:black; text-align: left; font-size:200%; font-weight:bold")
                                   ))
    
    {if(input$BattersideSwitch == F) {
      # Highlights and bolds the TOTALS row
      Pitch_Stats_Table <- Pitch_Stats_Table %>% 
        formatStyle(0, target = "row",
                    backgroundColor = styleEqual("TOTALS", 'aliceblue'),
                    fontWeight = styleEqual("TOTALS", "bold")) 
    } else if(nrow(table_left) >= 2) {
      Pitch_Stats_Table <- Pitch_Stats_Table %>% 
        formatStyle(0, target = "row", 
                    backgroundColor = styleEqual(c("TOTALS (L)", "TOTALS (R)"), 'aliceblue'),
                    fontWeight = styleEqual(c("TOTALS (L)", "TOTALS (R)"), "bold"))
    }}
    Pitch_Stats_Table
  })
  
  # PITCH VELOCITY TRENDLINES: 
  output$PVTrendlinesPlot <- renderPlot({
    
    width = 850 
    height = 450
    
    Filtered_Data <- CCBData %>%
      filter(Games %in% input$PitcherGameInput,
             Pitcher == input$PitcherInput) %>%
      group_by(Pitcher, TaggedPitchType, game_date) %>%
      summarize('mean_release_speed' = mean(RelSpeed, na.rm = T))
    
    ggplot() + 
      geom_point(data = Filtered_Data, size = 4, na.rm = T,
                 mapping = aes(x = game_date, y = mean_release_speed, group = Pitcher, color = TaggedPitchType)) + 
      
      labs(x = "Game Date", y = "Velocity (MPH)", title = paste("Pitcher:", input$PitcherInput, color = "")) +
      theme(plot.title = element_text(face = "bold", size = 24))
    
  }) 
  
  # RELEASE POINT PLOT: 
  output$ReleasePointPlot <- renderPlot({
    
    ggplot() + 
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_point(data = Pitcher_Data(), mapping = aes(x = RelSide, y = RelHeight, color = TaggedPitchType), 
                 size = 4, na.rm = T) +
      geom_segment(aes(x = -0.7, y = -0.5, xend = -0.7, yend = 0.5), color="black") +
      geom_segment(aes(x = -0.7, y = 0.5, xend = 0.7, yend = 0.5), color="black") +
      geom_segment(aes(x = 0.7, y = 0.5, xend = 0.7, yend = -0.5), color="black") + 
      ylim(-0.5, 7) +
      
      labs(x = "RelSide", y = "RelHeight", title = paste("Pitcher: ", input$PitcherInput)) +
      theme(plot.title = element_text(face = "bold", size = 24))
    
  })
  
  # VELOCITY BY PITCH COUNT: 
  output$VelocityPitchCountPlot <- renderPlot({   # Velocity by Pitch Count for pitchers
    
    ggplot() + 
      geom_point(data = Pitcher_Data(), size = 4, na.rm = T,
                 mapping = aes(x = Counter_1, y = EffectiveVelo, color = TaggedPitchType)) + 
      geom_smooth(data = Pitcher_Data(), method = lm, se = F,
                  mapping = aes(x = Counter_1, y = EffectiveVelo, color = TaggedPitchType)) +
      
      labs(title = paste("Pitcher:", input$PitcherInput), x = "Pitch Count", y = "Effective Velocity") + 
      theme(plot.title = element_text(face = "bold", size = 24))
    
  })
  
  # VERT/HORZ BREAK SCATTER PLOT
  output$VertHorzBreakScatterPlot <- renderPlot({ #Vertical and horizontal break plot
    
    max_break_point <- max(c(abs(Pitcher_Data()$HorzBreak), abs(Pitcher_Data()$InducedVertBreak)), na.rm = T) + 1
    
    ggplot() + 
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_point(data = Pitcher_Data(), size = 4, na.rm = T, 
                 aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      
      xlim(-max_break_point, max_break_point) +
      ylim(-max_break_point, max_break_point) +
      labs(title = paste("Pitcher:", input$PitcherInput)) +
      theme(plot.title = element_text(face = "bold", size = 24))
    
  })
  
  # SPRAY CHART: shows spray chart of the field.
  output$SprayChartPlot1 <- renderPlot({
    
    HundredFtArcs <- c(100, 200, 300, 400)
    FiftyFtArcs <- c(50, 150, 250, 350)
    
    Plot <- geom_baseball(league = "MLB", field_units = "ft",
                          # Enables choice of which portion of the field is visible
                          display_range = case_when(input$SprayChartRange == "Infield + Outfield" ~ "full",
                                                    input$SprayChartRange == "Infield Only" ~ "infield")) +
      # Creates distance arcs
      geom_arc(aes(x0 = 0, y0 = 0, start = -pi/4, end = pi/4,
                   r = case_when(input$SprayChartArcs == "None" ~ 0,
                                 input$SprayChartArcs == "Every 100ft" | input$SprayChartArcs == "Every 50ft" ~ HundredFtArcs))) +
      geom_arc(aes(x0 = 0, y0 = 0, start = -pi/4, end = pi/4,
                   r = case_when(input$SprayChartArcs == "Every 50ft" ~ FiftyFtArcs,
                                 input$SprayChartArcs == "None" | input$SprayChartArcs == "Every 100ft" ~ 0)),
               color = "gray") +
      # Plots landing point of balls
      geom_point(data = Pitcher_Data(), size = 5, na.rm = T,
                 aes(x = hc_x, y = hc_y, shape = TaggedHitType,
                     color = case_when(input$SprayChartColor == "Pitch Type" ~ TaggedPitchType,
                                       input$SprayChartColor == "Play Result" ~ PlayResult))) +
      scale_shape_manual(values = c("GroundBall" = 15, "FlyBall" = 16, "LineDrive" = 17, "Bunt" = 4, "Popup" = 18)) +
      
      
      labs(title = paste("Pitcher:", input$PitcherInput), color = paste(input$SprayChartColor), shape = "Hit Type") +
      theme(plot.title = element_text(face = "bold", size = 24))
    
    Plot
  })
  
  # SPRAY CHART HOVER
  output$SprayChartPlot2 <- renderPlotly({
    
    
    # reverses the order of first and last name of batter
    BatterFirstLast <- Pitcher_Data() %>%
      mutate(Batter = str_split(Batter, ", ")) %>%
      mutate(Batter = map_chr(Batter, ~ paste(rev(.x), collapse = ",")))
    
    
    # Used for drawing the field
    ({
      InfieldCircleXVals <- c(-90.229, -90:90, 90.229)
      InfieldCircle <- data.frame(x = InfieldCircleXVals,
                                  y = (sqrt(95^2 - InfieldCircleXVals^2) + 60.5))
      OutfieldCircleXVals <- c(-221.32, -221:221, 221.32)
      OutfieldCircle <- data.frame(x = OutfieldCircleXVals,
                                   y = (sqrt(51917.3395 - OutfieldCircleXVals^2) + 167.1462))
      PitcherMoundXVals <- c(-9:9)
      PitcherMoundPlus <- data.frame(x = PitcherMoundXVals,
                                     y = (sqrt(81 - PitcherMoundXVals^2) + 60.5))
      PitcherMoundMinus <- data.frame(x = PitcherMoundXVals,
                                      y = (-sqrt(81 - PitcherMoundXVals^2) + 60.5))
      XMin <- ifelse(input$SprayChartRange2 == "Infield Only", -100,
                     ifelse(input$SprayChartRange2 == "Zoom Home Plate", -20, -225))
      XMax <- ifelse(input$SprayChartRange2 == "Infield Only", 100,
                     ifelse(input$SprayChartRange2 == "Zoom Home Plate", 20, 225))
      YMax <- ifelse(input$SprayChartRange2 == "Infield Only", 175,
                     ifelse(input$SprayChartRange2 == "Zoom Home Plate", 25, 425))
    })
    
    Plot <- ggplot() +
      # These geom_path elements draw the field
      geom_path(mapping = aes(x = c(0, -221.32, -60.5, 0, 60.5, 221.32, 0),
                              y = c(0, 221.32, 60.5, 121, 60.5, 221.32, 0))) +
      geom_path(mapping = aes(x = c(-100, 0, 100), y = c(100, 0, 100))) +
      geom_path(mapping = aes(x = c(-20, 0, 20), y = c(20, 0, 20))) +
      geom_path(mapping = aes(x = c(0, -0.707, -0.707, 0.707, 0.707, 0),
                              y = c(0, 0.707, 1.414, 1.414, 0.707, 0))) +
      geom_path(data = PitcherMoundPlus, mapping = aes(x, y)) +
      geom_path(data = PitcherMoundMinus, mapping = aes(x, y)) +
      geom_path(data = InfieldCircle, mapping = aes(x, y), col = "black") +
      geom_path(data = OutfieldCircle, mapping = aes(x, y), col = "black") +
      
      coord_fixed() + xlab("") + ylab("") + 
      xlim(XMin, XMax) + ylim(-5, YMax) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "darkolivegreen", color = "darkolivegreen",
                                            size = 0.5, linetype = "solid")) +
      # Plots the hits on the graph
      geom_point(data = BatterFirstLast, size = 2, 
                 aes(x = hc_x, y = hc_y, color = TaggedPitchType,
                     
                     text = paste( Batter, ifelse(BatterSide == "Right", ", R ", ", L "),"(",BatterTeam,")",
                                   "<br><b>EV:</b>", format(round(ExitSpeed, 1), nsmall = 1),", ",TaggedPitchType,"(",format(round(RelSpeed, 1), nsmall = 1),")",
                                   "<br>", TaggedHitType,", ",PlayResult,
                                   "<br><b>Count:</b> ", Count)
                 ))
    
    
    
    
    # Converts the ggplot element into a plotly element, and removes x and y values from the hover
    Plot <- Plot %>% ggplotly(tooltip="text") %>% config(displayModeBar = F)
    
    Plot
  })
  
  ## Hoverd Kzone graph  for a particular pitch 
  observe({
    output$PitchKzone <- renderPlot({
      Plot <- ggplot() + 
        geom_point(data = Pitcher_Data(), size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType))
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch")
      Plot
      
    })
  })
  #, width = Plot_Width(), height = Plot_Height())
  
  
  
  
  
  # * KZONE PITCHING CHARTS *
  # These charts are placed within "observe" functions so their widths and heights can be reactive
  
  # PITCH LOCATIONS:
  observe({
    output$PitchLocationPlot <- renderPlot({
      
      Plot <- ggplot() + 
        geom_point(data = Pitcher_Data(), size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType))
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch")
      Plot
      
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # KZone/OZone Pitch Result:
  observe({
    output$PitchCallPlot <- renderPlot({  
      
      Plot <- ggplot() + 
        geom_point(data = Pitcher_Data(), size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch")
      Plot
      
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # EFFECTIVE VELOCITY: 
  ({
    # Creates first plot
    output$EffectiveVelocityPlot1 <- renderPlot({ 
      
      
      Filtered_Data <- Pitcher_Data()
      
      if(input$EVComparison == "Yes") {
        Filtered_Data <- Pitcher_Data() %>% filter(Games == input$ConditionalPitcherGameInput1)
      }
      
      pitcher_Filtered_Data <- CCBData %>% 
        filter(Pitcher == input$PitcherInput,
               Count %in% input$PitcherCountInput,
               TaggedPitchType %in% input$PitcherPitchType)
      
      Plot <- ggplot() + 
        geom_point(data = Filtered_Data, na.rm = T, size = 3,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = EffectiveVelo)) +
        scale_color_gradient(limits = c(floor(min(pitcher_Filtered_Data$EffectiveVelo, na.rm = T)), 
                                        ceiling(max(pitcher_Filtered_Data$EffectiveVelo, na.rm = T))), 
                             low = "red", high = "green")
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch") +
        labs(# Title and subtitle are conditional based on whether or not EV Comparison is selected
          title =  paste("Pitcher:", input$PitcherInput),
          subtitle = case_when(input$EVComparison == "Yes" ~ paste(input$ConditionalPitcherGameInput1),
                               input$EVComparison == "No" ~ ""),
          caption = paste("Note: Max (", ceiling(max(pitcher_Filtered_Data$EffectiveVelo, na.rm = T)), 
                          ") and Min (", floor(min(pitcher_Filtered_Data$EffectiveVelo, na.rm = T)), 
                          ") values of color gradient are determined \n by the pitcher's highest and lowest EV across all available games"), 
          sep = "") +
        theme(plot.title = element_text(face = "bold", size = 24),
              plot.caption = element_text(size = 13), strip.text = element_text(size = 10) )
      
      Plot
    })
    # Creates second plot if "Compare 2 Games" is selected
    output$EffectiveVelocityPlot2 <- renderPlot({
      
      Filtered_Data <- Pitcher_Data() %>%
        filter(Games == input$ConditionalPitcherGameInput2)
      pitcher_Filtered_Data <- CCBData %>% 
        filter(Pitcher == input$PitcherInput,
               Count %in% input$PitcherCountInput,
               TaggedPitchType %in% input$PitcherPitchType)
      
      if(input$EVComparison == "Yes") {
        
        Plot <- ggplot() + 
          geom_point(data=Filtered_Data, na.rm = T, size = 3,
                     aes(x = PlateLocSide, y = PlateLocHeight, color = EffectiveVelo)) +
          scale_color_gradient(limits = c(floor(min(pitcher_Filtered_Data$EffectiveVelo, na.rm = T)), 
                                          ceiling(max(pitcher_Filtered_Data$EffectiveVelo, na.rm = T))), 
                               low = "red", high = "green")
        
        Plot <- Kzone_Chart_Setup(Plot, "pitch") +
          labs(title = paste("Pitcher:", input$PitcherInput), subtitle = paste(input$ConditionalPitcherGameInput2), sep = "")
        
        Plot
        
      }
    })
  })
  
  # KZone/OZone Outcome
  observe({
    output$PlayResultPlot <- renderPlot({           #Play Results from Pitching persepective
      
      NewPlayResult <- with(subset(Pitcher_Data(), PlayResult != "Undefined" | KorBB == "Strikeout"), 
                            replace(PlayResult, PlayResult == "Undefined", "Strikeout"))
      Filtered_Data <- Pitcher_Data() %>% filter(PlayResult != "Undefined" | KorBB == "Strikeout")
      
      Plot <- ggplot() + 
        geom_point(data = Filtered_Data, size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = NewPlayResult))
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch")
      Plot
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # STRIKEZONE HEATMAP:
  observe({
    output$StrikezoneHeatmapPlot <- renderPlot({    #Strike zone heatmap by batter handedness
      
      Plot <- ggplot() + 
        stat_density2d(data = Pitcher_Data(), na.rm = T,
                       aes(x = PlateLocSide, y = PlateLocHeight, fill = ..level.., alpha = ..level..),
                       geom = "polygon", size = 0.01, bins = 16) +
        scale_fill_gradient(low = "red", high = "green") +
        scale_alpha(range = c(0, 0.3), guide = F)
      
      Plot <- Kzone_Chart_Setup(Plot, "pitch")
      Plot
      
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # PITCHER GRIDS: Displays pitcher grid that contains percentages of where pitches land in a grid
  ({
    # PLOT 1
    output$PitcherGridsPlot1 <- renderPlot({
      
      Filtered_Data <- Pitcher_Data()
      
      if(input$BattersideSwitch == T) {
        Filtered_Data <- Filtered_Data %>% filter(BatterSide == "Left")
      }
      
      Sum_Zones_Vec <- Grids_Sum_Fnct(Filtered_Data)
      Sum_All_Zone <- sum(Sum_Zones_Vec)
      Colors_Vec <- Grids_Color_Fnct(Sum_Zones_Vec, Sum_All_Zone)
      Zone_Percent_Vec <- (c(Sum_Zones_Vec/Sum_All_Zone)*100) %>% round(1) %>% format(nsmall = 1)
      Alpha_Vec <- c(round(Sum_Zones_Vec/Sum_All_Zone, 3))
      
      Plot <- ggplot() +
        geom_point(data = Filtered_Data, na.rm = T, 
                   aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) + 
        
        coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) + 
        geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1) +
        labs(title = paste("Pitcher:", input$PitcherInput)) +
        theme(plot.title = element_text(face = "bold", size = 24)) +
        {if(input$BattersideSwitch == T) guides(color = FALSE)}
      
      Plot <- Grids_Text_Fnct(Plot, Zone_Percent_Vec)
      Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
      Plot <- Grids_Border_Fnct(Plot)
      
      Plot
    }, width = 500, height = 600)
    
    # PLOT 2 (only used if comparing L/R)
    output$PitcherGridsPlot2 <- renderPlot({
      
      Filtered_Data <- Pitcher_Data() %>% filter(BatterSide == "Right")
      
      Sum_Zones_Vec <- Grids_Sum_Fnct(Filtered_Data)
      Sum_All_Zone <- sum(Sum_Zones_Vec)
      Colors_Vec <- Grids_Color_Fnct(Sum_Zones_Vec, Sum_All_Zone)
      Zone_Percent_Vec <- (c(Sum_Zones_Vec/Sum_All_Zone)*100) %>% round(1) %>% format(nsmall = 1)
      Alpha_Vec <- c(round(Sum_Zones_Vec/Sum_All_Zone, 3))
      
      if(input$BattersideSwitch == T) {
        
        Plot <- ggplot() +
          geom_point(data = Filtered_Data, na.rm = T, 
                     aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) + 
          
          coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) + 
          geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1) +
          labs(title = paste("Pitcher:", input$PitcherInput)) +
          theme(plot.title = element_text(face = "bold", size = 24))
        
        Plot <- Grids_Text_Fnct(Plot, Zone_Percent_Vec)
        Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
        Plot <- Grids_Border_Fnct(Plot)
        
        Plot
      }
    }, width = 500, height = 600)
  })
  
  
  
  # ** BATTER TABLES **
  
  # BATTING STATISTICS TABLE: displays variety of batting statistics summarized by pitch type. Data table.
  output$BatterTable <- renderDataTable({
    
    # Creates two new data tables filtered by batter side
    filtered_table_left <- filter(Batter_Data(), PitcherThrows == "Left")
    filtered_table_right <- filter(Batter_Data(), PitcherThrows == "Right")
    
    batter_stats_function <- function(data) {
      data %>% summarize(
        'No.' = n(),
        'Usage %' = (n() / nrow(data)*100) %>% 
          round(1) %>% format(nsmall = 1),
        '90% Exit Velo' = quantile(ExitSpeed, probs = 0.9, na.rm = T) %>% 
          round(1) %>% format(nsmall = 1),
        'Whiff%' = (sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% PitchCall_Swinging)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'Slug%' = (sum(SLG_VAL, na.rm = T) / sum(AB, na.rm = T)) %>% 
          round(3) %>% format(nsmall = 3),
        'OSwing%' = ((sum(Out_Zone_Swinging, na.rm = T) / sum(Out_Zone_NotSwinging + Out_Zone_Swinging, na.rm = T))*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing%' = ((sum(In_Zone_Swinging, na.rm = T) / sum(In_Zone_NotSwinging + In_Zone_Swinging, na.rm = T))*100) %>% 
          round(1) %>% format(nsmall = 1),
        'OBP% Old' = round(((sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm=TRUE) + 
                               sum(KorBB %in% c("Walk"), na.rm=TRUE) + sum(PitchCall %in% c("HitByPitch"), na.rm=TRUE)) /
                              (sum(AB, na.rm=TRUE) + sum(KorBB %in% c('Walk'), na.rm=TRUE) + 
                                 sum(PitchCall %in% c("HitByPitch"), na.rm=TRUE) + sum(SacrificeFly, na.rm=TRUE))), 3) %>% format(nsmall = 3),
        'OBP% New' = ((sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun") | KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = T)) /
                        (sum(AB == 1 | KorBB == "Walk" | PitchCall == "HitByPitch" | SacrificeFly == 1, na.rm=T))) %>% 
          round(3) %>% format(nsmall = 3), 
        'OSwing Whiff%' = (sum(Out_Zone_Swinging == 1 & PitchCall == "StrikeSwinging", na.rm = T) / sum(Out_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'OSwing Contact%' = (sum(Out_Zone_Swinging == 1 & PitchCall %in% c("FoulBall", "InPlay"), na.rm = T) / sum(Out_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing Whiff%' = (sum(In_Zone_Swinging == 1 & PitchCall == "StrikeSwinging", na.rm = T) / sum(In_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1),
        'ZSwing Contact%' = (sum(In_Zone_Swinging == 1 & PitchCall %in% c("FoulBall", "InPlay"), na.rm = T) / sum(In_Zone_Swinging, na.rm = T)*100) %>% 
          round(1) %>% format(nsmall = 1)
      )
    }
    
    table <- Batter_Data() %>% group_by('Pitch' = TaggedPitchType) %>% batter_stats_function()
    totals_table <- Batter_Data() %>% batter_stats_function()
    
    table <- rbind(table, totals_table %>% mutate(Pitch = "TOTALS"))
    
    table_left <- filtered_table_left %>% group_by('Pitch' = TaggedPitchType) %>% batter_stats_function()
    table_right <- filtered_table_right %>% group_by('Pitch' = TaggedPitchType) %>% batter_stats_function()
    
    # Modifies the "Pitch" column to indicate pitcher side
    table_left$Pitch <- paste(table_left$Pitch, "(L)")
    table_right$Pitch <- paste(table_right$Pitch, "(R)")
    
    # Creates a row that shows the totals for the left and right batter side stats  
    totals_table_left <- filtered_table_left %>% batter_stats_function()
    totals_table_right <- filtered_table_right %>% batter_stats_function()
    
    
    # Creates a batting statistics table that separates L and R pitcher side statistics
    if(input$PitcherThrowsSwitch == T) {
      if(nrow(table_left) <= 1) {table <- rbind(table_left, table_right)}
      else{table <- rbind(table_left, totals_table_left %>% mutate(Pitch = "TOTALS (L)"), 
                          table_right, totals_table_right %>% mutate(Pitch = "TOTALS (R)"))}
    }
    
    Bat_Stats_Table <- datatable(table, options = list(searching = F, paging = F, info = F, scrollX = T),
                                 # changes the datatable title
                                 caption = htmltools::tags$caption(
                                   htmltools::tags$span("Batter:    ", style="color:orange; text-align: left; font-size:200%; font-weight:bold"), 
                                   htmltools::tags$span(input$BatterInput, style="color:black; text-align: left; font-size:200%; font-weight:bold")
                                 )
    )
    
    if(input$PitcherThrowsSwitch == F) {
      Bat_Stats_Table <- Bat_Stats_Table %>% 
        formatStyle(0, target = "row", 
                    backgroundColor = styleEqual(nrow(table), 'aliceblue'),
                    fontWeight = styleEqual(nrow(table), "bold"))
      
    } else if(nrow(table_left) >= 2) {
      Bat_Stats_Table <- Bat_Stats_Table %>% 
        formatStyle(0, target = "row", 
                    backgroundColor = styleEqual(c(nrow(table_left)+1, nrow(table)), 'aliceblue'),
                    fontWeight = styleEqual(c(nrow(table_left)+1, nrow(table)), "bold"))
    }
    Bat_Stats_Table
  })
  
  # BATTER CONTACT RESULT: Play result for at bats
  observe({
    output$BatterContactResultPlot <- renderPlot({          #Batter contacts / at bat play results 
      
      Filtered_Data <- Batter_Data() %>% filter(PlayResult != "Error") %>% 
        filter(PlayResult != "Undefined" | KorBB == "Strikeout")
      NewPlayResult <- with(subset(Filtered_Data, PlayResult != "Undefined" | KorBB == "Strikeout"),
                            replace(PlayResult, PlayResult == "Undefined", "Strikeout"))
      
      Plot <- ggplot() + 
        geom_point(data = Filtered_Data, size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = NewPlayResult))
      
      Plot <- Kzone_Chart_Setup(Plot, "bat")
      Plot
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # BATTER EXIT SPEED:
  observe({
    output$BatterExitSpeedPlot <- renderPlot({       #Batter Exit Speed Chart
      
      batter_Filtered_Data <- CCBData %>% 
        filter(Batter == input$BatterInput,
               Count %in% input$BatterCountInput,
               TaggedPitchType %in% input$BatterPitchType)
      
      Plot <- ggplot() + 
        geom_point(data = Batter_Data(), size = 3, na.rm = T,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = ExitSpeed)) +
        scale_color_gradient(limits = c(floor(min(batter_Filtered_Data$ExitSpeed, na.rm = T)), 
                                        ceiling(max(batter_Filtered_Data$ExitSpeed, na.rm = T))), 
                             low = "red", high = "green", na.value = NA)
      
      Plot <- Kzone_Chart_Setup(Plot, "bat")
      Plot
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # SLG GRIDS:
  ({
    # PLOT 1
    output$SLGGridsPlot1 <- renderPlot({      #Slugging grids for batters
      
      Filtered_Data <- Batter_Data() %>% filter(PlayResult != "Undefined",
                                                PlayResult != "Error")
      Filtered_Data_AB <- Filtered_Data %>% filter(AB == 1)
      
      if(input$PitcherThrowsSwitch == T) {
        Filtered_Data <- Filtered_Data %>% filter(PitcherThrows == "Left")
      }
      
      sum_zone_5 <- nrow(subset(Filtered_Data, Zone_5 == 1 & AB == 1))
      slg_zone_5 <- subset(Filtered_Data, Zone_5 == 1)
      slg_sum_5 <- sum(slg_zone_5$SLG_VAL)
      
      print(sum_zone_5)
      print(slg_sum_5)
      print(round(slg_sum_5/sum_zone_5,3)*100)
      
      
      slg_zone_sum_vector <- c(sum(subset(Filtered_Data, Zone_1 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_2 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_3 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_4 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_5 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_6  == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_7 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_8 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_9 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_L1 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_L2 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_L3 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_L4 == 1)$SLG_VAL))
      
      zone_sums <- Grids_Sum_Fnct(Filtered_Data_AB)
      Sum_All_Zone <- sum(zone_sums)
      Colors_Vec <- Grids_Color_Fnct(slg_zone_sum_vector, Sum_All_Zone)
      Zone_Slug_Vec <- c(round(slg_zone_sum_vector/zone_sums, 3))
      Alpha_Vec <- c(round(slg_zone_sum_vector/zone_sums, 3))/10
      
      print(slg_zone_sum_vector)
      print(zone_sums)
      print(Zone_Slug_Vec)
      print(Alpha_Vec)
      
      
      Plot <- ggplot() + 
        labs(title = paste("Batter:", input$BatterInput)) +
        theme(plot.title = element_text(face = "bold", size = 24), strip.text = element_text(size = 10)) +
        geom_point(data = Filtered_Data, aes(x = PlateLocSide, y = PlateLocHeight, color = PlayResult), na.rm = T) + 
        coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) +
        geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1)
      
      Plot <- Grids_Border_Fnct(Plot)
      Plot <- Grids_Text_Fnct(Plot, Zone_Slug_Vec)
      Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
      
      Plot
    })
    # PLOT 2 (only used if comparing L/R)
    output$SLGGridsPlot2 <- renderPlot({
      
      Filtered_Data <- Batter_Data() %>% filter(PlayResult != "Undefined",
                                                PlayResult != "Error",
                                                PitcherThrows == "Right")
      Filtered_Data_AB <- Filtered_Data %>% filter(AB == 1)
      
      slg_zone_sum_vector <- c(sum(subset(Filtered_Data, Zone_1 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_2 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_3 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_4 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_5 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_6  == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_7 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_8 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_9 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_L1 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_L2 == 1)$SLG_VAL), sum(subset(Filtered_Data, Zone_L3 == 1)$SLG_VAL), 
                               sum(subset(Filtered_Data, Zone_L4 == 1)$SLG_VAL))
      
      zone_sums <- Grids_Sum_Fnct(Filtered_Data_AB)
      Sum_All_Zone <- sum(zone_sums)
      Colors_Vec <- Grids_Color_Fnct(slg_zone_sum_vector, Sum_All_Zone)
      Zone_Slug_Vec <- c(round(slg_zone_sum_vector/zone_sums, 3))
      Alpha_Vec <- c(round(zone_sums/zone_sums, 3))
      
      if(input$PitcherThrowsSwitch == T) {
        
        Plot <- ggplot() + 
          labs(title = paste("Batter:", input$BatterInput)) +
          theme(plot.title = element_text(face = "bold", size = 24), strip.text = element_text(size = 10)) +
          geom_point(data = Filtered_Data, aes(x = PlateLocSide, y = PlateLocHeight, color = PlayResult), na.rm = T) + 
          coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) +
          geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1)
        
        Plot <- Grids_Border_Fnct(Plot)
        Plot <- Grids_Text_Fnct(Plot, Zone_Slug_Vec)
        Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
        
        Plot
      }
    })
  })
  
  # AVERAGE EXIT VELOCITY GRIDS:
  ({
    output$AvgExitVeloPlot1 <- renderPlot({            #Average exit velocity grid for batters 
      
      Filtered_Data <- Batter_Data()  %>% filter(ExitSpeed != ' ', PitchCall != "FoulBall")
      
      if(input$PitcherThrowsSwitch == T) {
        Filtered_Data <- Filtered_Data %>% filter(PitcherThrows == "Left")
      }
      
      avg_EV_vec <- c(mean(subset(Filtered_Data, Zone_1 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_2 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_3 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_4 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_5 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_6  == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_7 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_8 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_9 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_L1 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_L2 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_L3 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_L4 == 1)$ExitSpeed))
      avg_EV_vec[is.na(avg_EV_vec)] <- 0
      
      sum_zone_vec <- Grids_Sum_Fnct(Filtered_Data)
      Sum_All_Zone <- sum(sum_zone_vec)
      Colors_Vec <- Grids_Color_Fnct(avg_EV_vec, Sum_All_Zone)
      avg_EV_text_vec <- round(avg_EV_vec, 1)
      Alpha_Vec <- avg_EV_vec/300
      
      Plot <- ggplot() + 
        labs(title = paste("Batter:", input$BatterInput)) +
        geom_point(data = Filtered_Data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), na.rm = T) + 
        coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) +
        geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1)
      
      Plot <- Grids_Text_Fnct(Plot, avg_EV_text_vec)
      Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
      Plot <- Grids_Border_Fnct(Plot)
      
      Plot
    })
    # PLOT 2 (only used if comparing L/R)
    output$AvgExitVeloPlot2 <- renderPlot({
      
      Filtered_Data <- Batter_Data()  %>% filter(ExitSpeed != ' ',
                                                 PitcherThrows == "Right")
      
      avg_EV_vec <- c(mean(subset(Filtered_Data, Zone_1 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_2 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_3 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_4 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_5 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_6  == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_7 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_8 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_9 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_L1 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_L2 == 1)$ExitSpeed), mean(subset(Filtered_Data, Zone_L3 == 1)$ExitSpeed),
                      mean(subset(Filtered_Data, Zone_L4 == 1)$ExitSpeed))
      avg_EV_vec[is.na(avg_EV_vec)] <- 0
      
      sum_zone_vec <- Grids_Sum_Fnct(Filtered_Data)
      Sum_All_Zone <- sum(sum_zone_vec)
      Colors_Vec <- Grids_Color_Fnct(avg_EV_vec, Sum_All_Zone)
      avg_EV_text_vec <- round(avg_EV_vec, 1)
      Alpha_Vec <- avg_EV_vec/300
      
      if(input$PitcherThrowsSwitch == T) {
        Plot <- ggplot() + 
          labs(title = paste("Batter:", input$BatterInput)) +
          geom_point(data = Filtered_Data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), na.rm = T) + 
          coord_fixed() + xlim(-1.5, 1.5) + ylim(0, 4) +
          geom_path(data = Home_Plate, mapping = aes(x, y), col = "gray", lwd = 1)
        
        Plot <- Grids_Text_Fnct(Plot, avg_EV_text_vec)
        Plot <- Grids_ColorFill_Fnct(Plot, Alpha_Vec, Colors_Vec)
        Plot <- Grids_Border_Fnct(Plot)
        
        Plot
      }
    })
  })
  
  # BATTER VS L/R HANDED PITCHER
  observe({
    output$BatterHandedPitcherPlot <- renderPlot({
      
      Plot <- ggplot() +
        geom_point(data = Batter_Data(), na.rm = T, size = 3,
                   aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))
      
      Plot <- Kzone_Chart_Setup(Plot, "bat")
      Plot
      
    }, width = Plot_Width(), height = Plot_Height())
  })
  
  # * UMPIRE CHARTS *
  
  # SEPARATE TEAM:
  output$SeparateTeamPlot <- renderPlot({     #Seperate team umpire calls 
    dataFilter <- 
      CCBData %>%
      filter(Games %in% input$UmpireGameInput, 
             # TaggedPitchType %in% input$PitcherPitchType, 
             # Count %in% input$PitcherCountInput,
             PitchCall == "BallCalled"|PitchCall == "StrikeCalled") %>% 
      mutate(GameDate = paste("Date:", Date, "Game:", GameNumber))
    
    
    ggplot(dataFilter,aes(x=Counter,y=EffectiveVelo,color = TaggedPitchType)) + 
      geom_point(size = 4) + geom_smooth(method = lm, se = F)
    
    
    ggplot() + 
      geom_point(data=dataFilter,
                 aes(x=PlateLocSide,y=PlateLocHeight,color = PitchCall),
                 size = 2) +
      
      geom_path(data = kZone, mapping = aes(x, y), col = "black", lwd = 1.5) +
      coord_fixed() + xlim(-1.5, 1.5) + ylim(1, 4) + xlab("") + ylab("") +
      facet_wrap(GameDate ~ PitcherTeam) + 
      theme(strip.text = element_text(size = 10))
  })
  
  # ALL TEAMS: 
  output$AllTeamsPlot <- renderPlot({       #All teams umpire call data 
    dataFilter <- 
      CCBData %>%
      filter(Games %in% input$PitcherGameInput,TaggedPitchType %in% input$PitcherPitchType, Count %in% input$PitcherCountInput,
             PitchCall == "BallCalled"|PitchCall == "StrikeCalled", PitcherTeam %in% input$PitcherTeamInput)
    
    sum_IZB <- sum(dataFilter$In_Zone_Ball)
    sum_OZB <- sum(dataFilter$Out_Zone_Ball)
    sum_IZS <- sum(dataFilter$In_Zone_Swinging)
    sum_OZS <- sum(dataFilter$Out_Zone_Swinging)
    
    
    ggplot(data=dataFilter,aes(x=PlateLocSide,y=PlateLocHeight,color = PitchCall),
           size = 3) + 
      labs(title = paste(input$PitcherTeamInput)) +
      geom_point() + annotate("text", x = -2, y = 4.8, label = toString(round((sum_OZB+sum_IZS)/(sum_OZB+sum_IZB+sum_IZS+sum_OZS),3)*100),col = "black") +
      annotate("text", x = -2, y = 5, label = "Effective Call Percent",col = "black") +
      coord_fixed() +
      
      geom_segment(aes(x=-0.95,y=2.82,xend=0.95,yend=2.82),
                   colour="black") +
      geom_segment(aes(x=-0.95,y=1.6,xend=-0.95,yend=3.5),
                   colour="black") +
      geom_segment(aes(x=-0.95,y=3.5,xend=0.95,yend=3.5),
                   colour="black") +
      geom_segment(aes(x=0.95,y=3.5,xend=0.95,yend=1.6),
                   colour="black") +
      geom_segment(aes(x=-0.95,y=1.6,xend=0.95,yend=1.6),
                   colour="black") +
      geom_segment(aes(x=-0.95,y=2.23,xend=0.95,yend=2.23),
                   colour="black") +
      geom_segment(aes(x=-0.31,y=3.5,xend=-0.31,yend=1.6),
                   colour="black") +
      geom_segment(aes(x=0.32,y=3.5,xend=0.32,yend=1.6),
                   colour="black") +
      
      geom_segment(aes(x=-0.95,y=2.525,xend=-1.3,yend=2.525),
                   colour="black") +
      geom_segment(aes(x=0.95,y=2.525,xend=1.3,yend=2.525),
                   colour="black") +
      geom_segment(aes(x=0.005,y=1.6,xend=0.005,yend=1.25),
                   colour="black") +
      geom_segment(aes(x=0.005,y=3.5,xend=0.005,yend=3.85),
                   colour="black") +
      geom_segment(aes(x=-1.3,y=3.85,xend=1.3,yend=3.85),
                   colour="black") +
      geom_segment(aes(x=-1.3,y=3.85,xend=-1.3,yend=1.25),
                   colour="black") +
      geom_segment(aes(x=-1.3,y=1.25,xend=1.3,yend=1.25),
                   colour="black") +
      geom_segment(aes(x=1.3,y=1.25,xend=1.3,yend=3.85),
                   colour="black") + 
      
      xlab("") + ylab("") +
      theme(strip.text = element_text(size = 10))
  })
  
  # HEATMAP:
  output$UmpireHeatmapPlot <- renderPlot({    #Heatmap for umpire calls
    dataFilter <- CCBData %>% 
      filter(Games %in% input$PitcherGameInput, 
             PitchCall == "BallCalled"| PitchCall == "StrikeCalled",
             Count %in% input$PitcherCountInput,
             TaggedPitchType %in% input$PitcherPitchType)
    
    ggplot() + 
      stat_density2d(data = dataFilter, aes(x = PlateLocSide, y = PlateLocHeight, 
                                            fill = ..level.., alpha = ..level..),
                     geom = "polygon", size = 0.01, bins = 16) +
      scale_fill_gradient(low = "red", high = "green") +
      scale_alpha(range = c(0, 0.3), guide = F) +
      
      geom_path(data = kZone, mapping = aes(x, y), col = "black", lwd = 1.5) +
      coord_fixed() + xlim(-1.5, 1.5) + ylim(1, 4) + xlab("") + ylab("") +
      facet_wrap(~PitchCall) + 
      theme(strip.text = element_text(size = 10))
  })
  
  
  
  # ** UNUSED CHARTS **
  
  # VERT/HORZ BREAK 3D PLOT: 
  output$VertHorzBreak3DPlot <- renderPlotly({  #Renders a 3D plot of the vertical and horizontal break along with release pt
    theta=0 
    phi=20
    
    # ggplot(dataFilter,aes(x=HorzBreak,y=InducedVertBreak,z=EffectiveVelo,color = TaggedPitchType)) +
    #     theme_void() +
    #     axes_3D() +
    #     stat_3D() + labs_3D(
    #         labs=c("Horz break", "Vert Break", "Effective Velo"),
    #         hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 90))
    
    plot_ly(data = Pitcher_Data(), x = ~HorzBreak, y = ~InducedVertBreak,z=~EffectiveVelo,  type = 'scatter3d', mode = 'markers', color = ~TaggedPitchType) %>%
      layout(scene = list(camera = list(
        eye = list(
          x = 1.25,
          y = 1.25,
          z = 1.25
        ),
        center = list(x = 0,
                      y = 0,
                      z = 0)
      ))) %>%
      onRender("
      function(el, x){
  var id = el.getAttribute('id');
  var gd = document.getElementById(id);
  Plotly.update(id).then(attach);
  function attach() {
    var cnt = 0;
    
    function run() {
      rotate('scene', Math.PI / 180);
      requestAnimationFrame(run);
    } 
    run();
    
    function rotate(id, angle) {
      var eye0 = gd.layout[id].camera.eye
      var rtz = xyz2rtz(eye0);
      rtz.t += angle;
      
      var eye1 = rtz2xyz(rtz);
      Plotly.relayout(gd, id + '.camera.eye', eye1)
    }
    
    function xyz2rtz(xyz) {
      return {
        r: Math.sqrt(xyz.x * xyz.x + xyz.y * xyz.y),
        t: Math.atan2(xyz.y, xyz.x),
        z: xyz.z
      };
    }
    
    function rtz2xyz(rtz) {
      return {
        x: rtz.r * Math.cos(rtz.t),
        y: rtz.r * Math.sin(rtz.t),
        z: rtz.z
      };
    }
  };
}
    ")
    
  })
  
  # TILT DIAGRAMS: 
  output$TiltDiagramsPlot <- renderPlot({     #Pitching tilt diagrams
    require(grid)
    
    drawClock <- function(hour, minute) {
      t <- seq(0, 2*pi, length=13)[-13]
      x <- cos(t)
      y <- sin(t)
      
      grid.newpage()
      pushViewport(dataViewport(x, y, gp=gpar(lwd=4)))
      # Circle with ticks
      grid.circle(x=0, y=0, default="native",
                  r=unit(1, "native"))
      grid.segments(x, y, x*.9, y*.9, default="native")
      # Hour hand
      hourAngle <- pi/2 - (hour + minute/60)/12*2*pi
      grid.segments(0, 0,
                    0.7*cos(hourAngle), 0.7*sin(hourAngle),
                    default="native", gp=gpar(lex=2))
      # Minute hand
      minuteAngle <- pi/2 - (minute)/60*2*pi
      #grid.segments(0, 0,
      #.8*cos(minuteAngle), .8*sin(minuteAngle),
      #default="native", gp=gpar(lex=2))
      #grid.circle(0, 0, default="native", r=unit(1, "mm"),
      #gp=gpar(fill="white"))
      grid.text(label = paste("Pitcher:", input$PitcherInput), x = 0.5, y = 0.9, #Adds title to chart
                gp = gpar(fontsize = 16, fontface = "bold"))
    }
    drawClock(hour = Pitcher_Data()$Tilt_1, minute = Pitcher_Data()$Tilt_2)
  })
  
}

shinyApp(ui = ui, server = server)


