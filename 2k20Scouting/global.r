#########################################################################################
#                                     Leo Blakely                                       #
#                                   Modified: 1.31.2020                                 # 
#                                 2020 Robot Scouting                                   #
#   ______  _____    _____   _______                        _  _   ____    ___    ___   #
#  | ____| |  __ \  / ____| |__   __|                      | || | |___ \  / _ \  / _ \  #
#  | |__   | |__) || |         | |  ___   __ _  _ __ ___   | || |_  __) || (_) || (_) | #
#  |  __|  |  _  / | |         | | / _ \ / _` || '_ ` _ \  |__   _||__ <  > _ <  > _ <  #
#  | |     | | \ \ | |____     | ||  __/| (_| || | | | | |    | |  ___) || (_) || (_) | #
#  |_|     |_|  \_\ \_____|    |_| \___| \__,_||_| |_| |_|    |_| |____/  \___/  \___/  #
#                                                                                       #
# Global.R                                                                              #
# This file has all of the global enviroment variables, and is used to call the         #
# BlueAlliance API to get team numbers and match order                                  #
#########################################################################################

#Attaches all required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
library(plyr)
library(tibble)
library(dplyr)
library(haven)
library(tidyr)
library(lazyeval)
library(tidyverse)
library(cmna)
library(htmlwidgets)
library(plotly)
library(ggplot2)
library(shinyBS)
library(rhandsontable)
library(jsonlite)
library(httr)
library(lubridate)
library(splitstackshape)
library(stringr)
library(`data.table`)
library(devtools)
library(curl)
library(geojsonR)
library(tidyselect)
library(norm)
library(quantreg)
library(reshape2)
library(knitr)
library(kableExtra)
library(GGally)

#sets global options for R
options(stringsAsFactors = FALSE)

#Calls TheBlueAllianceAPI with V3 terminology and X-TBA-Auth-Key hard coded. 
#If the API Call is not working try changing the Auth-Key
#This is the matches simple api call

raw.result <-
  GET(
    "https://www.thebluealliance.com/api/v3/event/2020ndgf/matches/simple?X-TBA-Auth-Key=T5l4si2D8i7FOrZc9hXhUlI3CRRU1elBsM6YflBdPKQfdCExpgT0SQHQ6SuWNkMq"
  )

#The remainder of this code cleans the API raw result
#This is required as most of the data from TheBlueAlliance is unnesscary and cluttered

#Changes JSON return to a data_frame for work with r
apiCall <- content(raw.result, "text")
apiCallFromJSON <- fromJSON(apiCall, flatten = TRUE)
apiCallDF <- as.data.frame(apiCallFromJSON)
rm(apiCall, apiCallFromJSON, raw.result)

#Removes variables from the data_frame created above
apiCallDF <- apiCallDF %>%
  select(
    -actual_time,
    -comp_level,
    -event_key,
    -key,
    -match_number,
    -predicted_time,
    -set_number,
    -time,
    -winning_alliance,
    -`alliances.blue.dq_team_keys`,
    -`alliances.blue.score`,
    -`alliances.blue.surrogate_team_keys`,
    -`alliances.red.dq_team_keys`,
    -`alliances.red.score`,
    -`alliances.red.surrogate_team_keys`
  )

#Splits the dataframe to continue cleaning process
apiCallDF <- concat.split(apiCallDF, 1, sep = ",")
apiCallDF <- concat.split(apiCallDF, 2, sep = ",")

#creates final variables in data_frame
apiCallDF <- apiCallDF %>%
  select(-`alliances.blue.team_keys`, -`alliances.red.team_keys`)

#Cleans data of unnesscary strings
apiCallDF$alliances.blue.team_keys_1 <-
  str_remove(apiCallDF$alliances.blue.team_keys_1, fixed("c(\"frc"))
apiCallDF$alliances.blue.team_keys_1 <-
  str_remove(apiCallDF$alliances.blue.team_keys_1, fixed("\""))
apiCallDF$alliances.blue.team_keys_2 <-
  str_remove(apiCallDF$alliances.blue.team_keys_2, fixed("frc"))
apiCallDF$alliances.blue.team_keys_2 <-
  str_remove_all(apiCallDF$alliances.blue.team_keys_2, fixed("\""))
apiCallDF$alliances.blue.team_keys_3 <-
  str_remove(apiCallDF$alliances.blue.team_keys_3, fixed("frc"))
apiCallDF$alliances.blue.team_keys_3 <-
  str_remove_all(apiCallDF$alliances.blue.team_keys_3, fixed("\""))
apiCallDF$alliances.blue.team_keys_3 <-
  str_remove(apiCallDF$alliances.blue.team_keys_3, fixed(")"))

apiCallDF$alliances.red.team_keys_1 <-
  str_remove(apiCallDF$alliances.red.team_keys_1, fixed("c(\"frc"))
apiCallDF$alliances.red.team_keys_1 <-
  str_remove(apiCallDF$alliances.red.team_keys_1, fixed("\""))
apiCallDF$alliances.red.team_keys_2 <-
  str_remove(apiCallDF$alliances.red.team_keys_2, fixed("frc"))
apiCallDF$alliances.red.team_keys_2 <-
  str_remove_all(apiCallDF$alliances.red.team_keys_2, fixed("\""))
apiCallDF$alliances.red.team_keys_3 <-
  str_remove(apiCallDF$alliances.red.team_keys_3, fixed("frc"))
apiCallDF$alliances.red.team_keys_3 <-
  str_remove_all(apiCallDF$alliances.red.team_keys_3, fixed("\""))
apiCallDF$alliances.red.team_keys_3 <-
  str_remove(apiCallDF$alliances.red.team_keys_3, fixed(")"))

#coerses final data_frame to dataframe type from matrix
apiCallDF <- as.data.frame(apiCallDF)

#coerses final dataframe to vector to create list of all teams
apiCallVector <- as.vector(apiCallDF)

#Creates list of all teams
listOfTeams <- apiCallVector

#Removes all unnesscary values
listOfTeams <-
  data.frame('Team #' = c(t(listOfTeams)), stringsAsFactors = FALSE)

#Gets only unique values
listOfTeams <- listOfTeams %>%
  distinct()

#meltData Function, melts datatables for display in a scatter chart by useing teams as index
#' meltData
#'
#' @param x :the data frame to melt
#' @param dataSelector :the value to melt by, used to determine the data column
#'
#' @return :returns the melted data set
#' @export
#'
#' @examples :graphData <- meltData(graphData, dataTypes2)

meltData <- function(x, dataSelector) {
  x <- .na.to.snglcode(x, FALSE)
  x <- split(x, as.factor(x[["Team.."]]), drop = FALSE)
  xMelted <-
    reshape2::melt(x, id.var = "Match..", value.name = dataSelector)
  xMelted <- xMelted[xMelted[[dataSelector]] != xMelted[["L1"]], ]
  try(xMelted[[dataSelector]] <-
        as.integer(xMelted[[dataSelector]]))
  xMelted <- xMelted %>% plyr::arrange(xMelted[[dataSelector]])
  return(xMelted)
}

#################################################################################################
# ___   ___ ___   ___    _____       _           _      _____                 _   _             #
#|__ \ / _ \__ \ / _ \  |  __ \     | |         | |    / ____|               | | (_)            #
#   ) | | | | ) | | | | | |__) |___ | |__   ___ | |_  | (___   ___ ___  _   _| |_ _ _ __   __ _ #
#  / /| | | |/ /| | | | |  _  // _ \| '_ \ / _ \| __|  \___ \ / __/ _ \| | | | __| | '_ \ / _` |#
# / /_| |_| / /_| |_| | | | \ \ (_) | |_) | (_) | |_   ____) | (_| (_) | |_| | |_| | | | | (_| |#
#|____|\___/____|\___/  |_|  \_\___/|_.__/ \___/ \__| |_____/ \___\___/ \__,_|\__|_|_| |_|\__, |#
#                                                                                          __/ |#
#                                                                                         |___/ #
#                                                                                               #
#                               Leo Blakely: Handgloves Web Design                              #
#                                                                                               #
#################################################################################################
