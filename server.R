#########################################################################################
#                                     Leo Blakely                                       #
#                                   Modified: 1.12.2021                                 # 
#                                 2020 Robot Scouting                                   #
#   ______  _____    _____   _______                        _  _   ____    ___    ___   #
#  | ____| |  __ \  / ____| |__   __|                      | || | |___ \  / _ \  / _ \  #
#  | |__   | |__) || |         | |  ___   __ _  _ __ ___   | || |_  __) || (_) || (_) | #
#  |  __|  |  _  / | |         | | / _ \ / _` || '_ ` _ \  |__   _||__ <  > _ <  > _ <  #
#  | |     | | \ \ | |____     | ||  __/| (_| || | | | | |    | |  ___) || (_) || (_) | #
#  |_|     |_|  \_\ \_____|    |_| \___| \__,_||_| |_| |_|    |_| |____/  \___/  \___/  #
#                                                                                       #
# server.R                                                                              #
# This file contains all of the code that is required to make the dashboard reactive    #
# This is also where all of the data saving funcion is carried out                      #
#########################################################################################


#this is calling the shiny server function and is how it connects with the ui.R file
shinyServer(function(input, output, session) {
  
  #this is telling r that this section of the code should be reactive
  values <- reactiveValues()
  
  #this reads in all of the excel files to dataframes to that are used to display the
  #data in types
  DF1 <- readxl::read_excel("convertcsv (1).xlsx")
  DF2 <- readxl::read_excel("convertcsv (1).xlsx")
  DF3 <- readxl::read_excel("convertcsv (1).xlsx")
  DF4 <- readxl::read_excel("convertcsv (1).xlsx")
  DF5 <- readxl::read_excel("convertcsv (1).xlsx")
  DF6 <- readxl::read_excel("convertcsv (1).xlsx")
  
  #this nested obseveEvent funtion is what allows the data in this to be reactive
  #and is used to clean and filter all of the data by input slicers.
  observeEvent(input$displayTeams2, {
    observeEvent(input$dataTypesSingle, {
      observeEvent(input$dataTypesSingle4388, {
        
        #The following assign funcitons try to read in all of the data
        #that was saved to JSON files from data input
        dataBlue1 <-
          try(read_json(paste(getwd(), "/dataOutBlue1.json", sep = ""),
                        simplifyVector = TRUE))
        dataBlue2 <-
          try(read_json(paste(getwd(), "/dataOutBlue2.json", sep = ""),
                        simplifyVector = TRUE))
        dataBlue3 <-
          try(read_json(paste(getwd(), "/dataOutBlue3.json", sep = ""),
                        simplifyVector = TRUE))
        dataRed1 <-
          try(read_json(paste(getwd(), "/dataOutRed1.json", sep = ""),
                        simplifyVector = TRUE))
        dataRed2 <-
          try(read_json(paste(getwd(), "/dataOutRed2.json", sep = ""),
                        simplifyVector = TRUE))
        dataRed3 <-
          try(read_json(paste(getwd(), "/dataOutRed3.json", sep = ""),
                        simplifyVector = TRUE))
        
        #this combines all of the rows from the blue data sets and makes one global var
        try(dataBlue0 <- rbind(dataBlue1, dataBlue2))
        try(dataBlue <- rbind(dataBlue0, dataBlue3))
        try(rm(dataBlue0, dataBlue1, dataBlue2, dataBlue3))
        try(dataBlue <- dataBlue %>% as.data.frame())
        
        #this combines all of the rows from the red data sets and makes one global var
        try(dataRed0 <- rbind(dataRed1, dataRed2))
        try(dataRed <- rbind(dataRed0, dataRed3))
        try(rm(dataRed0, dataRed1, dataRed2, dataRed3))
        try(dataRed <- dataRed %>% as.data.frame())
        
        #this combines all of the rows from bolth red and blue and makes one globar var
        #sets it as a dataframe and arranges it by team # and match #
        try(allData <- rbind(dataBlue, dataRed))
        try(allData <- as.data.frame(lapply(allData, unlist)))
        try(allData <-
          allData %>% plyr::arrange(`Team..`, `Match..`))
        
        ###print(allData)
        
        #This is where the input functions are called and assings them to a global var
        #to be used to filter data sets and to be sent to charts to display them.
        try(dataSetList2 <- input$displayTeams2)
        try(dataTypes2 <- input$dataTypesSingle)
        try(dataTypes4388 <- input$dataTypesSingle4388)
        try(displayData2 <- allData[allData$Team.. == dataSetList2, ])
        try(displayData4388 <- allData[allData$Team.. == "4388", ])
        
        #This is where the data is being filtered based on the slicer selection
        #on the single team data page, and this is the data set that is being displayed
        try(graphData <-
          displayData2 %>% select(dataTypes2 , Match.. , Team..))
        try(graphData <- meltData(graphData, dataTypes2))
        
        #This is where the data is being filtered based on the slicer selection
        #on the dashboard page, and creates the display data set
        try(graphData4388 <-
          displayData4388 %>% select(dataTypes4388, Match.., Team..))
        try(graphData4388 <- meltData(graphData4388, dataTypes4388))
        
        #this creates the climb data for both the single team metrics and dashboard pages
        try(climbData <-
          displayData2 %>% select(Climb , Match.. , Team..))
        try(climbData4388 <-
          displayData4388 %>% select(Climb, Match.., Team..))
        
        #Melts the data sets for final display
        try(climbData <- meltData(climbData, "Climb"))
        try(climbData$Avg <- mean(as.integer(climbData$Climb)))
        try(climbData4388 <- meltData(climbData4388, "Climb"))
        try(climbData4388$Avg <- mean(as.integer(climbData4388$Climb)))
        
        #filters the data to be displayed in a rHandsonTable on single team metrics page
        try(qualitiaveOut <-
          displayData2 %>% select(
            Team..,
            Match..,
            They.Did.Well,
            They.Struggled.With,
            They.Cant.Do,
            Yellow.Card,
            Red.Card,
            Disabled,
            Flipped.Over,
            Assisted.Climb,
            Center.Climb,
            Climb,
            Trench.Run
          ))
        
        #filters the data to be diplayed in a rHandsonTable on the dashboard page
        try(qualitiaveOut4388 <-
          displayData4388 %>% select(
            Team..,
            Match..,
            They.Did.Well,
            They.Struggled.With,
            They.Cant.Do,
            Yellow.Card,
            Red.Card,
            Disabled,
            Flipped.Over,
            Assisted.Climb,
            Center.Climb,
            Climb,
            Trench.Run
          ))
        
        #outputs the chart for the single team metrics page
        output$singleTeam <- renderPlotly({
          ggplot(data = graphData, aes(
            x = Match..,
            y = graphData[[dataTypes2]],
            color = L1
          )) + geom_point() + stat_smooth(method = "loess", formula = y ~ x) + geom_quantile(quantiles = 0.5)
        })
        
        #outputs the chart for the dashboard page
        output$singleTeam4388 <- renderPlotly({
          ggplot(data = graphData4388, aes(
            x = Match..,
            y = graphData4388[[dataTypes4388]],
            color = L1
          )) + geom_point() + stat_smooth(method = "loess", formula = y ~ x) + geom_quantile(quantiles = 0.5)
        })
        
        #outputs the qualitiative data for the single team metrics page
        output$Qualitative1 <- renderRHandsontable({
          rhandsontable(qualitiaveOut)
        })
        
        #outputs the qualitative data for the dashboard page
        output$Qualitative14388 <- renderRHandsontable({
          rhandsontable(qualitiaveOut4388)
        })
        
        #outputs the gauge for the single team metrics page
        output$climbProbability <- renderPlotly({
          layout(
            plot_ly(
              domain = list(x = c(0 , 1), y = c(0, 1)),
              value = climbData$Avg,
              title = list(text = "Climb Probability"),
              type = "indicator",
              mode = "gauge+number",
              gauge = list(
                axis = list(range = list(NULL, 1)),
                threshold = list(
                  line = list(color = "red", width = 4),
                  thickness = 0.75,
                  value = 0.75
                )
              )
            )
            ,
            margin = list(l = 20, r = 30)
          )
        })
        
        #outputs the gauge for the dashboard page 
        output$climbProbability4388 <- renderPlotly({
          layout(
            plot_ly(
              domain = list(x = c(0 , 1), y = c(0, 1)),
              value = climbData4388$Avg,
              title = list(text = "Climb Probability"),
              type = "indicator",
              mode = "gauge+number",
              gauge = list(
                axis = list(range = list(NULL, 1)),
                threshold = list(
                  line = list(color = "red", width = 4),
                  thickness = 0.75,
                  value = 0.75
                )
              )
            )
            ,
            margin = list(l = 20, r = 30)
          )
        })
        
        #summarizes all of the data for every team to display in a heatmap on the
        #all competiton comparison page
        try(allTeamsInverse <- ddply(
          allData,
          .(Team..),
          summarise,
          Red.Card = sum(as.integer(as.logical(Red.Card)), na.rm = TRUE),
          Yellow.Card = sum(as.integer(as.logical(Yellow.Card)), na.rm = TRUE),
          Fouls = sum(Fouls, na.rm = TRUE),
          Tech.Fouls = sum(Tech.Fouls, na.rm = TRUE),
          Disabled = sum(Disabled, na.rm = TRUE),
          Flipped.Over = sum(as.integer(as.logical(Flipped.Over)), na.rm = TRUE),
          Inner.Port = -1*sum(Inner.Port, na.rm = TRUE),
          Outer.Port = -1*sum(Outer.Port, na.rm = TRUE),
          Lower.Port = -1*sum(Lower.Port, na.rm = TRUE),
          Assisted.Climb = -1*sum(as.integer(as.logical(Assisted.Climb)), na.rm = TRUE),
          Center.Climb = -1*sum(as.integer(as.logical(Center.Climb)), na.rm = TRUE),
          Leveling.Climb = -1*sum(as.integer(as.logical(Leveling.Climb)), na.rm = TRUE),
          Climb = -1*sum(as.integer(as.logical(Climb)), na.rm = TRUE),
          Trench.Run = -1*sum(as.integer(as.logical(Trench.Run)), na.rm = TRUE),
          Control.Panel = -1*sum(Control.Panel, na.rm = TRUE),
          number = length(Team..)
        ))
        
        #summarizes all of the data for every team to display in the parallel coordinate
        #chart currently on the TEST PAGE
        try(allTeams2 <- ddply(
          allData,
          .(Team..),
          summarise,
          Red.Card = -1*mean(.na.to.snglcode(as.logical(Red.Card), FALSE), na.rm = TRUE),
          Yellow.Card = -1*mean(.na.to.snglcode(as.logical(Yellow.Card), FALSE), na.rm = TRUE),
          Fouls = -1*mean(Fouls, na.rm = TRUE),
          Tech.Fouls = -1*mean(Tech.Fouls, na.rm = TRUE),
          Disabled = -1*mean(.na.to.snglcode(as.logical(Disabled), FALSE), na.rm = TRUE),
          Flipped.Over = -1*mean(.na.to.snglcode(as.logical(Flipped.Over), FALSE), na.rm = TRUE),
          Inner.Port = mean(Inner.Port, na.rm = TRUE),
          Outer.Port = mean(Outer.Port, na.rm = TRUE),
          Lower.Port = mean(Lower.Port, na.rm = TRUE),
          Assisted.Climb = mean(.na.to.snglcode(as.logical(Assisted.Climb), FALSE), na.rm = TRUE),
          Center.Climb = mean(.na.to.snglcode(as.logical(Center.Climb), FALSE), na.rm = TRUE),
          Leveling.Climb = mean(.na.to.snglcode(as.logical(Leveling.Climb), FALSE), na.rm = TRUE),
          Climb = mean(.na.to.snglcode(as.logical(Climb), FALSE), na.rm = TRUE),
          Trench.Run = mean(.na.to.snglcode(as.logical(Trench.Run), FALSE), na.rm = TRUE),
          Control.Panel = mean(Control.Panel, na.rm = TRUE),
          number = length(Team..)
        ))
        

        
        #creates a mean for each of the data points in the allTeams data set
        try(allTeamsAVG <- ddply(
          allData,
          .(Team..),
          summarize,
          Red.Card = mean(.na.to.snglcode(as.logical(Red.Card), FALSE), na.rm = FALSE),
          Yellow.Card = mean(.na.to.snglcode(as.logical(Yellow.Card), FALSE), na.rm = TRUE),
          Fouls = mean((Fouls), na.rm = TRUE),
          Tech.Fouls = mean((Tech.Fouls), na.rm = TRUE),
          Disabled = mean(.na.to.snglcode(as.logical(Disabled), FALSE), na.rm = TRUE),
          Flipped.Over = mean(.na.to.snglcode(as.logical(Flipped.Over), FALSE), na.rm = TRUE),
          Inner.Port = mean(Inner.Port, na.rm = TRUE),
          Outer.Port = mean(Outer.Port, na.rm = TRUE),
          Lower.Port = mean(Lower.Port, na.rm = TRUE),
          Assisted.Climb = mean(.na.to.snglcode(as.logical(Assisted.Climb), FALSE), na.rm = TRUE),
          Center.Climb = mean(.na.to.snglcode(as.logical(Center.Climb), FALSE), na.rm = TRUE),
          Leveling.Climb = mean(.na.to.snglcode(as.logical(Leveling.Climb), FALSE), na.rm = TRUE),
          Climb = mean(.na.to.snglcode(as.logical(Climb), FALSE), na.rm = TRUE),
          Trench.Run = mean(.na.to.snglcode(as.logical(Trench.Run), FALSE), na.rm = TRUE),
          Control.Panel = mean(Control.Panel, na.rm = TRUE),
          number = length(Team..)
        )) 

        
        #Observes the team slicer on the side bar to allow for filtering of data on the
        #parallel coordianate chart currently on the TEST PAGE
      observeEvent(input$teamSlicer, {
          
          #input var
          teamsSelected <- input$teamSlicer
          #filters based on input var for both allTeams data and for allTeamsAVG data
          
          teamSelectedData <- allTeams2[allTeams2$Team.. %in% teamsSelected, ]
          allTeamsAVGSelected <- allTeamsAVG[teamsSelected %in% allTeamsAVG$Team.., ]
          
          #outputs the parallel coordianate chart
          output$testParallel <- renderPlotly({
            GGally::ggparcoord(teamSelectedData
                       ,columns = c(2:7, 9:11, 14)
                       ,groupColumn = 1
                       ,order = "skewness"
                       ,scale = "std"
                      )
          })
          
          #function in use to create AVG value boxes for display on team comparison page
          #' valueBoxOut
          #'
          #' @param index : The variable from UI to output to
          #' @param col : The column in the data to be selected
          #' @param row : The row from the data to be selected
          #' @param subtitle : The string to be pasted infront of team number on card
          #'
          #' @return
          #' @export
          #'
          #' @examples
          valueBoxOut <- function(index, col, row, subtitle){
            output[[index]] <- renderValueBox({
              valueBox(
                allTeamsAVGSelected[row, col]
                ,subtitle = paste(subtitle, allTeamsAVGSelected[row, 1])
                ,color = "light-blue"
              )
            })
          }
          
          #Climb probablity section for value boxes on the comparison page
          valueBoxOut("values1", 14, 1, "Climb Probablity")
          valueBoxOut("values2", 14, 2, "Climb Probablity")
          valueBoxOut("values3", 14, 3, "Climb Probablity")
          valueBoxOut("values4", 14, 4, "Climb Probablity")
          valueBoxOut("values5", 14, 5, "Climb Probablity")
          valueBoxOut("values6", 14, 6, "Climb Probablity")
          
          #Foul AVG value boxes for display on the comparison page
          valueBoxOut("foulAvg1", 4, 1, "Foul Average")
          valueBoxOut("foulAvg2", 4, 2, "Foul Average")
          valueBoxOut("foulAvg3", 4, 3, "Foul Average")
          valueBoxOut("foulAvg4", 4, 4, "Foul Average")
          valueBoxOut("foulAvg5", 4, 5, "Foul Average")
          valueBoxOut("foulAvg6", 4, 6, "Foul Average")
      })  
        
        #outputs the heatmap rHandsontable for the entire compeition comparison page
        output$allTeamData <- renderRHandsontable({
          rhandsontable(allTeamsInverse, readOnly = TRUE, height = 600) %>%
            hot_cols(
              columnSorting = TRUE,
              highlightCol = TRUE,
              highlightRow = TRUE
            ) %>% hot_heatmap(color_scale = c("#17F556", "#ED6D47"))
        })
      }) 
    })
  })
  
  #trys to read data in a second time
  try(dataBlue1 <-
        read_json(paste(getwd(), "/dataOutBlue1.json", sep = ""),
                  simplifyVector = TRUE))
  try(dataBlue2 <-
        read_json(paste(getwd(), "/dataOutBlue2.json", sep = ""),
                  simplifyVector = TRUE))
  try(dataBlue3 <-
        read_json(paste(getwd(), "/dataOutBlue3.json", sep = ""),
                  simplifyVector = TRUE))
  try(dataRed1 <-
        read_json(paste(getwd(), "/dataOutRed1.json", sep = ""), simplifyVector = TRUE))
  try(dataRed2 <-
        read_json(paste(getwd(), "/dataOutRed2.json", sep = ""), simplifyVector = TRUE))
  try(dataRed3 <-
        read_json(paste(getwd(), "/dataOutRed3.json", sep = ""), simplifyVector = TRUE))
  
  #observes the scout num selector, to display right data set for input
  #as well as save the data to the right JSON file.
  observeEvent(input$scoutNum, {
    
    #lots of if statments for logic for the save function
    if (input$scoutNum == 1) {
      save <- "save1"
      ###print(save)
      DF1 <-
        as.data.table(DF1)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF1$`Team #` <- apiCallDF$`alliances.blue.team_keys_1`
      
      try(DF1 <-
            read_json(paste(getwd(), "/dataOutBlue1.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF1 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF1"]]))
            DF1 <- DF1
          else
            DF1 <- values[["DF1"]]
        }
        values[["DF1"]] <- DF1
      })
      
      output$hot <- renderRHandsontable({
        DF1 <- DF1
        if (!is.null(DF1))
          rhandsontable(
            DF1,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 1) {
            finalDF1 <- isolate(values[["DF1"]])
            write_json(finalDF1,
                       paste(getwd(), "/dataOutBlue1.json", sep = ""))
          }
        })
      })
    } else if (input$scoutNum == 2) {
      save <- "save2"
      ###print(save)
      DF2 <-
        as.data.table(DF2)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF2$`Team #` <- apiCallDF$`alliances.blue.team_keys_2`
      
      try(DF2 <-
            read_json(paste(getwd(), "/dataOutBlue2.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF2 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF2"]]))
            DF2 <- DF2
          else
            DF2 <- values[["DF2"]]
        }
        values[["DF2"]] <- DF2
      })
      
      output$hot <- renderRHandsontable({
        DF2 <- DF2
        if (!is.null(DF2))
          rhandsontable(
            DF2,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 2) {
            finalDF2 <- isolate(values[["DF2"]])
            write_json(finalDF2,
                       paste(getwd(), "/dataOutBlue2.json", sep = ""))
          }
        })
      })
    } else if (input$scoutNum == 3) {
      save <- "save3"
      ####print(save)
      DF3 <-
        as.data.table(DF3)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF3$`Team #` <- apiCallDF$`alliances.blue.team_keys_3`
      
      try(DF3 <-
            read_json(paste(getwd(), "/dataOutBlue3.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF3 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF3"]]))
            DF3 <- DF3
          else
            DF3 <- values[["DF3"]]
        }
        values[["DF3"]] <- DF3
      })
      
      output$hot <- renderRHandsontable({
        DF3 <- DF3
        if (!is.null(DF3))
          rhandsontable(
            DF3,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 3) {
            finalDF3 <- isolate(values[["DF3"]])
            write_json(finalDF3,
                       paste(getwd(), "/dataOutBlue3.json", sep = ""))
          }
        })
      })
    } else if (input$scoutNum == 4) {
      save <- "save4"
      ###print(save)
      DF4 <-
        as.data.table(DF4)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF4$`Team #` <- apiCallDF$`alliances.red.team_keys_1`
      
      try(DF4 <-
            read_json(paste(getwd(), "/dataOutRed1.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF4 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF4"]]))
            DF4 <- DF4
          else
            DF4 <- values[["DF4"]]
        }
        values[["DF4"]] <- DF4
      })
      
      output$hot <- renderRHandsontable({
        DF4 <- DF4
        if (!is.null(DF4))
          rhandsontable(
            DF4,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 4) {
            finalDF4 <- isolate(values[["DF4"]])
            write_json(finalDF4,
                       paste(getwd(), "/dataOutRed1.json", sep = ""))
          }
        })
      })
    } else if (input$scoutNum == 5) {
      save <- "save5"
      ###print(save)
      DF5 <-
        as.data.table(DF5)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF5$`Team #` <- apiCallDF$`alliances.red.team_keys_2`
      
      try(DF5 <-
            read_json(paste(getwd(), "/dataOutRed2.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF5 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF5"]]))
            DF5 <- DF5
          else
            DF5 <- values[["DF5"]]
        }
        values[["DF5"]] <- DF5
      })
      
      output$hot <- renderRHandsontable({
        DF5 <- DF5
        if (!is.null(DF5))
          rhandsontable(
            DF5,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 5) {
            finalDF5 <- isolate(values[["DF5"]])
            write_json(finalDF5,
                       paste(getwd(), "/dataOutRed2.json", sep = ""))
          }
        })
      })
    } else if (input$scoutNum == 6) {
      save <- "save6"
      ##print(save)
      DF6 <-
        as.data.table(DF6)[, lapply(.SD, `length<-`, nrow(apiCallDF)), by = `Team #`]
      DF6$`Team #` <- apiCallDF$`alliances.red.team_keys_3`
      
      try(DF6 <-
            read_json(paste(getwd(), "/dataOutRed3.json", sep = ""),
                      simplifyVector = TRUE))
      
      observe({
        if (!is.null(input$hot)) {
          DF6 = hot_to_r(input$hot)
        } else {
          if (is.null(values[["DF6"]]))
            DF6 <- DF6
          else
            DF6 <- values[["DF6"]]
        }
        values[["DF6"]] <- DF6
      })
      
      output$hot <- renderRHandsontable({
        DF6 <- DF6
        if (!is.null(DF6))
          rhandsontable(
            DF6,
            useTypes = as.logical(input$useType),
            stretchH = "all",
            height = 600
          )
      })
      
      ## Save
      observeEvent(input$scoutNum, {
        observeEvent(input$save, {
          if (input$scoutNum == 6) {
            finalDF6 <- isolate(values[["DF6"]])
            write_json(finalDF6,
                       paste(getwd(), "/dataOutRed3.json", sep = ""))
          }
        })
      })
    }
  })
}) 

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