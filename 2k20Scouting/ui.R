#########################################################################################
#                                     Leo Blakely                                       #
#                                   Modified: 2.1.2020                                  # 
#                                 2020 Robot Scouting                                   #
#   ______  _____    _____   _______                        _  _   ____    ___    ___   #
#  | ____| |  __ \  / ____| |__   __|                      | || | |___ \  / _ \  / _ \  #
#  | |__   | |__) || |         | |  ___   __ _  _ __ ___   | || |_  __) || (_) || (_) | #
#  |  __|  |  _  / | |         | | / _ \ / _` || '_ ` _ \  |__   _||__ <  > _ <  > _ <  #
#  | |     | | \ \ | |____     | ||  __/| (_| || | | | | |    | |  ___) || (_) || (_) | #
#  |_|     |_|  \_\ \_____|    |_| \___| \__,_||_| |_| |_|    |_| |____/  \___/  \___/  #
#                                                                                       #
# Ui.R                                                                                  #
# This file contains all nesscary code to display the ui of the shinydashboard          #
# This is half of the reactive R code to display reactive values                        #
#########################################################################################

#header contains all parts of the dashboard that are in the header bar.
#This includes the title of the app and controls for the sidebar menu
header <-
  dashboardHeader(title = "2020 Robot Scouting"
                  ,titleWidth = 300
                  #dropdownMenuOutput("messageMenu"),
                  #dropdownMenuOutput("tasks")
                  )

#sidebar contains all items that are on the sidebar menu, and is used to display the
#menu for navigation.
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
  
  #Each menu item is a tab that can be used to select a tab in the main body of the
  #dashboard.
  menuItem(
    "Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem(
    "One Team Metrics",
    tabName = "singleTeam",
    icon = icon('chart-line')
  ),
  menuItem(
    "Team Comparison"
    ,tabName = 'testPage'
    ,icon = icon('not-equal')
  ),
  menuItem(
    "All Competion Comparison",
    tabName = 'allTeam',
    icon = icon('chart-bar')
  ),
  menuItem(
    "Data Input",
    tabName = 'datainput',
    icon = icon('table')
  ),
  menuItem(
    "Settings",
    tabName = 'slicers',
    icon = icon('filter')
  ),
  #This checkboxGroup is the main slicer for the muti team comparison.
  checkboxGroupInput(
    "teamSlicer"
    ,label = "Team Slicer"
    ,sort(listOfTeams$Team..)
  )
))

#body contains all of the items required to display the main body of the app
#this includes all visualizations of the data, and data input.
#The order in this section does not matter, except for organizations sake.
body <- dashboardBody(
  tabItems(
  
  #testpage is only used for testing items, and is normaly not shown on the sidebar menu
  tabItem(tabName = 'testPage'
    ,fluidRow(
      box(
        width = 12
        ,valueBoxOutput('values1')
        ,valueBoxOutput('values2')
        ,valueBoxOutput('values3')
        ,valueBoxOutput('values4')
        ,valueBoxOutput('values5')
        ,valueBoxOutput('values6')
      )
    )
    ,fluidRow(
      box(
      width= 12
      ,plotlyOutput('testParallel')
      )
    ) 
    ,fluidRow(
      box(
        width = 12
        ,valueBoxOutput('foulAvg1')
        ,valueBoxOutput('foulAvg2')
        ,valueBoxOutput('foulAvg3')
        ,valueBoxOutput('foulAvg4')
        ,valueBoxOutput('foulAvg5')
        ,valueBoxOutput('foulAvg6')
      )
    )
    
  ),
  
  #allTeam is used to display the entire competion comparison
  tabItem(tabName = 'allTeam',
          fluidRow(box(
            width = 12
            , rHandsontableOutput("allTeamData")
          ))),
  
  #The dashboard page is used to display our teams data thruought the competition.
  tabItem(tabName = 'dashboard',
          fluidRow(
            box(
              width = 8
              ,
              
              #each output represents a visuization that is being displayed to the 
              #end user of the app.
              plotlyOutput("singleTeam4388")
              ,
              selectInput(
                "dataTypesSingle4388",
                "Data Types",
                c(
                  "Fouls",
                  "Inner.Port",
                  "Outer.Port",
                  "Lower.Port",
                  "Control.Panel"
                )
              )
              ,
              selectInput("displayTeams24388", label = "Team", choices = "4388")
            ),
            box(width = 4
                ,plotlyOutput("climbProbability4388")),
            fluidRow(box(
              width = 12
              , rHandsontableOutput("Qualitative14388")
            ))
          )),
  
  #the data input page is where scouts input data to the app
  tabItem(
    tabName = 'datainput',
    box(
      width = 12,
      solidHeader = TRUE,
      titlePanel("Edit and save a table"),
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Shiny app based on an example given in the rhandsontable package.",
            "Right-click on the table to delete/insert rows.",
            "Double-click on a cell to edit"
          ),
          
          wellPanel(
            h3("Table options"),
            radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
          ),
          br(),
          
          wellPanel(h3("Save"),
                    actionButton("save", "Save table"))
          
        ),
        
        mainPanel(rHandsontableOutput("hot"))
      )
    )
  ),
  
  #single team page displays one team at a time
  tabItem(tabName = "singleTeam",
          fluidRow(
            box(
              width = 8
              ,
              plotlyOutput("singleTeam")
              ,
              selectInput(
                "dataTypesSingle",
                "Data Types",
                c(
                  "Fouls",
                  "Inner.Port",
                  "Outer.Port",
                  "Lower.Port",
                  "Control.Panel"
                )
              )
              ,
              selectInput("displayTeams2", label = "Team", choices = sort(listOfTeams$Team..))
            ),
            box(width = 4
                , plotlyOutput("climbProbability")),
            fluidRow(box(
              width = 12
              , rHandsontableOutput("Qualitative1")
            ))
          )),
  
  #slicers page only contains the scout number selector
  tabItem(tabName = "slicers",
          fluidRow(
            box(
              width = 2,
              title = 'Settings',
              radioButtons("scoutNum", "Scout Number", c("1", "2", "3", "4", "5", "6"))
            )
          ))
))


#this is what displays the dashboard page
shinyUI(bootstrapPage(
  #theme = "www/styleing.css",
  includeScript("www/message.js")
  ,
  dashboardPage(skin = "green",
                header, sidebar, body,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styleing.css"),
      tags$style(
        HTML('.wrapper {
            height: auto !important; 
            position:relative; 
            overflow-y:hidden}')
      )
      
    )
  ),
  textOutput("text")
))

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