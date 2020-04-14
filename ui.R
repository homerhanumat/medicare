library(shinydashboard)
library(tidyverse)
#library(plotly)
library(leaflet)

load("data/DiagnosticRelatedGroups.Rda")
drg_descriptions <- 
  DiagnosticRelatedGroups %>% 
  pull(drgDefinition)

load("data/states_names.Rda")

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Medicare Services"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("County Table", tabName = "table", icon = icon("table")),
        menuItem("By-County Map", tabName = "plot", icon = icon("line-chart"))
      ),
      selectInput("year", "Year",
                  choices = c(
                    2011:2017,
                    "All Years"
                  ),
                  selected = "2011"),
      selectInput("state", "State",
                  choices = states_names$abbr,
                  selected = "KY"),
      selectInput("drg", "Diagnostic Related Group",
                  choices = drg_descriptions,
                  selected = drg_descriptions[1])
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "table",
          fluidRow(
            downloadButton('downloadData', 'Download CSV File'),
            br(),
            br(),
            DT::dataTableOutput("table")
          ) # end row
        ), # end table tab
        tabItem(
          tabName = "plot",
          fluidRow(
            htmlOutput("title"),
            #plotly::plotlyOutput("plot")
            leafletOutput("plot")
          ) # end row
        ) # end plot tab
      ) # end tabitems
    ) # end dashboard body
  ) # end dashboard page
) # end shinyUI
