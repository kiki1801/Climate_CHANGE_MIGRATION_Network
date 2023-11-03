##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-10-24                   #####
#####                               UI                               #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)
library(shinydashboard)
library(leaflet)

##############
##### UI #####
##############

##### fluidPage() #####

uiFPC <- fluidPage(
  titlePanel("Climate Change Migration Network"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "area", "Classification:",
                  choices = c("Continental Regions",
                              "Geographic Regions (SDG)",
                              "Development Levels",
                              "Income Levels"),
                  selected = "Continental Regions", multiple = FALSE)
    ),
    mainPanel(plotOutput("world_map"))
      )
  )

uiFPI <- fluidPage(
  titlePanel("Climate Change Migration Network"),
  plotOutput("world_map"), 
  fluidRow(
    column(width = 6, plotOutput("share_stacked_barplot")),
    column(width = 6, plotOutput("nb_stacked_barplot"))
  )
)

uiFPNAV <- fluidPage(
  navbarPage(
    title = "Climate Change Migration Network", 
    tabPanel("Migration",
             tabsetPanel(
               tabPanel("Residence",
                        tags$div(
                          style = "margin-top: 10px;",
                          actionButton("countries", "Countries"),
                          actionButton("regions", "Regions"),
                          tags$div(HTML("<br>")),
                          leafletOutput("world_map", width = "100%", height = 400), 
                          tags$div(style = "height: 10px;"),
                          textOutput("selected_residence")
                          )
                        ),
               tabPanel("Birth",
                        tags$div(
                          style = "margin-top: 10px;",
                          actionButton("countries", "Countries"),
                          actionButton("regions", "Regions")
                          )
                        ),
               )
             ),
    tabPanel("Climate"),
    tabPanel("Migration and Climate",
             tabsetPanel(
               tabPanel("Residence",
                        tags$div(
                          style = "margin-top: 10px;",
                          actionButton("countries", "Countries"),
                          actionButton("regions", "Regions")
                          )
                        ),
               tabPanel("Birth",
                        tags$div(
                          style = "margin-top: 10px;",
                          actionButton("countries", "Countries"),
                          actionButton("regions", "Regions")
                          )
                        ),
               )
             )
    ),
  )

##### navbarPage() #####

uiNBP <- navbarPage(
  title = "Climate Change Migration Network",
  tabPanel("Residence",
           fluidRow(
             column(3, selectInput(inputId = "area", "Classification:",
                                   choices = c("Continental Regions",
                                               "Geographic Regions (SDG)",
                                               "Development Levels",
                                               "Income Levels"),
                                   selected = "Continental Regions", multiple = FALSE)),
             column(9, plotOutput("stacked_barplot"))
           )
  ),
  tabPanel("Birth",
           fluidRow(
             column(3, selectInput(inputId = "area", "Classification:",
                                   choices = c("Continental Regions",
                                               "Geographic Regions (SDG)",
                                               "Development Levels",
                                               "Income Levels"),
                                   selected = "Continental Regions", multiple = FALSE)),
             column(9, plotOutput("stacked_barplot"))
           )
  )
)

##### dashboardPage() #####

uiDBP <- dashboardPage(
  dashboardHeader(title = "Climate Change Migration Network"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphics", tabName = "Graph", icon = icon("chart-line")),
      menuItem("Table", tabName = "Table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Graph",
              fluidRow(
                box(plotOutput("Map")),
                box(plotOutput("Graphic"))
              )
      ),
      tabItem("Table",
              dataTableOutput("Table")
      )
    )
  )
)