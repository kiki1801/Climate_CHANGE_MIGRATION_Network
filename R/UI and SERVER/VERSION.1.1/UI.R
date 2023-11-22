##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-11-14                   #####
#####                        Mod.: 2023-11-20                        #####
#####                               UI                               #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)
library(shinyjs)
library(leaflet)
library(plotly)

##############
##### UI #####
##############

ui <- fluidPage(
  
  useShinyjs(),
  
  navbarPage(
    title = "Climate Change Migration Network",
    
    tabPanel(
      title = "Migration",
      tabsetPanel(
        tabPanel(
          title = "Residence",
          tags$div(
            style = "margin-top: 10px;",
            actionButton("mr_countries", "Countries"),
            actionButton("mr_regions", "Regions")
            ),
          conditionalPanel(
            condition = "input.mr_countries >= 1",
            style = "margin-top: 10px;"
            ),
          conditionalPanel(
            condition = "input.mr_regions >= 1",
            style = "margin-top: 10px;",
            radioButtons(
              inputId = "mrr_region_choice",
              label = NULL,
              choices = c(
                "Continental Regions",
                "Continental Sub-Regions",
                "Geographic Regions (SDG)",
                "Income Levels"
                ),
              selected = "Continental Regions",
              inline = TRUE
              )
            ),
          leafletOutput("mr_world_map", width = "100%", height = 475), 
          tags$div(style = "margin-top: 10px;"),
          htmlOutput("mr_selected_residence"),
          uiOutput("mr_birth_view_choices"),
          tags$div(style = "margin-top: 10px;"),
          uiOutput("mr_birth_view_region_choice"),
          tags$div(style = "margin-top: 10px;"),
          plotlyOutput("mr_stacked_barplot_share", width = "100%", height = "400px")
          ),
        tabPanel(
          title = "Birth",
          tags$div(
            style = "margin-top: 10px;",
            actionButton("mb_countries", "Countries"),
            actionButton("mb_regions", "Regions")
            )
          )
        )
      ), 
    
    tabPanel(
      title = "Climate",
      ),
    
    tabPanel(
      title = "Migration and Climate", 
      tabsetPanel(
        tabPanel(
          title = "Residence",
          tags$div(
            style = "margin-top: 10px;",
            actionButton("mcr_countries", "Countries"),
            actionButton("mcr_regions", "Regions")
            )
          ), 
        tabPanel(
          title = "Birth",
          tags$div(
            style = "margin-top: 10px;",
            actionButton("mcb_countries", "Countries"),
            actionButton("mcb_regions", "Regions")
            )
          )
        )
      )
    
    )
  )