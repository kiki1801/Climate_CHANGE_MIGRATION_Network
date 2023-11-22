##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-11-14                   #####
#####                        Mod.: 2023-11-20                        #####
#####                             SERVER                             #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)
library(shinyjs)
library(leaflet)
library(plotly)

#####################
##### Load DATA #####
#####################

source("DATA.R")

##################
##### SERVER #####
##################

server <- function(input, output, session) {
  
  useShinyjs()
  
  RV <- reactiveValues(
    TilesLoaded = FALSE, 
    SSR = FALSE, 
    MRAction = NULL, 
    BVC = FALSE,
    BVRC = FALSE,
    )

  observeEvent(input$mr_countries, {
    
    shinyjs::hide("mrr_region_choice")
    
    RV$SSR <- FALSE
    
    RV$MRAction <- "COUNTRIES"
    
    RV$BVC <- FALSE
    
    RV$BVRC <- FALSE
    
    if (!RV$TilesLoaded) {
      output$mr_world_map <- renderLeaflet({
        leaflet() %>%
          addTiles(
            attribution = HTML(
              "© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>,
              <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> |
              <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> |
              <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>"
              ), 
            layerId = "OSM", group = "OSM",
            options = tileOptions(
              minZoom = 1,
              maxZoom = 10,
              maxNativeZoom = 18
              )
            ) %>%
          addScaleBar(position = "bottomleft")

        })
      RV$TilesLoaded <- TRUE
      }
    
    leafletProxy(mapId = "mr_world_map", session) %>%
      clearShapes() %>% 
      clearControls() %>%
      addPolygons(
        data = GeoDATA.WGS84, layerId = ~Residence,
        stroke = TRUE, color = "transparent", weight = 1.5, opacity = 0.75,
        fill = TRUE, fillOpacity = 0.25,
        smoothFactor = 1,
        label = ~lapply(Label, HTML), 
        highlightOptions = highlightOptions(
          stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
          fill = TRUE, fillColor = "#0000FF", fillOpacity = 0.25
          )
        )
    
    })

  observeEvent(input$mr_regions, {
    
    shinyjs::show("mrr_region_choice")
    
    RV$SSR <- FALSE
    
    RV$MRAction <- "REGIONS"
    
    RV$BVC <- FALSE
    
    RV$BVRC <- FALSE
    
    if (!RV$TilesLoaded) {
      output$mr_world_map <- renderLeaflet({
        leaflet() %>%
          addTiles(
            attribution = HTML(
              "© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>,
              <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> |
              <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> |
              <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>"
              ), 
            layerId = "OSM", group = "OSM",
            options = tileOptions(
              minZoom = 1,
              maxZoom = 10,
              maxNativeZoom = 18
              )
            ) %>%
          addScaleBar(position = "bottomleft")
        })
      RV$TilesLoaded <- TRUE
      }
    
    observeEvent(input$mrr_region_choice, {
      
      RV$SSR <- FALSE
      
      RV$BVC <- FALSE
      
      RV$BVRC <- FALSE
      
      SelectedRn <- input$mrr_region_choice
      if (SelectedRn == "Continental Regions") {
        GeoRDATA <- GeoRDATA.CRs.WGS84
        LResidence <- GeoRDATA$Residence
        DOMAIN <- GeoRDATA$Residence
        LNTI <- "Continental Region"
        LAYER <- "CRs"
        }
      else if (SelectedRn == "Continental Sub-Regions") {
        GeoRDATA <- GeoRDATA.CSRs.WGS84
        LResidence <- GeoRDATA$SIRegion
        DOMAIN <- GeoRDATA$SIRegion
        LNTI <- "Continental Sub-Region"
        LAYER <- "CSRs"
        }
      else if (SelectedRn == "Geographic Regions (SDG)") {
        GeoRDATA <- GeoRDATA.GRSDG.WGS84
        LResidence <- GeoRDATA$ClasseSDG
        DOMAIN <- GeoRDATA$ClasseSDG
        LNTI <- "SDG Classification"
        LAYER <- "GRSDG"
        }
      else if (SelectedRn == "Income Levels") {
        GeoRDATA <- GeoRDATA.IncomeLevel.WGS84
        LResidence <- GeoRDATA$Residence
        DOMAIN <- GeoRDATA$Residence
        LNTI <- "Income Level"
        LAYER <- "IncomeLevel"
        }
      
      fCLR <- colorFactor(GeoRDATA$CLRCode[order(factor(DOMAIN, levels = levels(DOMAIN)))], DOMAIN, na.color = "#999999")
      
      leafletProxy(mapId = "mr_world_map", session) %>%
        clearShapes() %>% 
        clearControls() %>%
        addPolygons(
          data = GeoRDATA, layerId = LResidence,
          stroke = TRUE, color = "#CCCCCC", weight = 0.5, opacity = 0.5,
          fill = TRUE, fillColor = fCLR(DOMAIN), fillOpacity = 0.75,
          smoothFactor = 1.25,
          label = ~lapply(LabelR, HTML), 
          highlightOptions = highlightOptions(
             stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
             fill = TRUE, fillColor = NULL, fillOpacity = 1
            )
          ) %>% 
        addLegend(
          position = "topright", pal = fCLR, values = DOMAIN, na.label = "Not Available",
          opacity = 1, title = LNTI, layerId = LAYER, group = "Regions"
          )

      })
    
    })
  
  SelectedResidence <- eventReactive(input$mr_world_map_shape_click, {
  
    ER <- input$mr_world_map_shape_click
    
    ClickedPOLYGON <- ER$id
    
    })
  
  observeEvent(input$mr_world_map_shape_click, {
    
    RV$SSR <- TRUE
    
    RV$BVC <- TRUE
    
    if (RV$MRAction == "COUNTRIES") {
      Geo <- GeoDATA.WGS84
      RESIDENCE <- Geo$Residence
      CLR <- "transparent"
      SWO <- c(1.5, 1.5, 0.75, 0.75)
      fillCLR <- c("transparent", "#0000FF")
      fillO <- c(0.25, 0.25)
      SMFCTR <- 1
      TXT <- Geo$Label
      hOfillCLR <- "#0000FF"
      hOfillO <- 0.25
      }
    else if (RV$MRAction == "REGIONS") {
      SelectedRn <- input$mrr_region_choice
      if (SelectedRn == "Continental Regions") {
        Geo <- GeoRDATA.CRs.WGS84
        RESIDENCE <- Geo$Residence
        }
      else if (SelectedRn == "Continental Sub-Regions") {
        Geo <- GeoRDATA.CSRs.WGS84
        RESIDENCE <- Geo$SIRegion
        }
      else if (SelectedRn == "Geographic Regions (SDG)") {
        Geo <- GeoRDATA.GRSDG.WGS84
        RESIDENCE <- Geo$ClasseSDG
        }
      else if (SelectedRn == "Income Levels") {
        Geo <- GeoRDATA.IncomeLevel.WGS84
        RESIDENCE <- Geo$Residence
        }
      CLR <- "#CCCCCC"
      SWO <- c(0.5, 1.5, 0.5, 0.75)
      fCLR <- colorFactor(Geo$CLRCode[order(factor(RESIDENCE, levels = levels(RESIDENCE)))], RESIDENCE, na.color = "#999999")
      fillCLR <- list(fCLR(RESIDENCE), fCLR(RESIDENCE))
      fillO <- c(0.75, 1)
      SMFCTR <- 1.25
      TXT <- Geo$LabelR
      hOfillCLR <- NULL
      hOfillO <- 1
      }
    
    leafletProxy(mapId = "mr_world_map", session) %>%
      addPolygons(data = Geo, layerId = RESIDENCE, 
                  stroke = TRUE, color = ifelse(RESIDENCE == SelectedResidence(), "#0000FF", CLR), 
                  weight = ifelse(RESIDENCE == SelectedResidence(), SWO[2], SWO[1]), 
                  opacity = ifelse(RESIDENCE == SelectedResidence(), SWO[4], SWO[3]),
                  fill = TRUE, fillColor = ifelse(RESIDENCE == SelectedResidence(), fillCLR[[2]], fillCLR[[1]]), 
                  fillOpacity = ifelse(RESIDENCE == SelectedResidence(), fillO[2], fillO[1]), 
                  smoothFactor = SMFCTR, 
                  label = lapply(TXT, HTML), 
                  highlightOptions = highlightOptions(
                    stroke = TRUE, color = "#0000FF", weight = 1.5, opacity = 0.75,
                    fill = TRUE, fillColor = hOfillCLR, fillOpacity = hOfillO
                    )
                  )
    
    })
  
  output$mr_selected_residence <- renderText({
    
    if (RV$SSR){
      SelectedR <- SelectedResidence()
      HTML(paste("<b>Selected Residence:</b>", SelectedR))
      }  
    
    })

  output$mr_birth_view_choices <- renderUI({
    
    if (RV$BVC){
      fluidRow(
        column(
          width = 3, 
          offset = 0,
          selectInput(
            inputId = "mr_birth_view_choice",
            label = "Birth View",
            choices = c("Countries", "Regions"),
            selected = "Countries",
            multiple = FALSE
            )
          ), 
        column(
          width = 3, 
          offset= 0, 
          sliderInput(
            inputId = "mr_decimal_choice", 
            label = "Decimal Choice", 
            min = 0,
            max = 5,
            value = 2,
            step = 1,
            ticks = FALSE
            )
          )
        )

      }
    
    })
  
  observeEvent(input$mr_birth_view_choice, {
    
    SelectedBvC <- input$mr_birth_view_choice
    
    if (SelectedBvC == "Regions") {RV$BVRC <- TRUE} else {RV$BVRC <- FALSE}
    
    output$mr_birth_view_region_choice <- renderUI({
      
      if (RV$BVRC) {
        radioButtons(
          inputId = "mr_birth_view_region_choice",
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
        }
      
      })
      
    })
  
  SelectedBvRC <- eventReactive(input$mr_birth_view_region_choice, {

    SBvRC <- input$mr_birth_view_region_choice

  })
  
  output$mr_stacked_barplot_share <- renderPlotly({
    
    if (RV$BVC) {
      SelectedR <- SelectedResidence()
      if (!RV$BVRC) {
        CountriesLVsCLR <- IMS.DATA %>% 
          distinct(Residence, ResidenceCode) %>% 
          filter(ResidenceCode < 900) %>%
          inner_join(
            DATA %>% 
              rename(ResidenceCC = Residence) %>% 
              select(ResidenceCC, ResidenceCode, CLRCode, ContinentalRegion), 
            by = "ResidenceCode"
            ) %>%
          arrange(ContinentalRegion, Residence) %>%
          add_row(
            Residence = "Other", 
            ResidenceCode = 2003, 
            ResidenceCC = "Other", 
            CLRCode = "#999999", 
            ContinentalRegion = NA
            ) %>%
          select(1:4)
        LVs <- CountriesLVsCLR$Residence
        CLR <- CountriesLVsCLR$CLRCode
        if (RV$MRAction == "COUNTRIES") {
          #COUNTRIEs-COUNTRIEs
          SelectedRCode <- GeoDATA.WGS84$ResidenceCode[which(GeoDATA.WGS84$Residence == SelectedR)]
          } else {
              #REGIONs-COUNTRIEs
              SelectedRn <- input$mrr_region_choice
              if (SelectedRn == "Continental Regions") {
                GeoRDATA <- GeoRDATA.CRs.WGS84
                RESIDENCE <- "Residence"
                }
              else if (SelectedRn == "Continental Sub-Regions") {
                GeoRDATA <- GeoRDATA.CSRs.WGS84
                RESIDENCE <- "SIRegion"
                }
              else if (SelectedRn == "Geographic Regions (SDG)") {
                GeoRDATA <- GeoRDATA.GRSDG.WGS84
                RESIDENCE <- "ClasseSDG"
                }
              else if (SelectedRn == "Income Levels") {
                GeoRDATA <- GeoRDATA.IncomeLevel.WGS84
                RESIDENCE <- "Residence"
                }
              SelectedRCode <- GeoRDATA$ResidenceCode[which(GeoRDATA[[RESIDENCE]] == SelectedR)]
              }
        IMS.FDATA <- IMS.DATA %>% 
          filter(
            ResidenceCode == SelectedRCode, 
            BirthCode < 900 | BirthCode == 2003,
            Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)
            ) %>%
          distinct(BirthCode, Year, .keep_all = TRUE)
        } else {
          if (SelectedBvRC() == "Continental Regions") {
            SBirthCodes = c(903, 935, 908, 904, 905, 909)
            LVs <- c(levels(GeoRDATA.CRs.WGS84$Residence), 'Other')
            CLR <- c(GeoRDATA.CRs.WGS84$CLRCode, "#999999")
            }
          else if (SelectedBvRC() == "Continental Sub-Regions") {
            SBirthCodes = c(927, 915, 916, 5500, 910, 906,
                            923, 928, 954, 911, 912, 905,
                            924, 957, 931, 920, 913, 5501,
                            925, 914, 922, 926)
            LVs <- c(levels(GeoRDATA.CSRs.WGS84$SIRegion), 'Other')
            LVs <- gsub("Polynesia", "Polynesia*", LVs)
            LVs <- gsub("Northern America", "NORTHERN AMERICA", LVs)
            CLR <- c(GeoRDATA.CSRs.WGS84$CLRCode[order(GeoRDATA.CSRs.WGS84$SIRegion)], "#999999")
            }
          else if (SelectedBvRC() == "Geographic Regions (SDG)") {
            SBirthCodes = c(927, 921, 1832, 1829, 1830, 1833, 1835, 947)
            LVs <- c(levels(GeoRDATA.GRSDG.WGS84$ClasseSDG), 'Other')
            LVs <- gsub("Oceania\\*", "Oceania (excluding Australia and New Zealand)", LVs)
            CLR <- c(GeoRDATA.GRSDG.WGS84$CLRCode, "#999999")
          }
          else if (SelectedBvRC() == "Income Levels") {
            SBirthCodes = c(1503, 1500, 1501, 1502)
            LVs <- c(levels(GeoRDATA.IncomeLevel.WGS84$Residence), 'Other')
            LVs <- LVs[LVs != 'Not Available']
            CLR <- c(GeoRDATA.IncomeLevel.WGS84$CLRCode)
            }
          if (RV$MRAction == "COUNTRIES") {
            #COUNTRIEs-REGIONs
            SelectedRCode <- GeoDATA.WGS84$ResidenceCode[which(GeoDATA.WGS84$Residence == SelectedR)]
            } else {
                #REGIONs-REGIONs
                SelectedRn <- input$mrr_region_choice
                if (SelectedRn == "Continental Regions") {
                  GeoRDATA <- GeoRDATA.CRs.WGS84
                  RESIDENCE <- "Residence"
                  }
                else if (SelectedRn == "Continental Sub-Regions") {
                  GeoRDATA <- GeoRDATA.CSRs.WGS84
                  RESIDENCE <- "SIRegion"
                  }
                else if (SelectedRn == "Geographic Regions (SDG)") {
                  GeoRDATA <- GeoRDATA.GRSDG.WGS84
                  RESIDENCE <- "ClasseSDG"
                  }
                else if (SelectedRn == "Income Levels") {
                  GeoRDATA <- GeoRDATA.IncomeLevel.WGS84
                  RESIDENCE <- "Residence"
                  }
                SelectedRCode <- GeoRDATA$ResidenceCode[which(GeoRDATA[[RESIDENCE]] == SelectedR)]
                } 
          IMS.FDATA <- IMS.DATA %>% 
            filter(
              ResidenceCode == SelectedRCode,
              BirthCode %in% SBirthCodes | BirthCode == 2003,
              Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)
              ) %>% 
            distinct(BirthCode, Year, .keep_all = TRUE)
          }
      IMS.FDATA <- IMS.FDATA %>%
        group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>%
        summarise(IMST, IMSM, IMSF, IMST_SHARE, IMSM_SHARE, IMSF_SHARE)
      
      IMS.FDATA$Birth <- factor(x = IMS.FDATA$Birth, levels = rev(LVs))
      
      DeciChoice <- input$mr_decimal_choice
      
      plot_ly(
        data = IMS.FDATA, x = ~Year, y = ~IMST_SHARE, 
        type = "bar", 
        color = ~Birth, colors = rev(CLR), 
        legendgroup = 'Birth',
        text = with(
          data = IMS.FDATA, 
          expr = paste(
            " Residence:", Residence, "<br>",
            "Birth:", Birth, "<br>",
            "Year:", Year, "<br>",
            "International Migrant Stock:", round(IMST_SHARE, DeciChoice), "%"
            )
          )
        ) %>%
        style(textposition = "none", hoverinfo = "text") %>%
        layout(
          barmode = "stack",
          title = paste("..."),
          yaxis = list(title = "International Migrant Stock (%)"),
          legend = list(title = list(text='<b> Birth </b>'), x=1, y=0.5)
          )
      }
    
    })
  
  }




