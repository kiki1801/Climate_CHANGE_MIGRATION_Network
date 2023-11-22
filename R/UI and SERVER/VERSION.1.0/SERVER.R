##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-10-24                   #####
#####                        Mod.: 2023-11-14                        #####
#####                             SERVER                             #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)
library(shinyjs)
library(leaflet)
library(DescTools)
library(RColorBrewer)
library(plotly)

###################################
##### Load IMS.DATA | GeoDATA #####
###################################

source("DATA.R")

##################
##### SERVER #####
##################

serverWGS84 <- function(input, output, session) {
  useShinyjs()
  
  observeEvent(input$countries, {
    shinyjs::hide("region_choice")
    shinyjs::show("C.birth_view_choice")
    shinyjs::hide("R.birth_view_choice")
    shinyjs::show("C.birth_view_region_choice")
    shinyjs::hide("R.birth_view_region_choice")
    shinyjs::show("C.stacked_barplot_share")
    shinyjs::hide("R.stacked_barplot_share")
    
    output$world_map <- renderLeaflet({
      leaflet() %>%
        addTiles(attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>, 
                                    <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> | 
                                    <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> | 
                                    <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>")) %>%
        addPolygons(data = GeoDATA.WGS84, layerId = ~Residence,
                    color = "blue", stroke = 1, weight = 1.5, opacity = 0.75, fillOpacity = 0.25, 
                    label = lapply(Label, HTML)) %>%
        addScaleBar(position = "bottomleft")
      })
  })
  
  observeEvent(input$regions, {
    shinyjs::show("region_choice")
    shinyjs::show("R.birth_view_choice")
    shinyjs::hide("C.birth_view_choice")
    shinyjs::show("R.birth_view_region_choice")
    shinyjs::hide("C.birth_view_region_choice")
    shinyjs::show("R.stacked_barplot_share")
    shinyjs::hide("C.stacked_barplot_share")
    
    fColor <- colorFactor(c("#339900", "#FF9900", "#003366", "#FF0000", "#663399", "#FFCC00"), 
                          GeoRDATA.CRs.WGS84$ContinentalRegion, na.color = "#999999")

    output$world_map <- renderLeaflet({
      leaflet() %>%
        addTiles(attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>,
                                    <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> |
                                    <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> |
                                    <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>")) %>%
        addPolygons(data = GeoRDATA.CRs.WGS84, layerId = ~Residence,
                    color = "blue", stroke = 1, weight = 1.5, opacity = 0.75, 
                    fillColor = ~fColor(ContinentalRegion), fillOpacity = 1,
                    label = lapply(LabelR.CRs, HTML)
                    ) %>%
        addLegend(position = "topright", pal = fColor, values = GeoRDATA.CRs.WGS84$ContinentalRegion, 
                  opacity = 1, title = "Continental Region") %>%
        addScaleBar(position = "bottomleft")
    })
  })
  
  observeEvent(input$region_choice, {
    SelectedRn <- input$region_choice
    if (SelectedRn == "Continental Regions") {
      GeoRDATA <- GeoRDATA.CRs.WGS84
      ResidenceL <- GeoRDATA$Residence
      LabelR <- LabelR.CRs
      ColorP <- c("#339900", "#FF9900", "#003366", "#FF0000", "#663399", "#FFCC00")
      DOMAIN <- GeoRDATA$ContinentalRegion
      LegendT <- "Continental Region"
      } 
    else if (SelectedRn == "Continental Sub-Regions") {
      GeoRDATA <- GeoRDATA.CSRs.WGS84
      ResidenceL <- GeoRDATA$SIRegion
      LabelR <- LabelR.CSRs
      # Latin America => ColorCreation
      # SR <- c("Caribbean", "Central America", "South America")
      # colorRampPalette(c("#FF0000", "#660000"))(length(SR))
      ColorP <- c("#CCFF33", "#99CC33", "#66CC33", "#339900", "#336600", 
                  "#FFCC99","#CC9966", "#996600", "#FF9900","#FF6600", 
                  "#33CCFF", "#3399CC", "#006699", "#003366", 
                  "#FF0000", "#B20000", "#660000", "#663399",
                  "#FFCC00", "#FFFF00", "#FFFF99", "#FFFFCC")
      DOMAIN <- GeoRDATA$SIRegion
      DOMAIN <- factor(DOMAIN, levels = c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", 'Western Africa', 
                                          "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia", 
                                          "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", 
                                          "Caribbean", "Central America", "South America", "Northern America", 
                                          "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia"))
      LegendT <- "Continental Sub-Region"
      } 
    else if (SelectedRn == "Geographic Regions (SDG)") {
      GeoRDATA <- GeoRDATA.GRSDG.WGS84
      ResidenceL <- GeoRDATA$ClasseSDG
      LabelR <- LabelR.GRSDG
      ColorP <- c("#FF0000", "#FF6600", "#339900", "#003366", "#33CCFF", "#FF9900", "#CC0000", "#FF3399")
      DOMAIN <- GeoRDATA$ClasseSDG
      LegendT <- "SDG Classification"
      } 
    else if (SelectedRn == "Income Levels") {
      GeoRDATA <- GeoRDATA.IncomeLevel.WGS84
      ResidenceL <- GeoRDATA$Residence
      LabelR <- LabelR.IncomeLevel
      ColorP <- c("#339900", "#99FF66", "#CC99FF", "#663399")
      DOMAIN <- GeoRDATA$Residence
      DOMAIN <- factor(DOMAIN, levels = c("High-Income Countries", 
                                          "Upper-Middle-Income Countries", 
                                          "Lower-Middle-Income Countries", 
                                          "Low-Income Countries"))
      LegendT <- "Income Level"
      }
    
    fColor <- colorFactor(ColorP, DOMAIN, na.color = "#999999")
    
    leafletProxy("world_map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = GeoRDATA, layerId = ResidenceL, 
                  color = "blue", stroke = 1, weight = 1.5, opacity = 0.75,
                  fillColor = fColor(DOMAIN), fillOpacity = 1,
                  label = lapply(LabelR, HTML)) %>%
      addLegend(position = "topright", pal = fColor, values = DOMAIN,
                opacity = 1, title = LegendT, na.label = "Not Available")
  })
  
  SelectedResidence <- eventReactive(input$world_map_shape_click, {
    event <- input$world_map_shape_click
    clickedPolygon <- event$id
  })
  
  output$selected_residence <- renderText({
    SelectedR <- SelectedResidence()
    paste("Selected Residence:", SelectedR)
  })
  
  observe({
    if (is.null(input$C.birth_view_choice) || input$C.birth_view_choice == "") {
      updateSelectInput(session, "C.birth_view_choice", selected = "Countries")
    }
  })
  
  output$C.birth_view_choice <- renderUI({
    if (input$countries >= 1 && !is.null(input$world_map_shape_click)) {
      fluidRow(
        column(3, offset = 0,
               selectInput(inputId = "C.birth_view_choice", "Birth View",
                           choices = c("Countries", "Regions"), selected = "Countries",
                           multiple = FALSE)
        ),
        column(3, offset = 0,
               sliderInput(inputId = "decimal_choice", "Decimal Choice", 
                           min = 0, max = 5, value = 2, step = 1, ticks = FALSE)
        )
      )
    }
  })
  
  output$C.birth_view_region_choice <- renderUI({
    if (input$countries >= 1 && input$C.birth_view_choice == "Regions") {
      radioButtons(inputId = "C.birth_view_region_choice", NULL,
                   choices = c("Continental Regions",
                               "Continental Sub-Regions",
                               "Geographic Regions (SDG)",
                               "Income Levels"),
                   selected = "Continental Regions", 
                   inline = TRUE)
    }
  })

  observeEvent(input$world_map_shape_click, {
    observeEvent(input$C.birth_view_choice, {
      SelectedBvC <- input$C.birth_view_choice
      if (SelectedBvC == "Countries") {
        output$C.stacked_barplot_share <- renderPlotly({
          SelectedR <- SelectedResidence()
          SelectedRCode <- GeoDATA.WGS84$ResidenceCode[which(GeoDATA.WGS84$Residence == SelectedR)]

          IMS.FDATA <- IMS.DATA %>% filter(ResidenceCode == SelectedRCode, BirthCode < 900 | BirthCode == 2003,
                                            Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
            distinct(BirthCode, Year, .keep_all = TRUE)
   
          SValues <- list()
          for (ROW in 1:nrow(IMS.FDATA)) {
            YYYY <- IMS.FDATA$Year[ROW]
            IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                                IMS.DATA$BirthCode == 900 &
                                IMS.DATA$Year == YYYY, "IMST"]
            SHARE <- (IMS.FDATA$IMST[ROW]/IMSS) * 100
            SValues[[ROW]] <- SHARE
          }
          IMS.FDATA$IMST_SHARE <- unlist(SValues)

          IMS.FDATA <- IMS.FDATA %>%
            group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>%
            summarise(IMST = sum(IMST), IMSM = sum(IMSM), IMSF = sum(IMSF), IMST_SHARE = sum(IMST_SHARE))

          IMS.FDATA$Birth <- factor(IMS.FDATA$Birth, levels = rev(CountriesColorCodeIMS$Residence))

          levels(IMS.FDATA$Birth)
          
          X <- Mode((IMS.FDATA %>%
                        group_by(Year) %>%
                        summarize(Count = n_distinct(BirthCode)))$Count)

          Colors <- rev(CountriesColorCodeIMS$CLRCode)

          plot_ly(data = IMS.FDATA, x = ~Year, y = ~IMST_SHARE, color = ~Birth,
                  type = "bar", colors = Colors, legendgroup = 'Birth',
                  text = with(IMS.FDATA, paste(" Residence:", Residence, "<br>",
                                               "Birth:", Birth, "<br>",
                                               "Year:", Year, "<br>",
                                               "International Migrant Stock:", round(IMST_SHARE, input$decimal_choice), "%"))) %>%
            style(hoverinfo = "text", textposition = "none") %>%
            layout(barmode = "stack",
                    yaxis = list(title = "International Migrant Stock (%)"),
                    title = paste("Share of International Migrant Stocks in", SelectedR, "from", X, "Countries (1990-2020)"),
                    legend = list(title = list(text='<b> Birth </b>'), x=1, y=0.5))
        })
      }
      else if (SelectedBvC == "Regions") {
        observeEvent(input$C.birth_view_region_choice, {
          SelectedBvRC <- input$C.birth_view_region_choice
          if (SelectedBvRC == "Continental Regions") {
            SBirthCode = c(903, 935, 908, 904, 905, 909)
            TT <- "6 Continental Regions"
            ColorsR <- c("#339900", "#FF9900", "#003366", "#FF0000", "#663399", "#FFCC00", "#999999")
            LVs <- c("AFRICA", "ASIA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", "NORTHERN AMERICA", "OCEANIA", "OTHER")
          }
          if (SelectedBvRC == "Continental Sub-Regions") {
            SBirthCode = c(927, 915, 916, 5500, 910, 906,
                           923, 928, 954, 911, 912, 905,
                           924, 957, 931, 920, 913, 5501,
                           925, 914, 922, 926)
            TT <- "22 Continental Sub-Regions"
            ColorsR <- c("#CCFF33", "#99CC33", "#66CC33", "#339900", "#336600",
                          "#FFCC99","#CC9966", "#996600", "#FF9900","#FF6600",
                          "#33CCFF", "#3399CC", "#006699", "#003366",
                          "#FF0000", "#B20000", "#660000", "#663399",
                          "#FFCC00", "#FFFF00", "#FFFF99", "#FFFFCC",
                          "#999999")
            LVs <- c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", 'Western Africa',
                      "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia",
                      "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
                      "Caribbean", "Central America", "South America", "Northern America",
                      "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia*",
                      "Other")
          }
          else if (SelectedBvRC == "Geographic Regions (SDG)") {
            SBirthCode = c(927, 921, 1832, 1829, 1830, 1833, 1835, 947)
            TT <- "8 Geographic Regions"
            ColorsR <- c("#FF0000", "#FF6600", "#339900", "#003366", "#33CCFF", "#FF9900", "#CC0000", "#FF3399", "#999999")
            LVs <- c("Australia and New Zealand", "Central and Southern Asia", "Eastern and South-Eastern Asia",
                      "Europe and Northern America", "Latin America and the Caribbean", "Northern Africa and Western Asia",
                      "Oceania (excluding Australia and New Zealand", "Sub-Saharan Africa", "Other")
          }
          else if (SelectedBvRC == "Income Levels") {
            SBirthCode = c(1503, 1500, 1501, 1502)
            TT <- "4 Income Levels"
            ColorsR <- c("#339900", "#99FF66", "#CC99FF", "#663399", "#999999")
            LVs <- c("High-Income Countries", "Upper-Middle-Income Countries",
                     "Lower-Middle-Income Countries", "Low-Income Countries", "Other")
            }
          output$C.stacked_barplot_share <- renderPlotly({
            SelectedR <- SelectedResidence()
            SelectedRCode <- GeoDATA.WGS84$ResidenceCode[which(GeoDATA.WGS84$Residence == SelectedR)]

            IMS.FDATA <- IMS.DATA %>% filter(ResidenceCode == SelectedRCode, BirthCode %in% SBirthCode | BirthCode == 2003,
                                             Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
              distinct(BirthCode, Year, .keep_all = TRUE)

            if (SelectedBvRC == "Continental Regions") {
              IMS.FDATA <- IMS.FDATA %>%
                mutate(Birth = case_when(
                  Birth == "Other" ~ "OTHER",
                  TRUE ~ Birth))
            }

            if (SelectedBvRC == "Continental Sub-Regions") {
              IMS.FDATA <- IMS.FDATA %>%
                mutate(Birth = case_when(
                  Birth == "NORTHERN AMERICA" ~ "Northern America",
                  TRUE ~ Birth))
            }

            SValues <- list()
            for (ROW in 1:nrow(IMS.FDATA)) {
              YYYY <- IMS.FDATA$Year[ROW]
              IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                                 IMS.DATA$BirthCode == 900 &
                                 IMS.DATA$Year == YYYY, "IMST"]
              SHARE <- (IMS.FDATA$IMST[ROW]/IMSS) * 100
              SValues[[ROW]] <- SHARE
            }
            IMS.FDATA$IMST_SHARE <- unlist(SValues)

            IMS.FDATA <- IMS.FDATA %>%
               group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>%
              summarise(IMST = sum(IMST), IMSM = sum(IMSM), IMSF = sum(IMSF), IMST_SHARE = sum(IMST_SHARE))

            IMS.FDATA$Birth <- factor(IMS.FDATA$Birth, levels = rev(LVs))

            # if (any(IMS.FDATA$BirthCode == 2003) == TRUE) {
            #   ColorsR <- Colors
            # } else {
            #   ColorsR <- ColorsO
            # }

            plot_ly(data = IMS.FDATA, x = ~Year, y = ~IMST_SHARE, color = ~Birth,
                    type = "bar", colors = rev(ColorsR), legendgroup = 'Birth',
                    text = with(IMS.FDATA, paste(" Residence:", Residence, "<br>",
                                                 "Birth:", Birth, "<br>",
                                                 "Year:", Year, "<br>",
                                                 "International Migrant Stock:", round(IMST_SHARE, input$decimal_choice), "%"))) %>%
              style(hoverinfo = "text", textposition = "none") %>%
              layout(barmode = "stack",
                     yaxis = list(title = "International Migrant Stock (%)"),
                     title = paste("Share of International Migrant Stocks in", SelectedR, "from", TT, "(1990-2020)"),
                     legend = list(title = list(text='<b> Birth </b>'), x=1, y=0.5))
          })
        })
      }
    })
  })
  
  observe({
    if (is.null(input$R.birth_view_choice) || input$R.birth_view_choice == "") {
      updateSelectInput(session, "R.birth_view_choice", selected = "Countries")
    }
  })
  
  output$R.birth_view_choice <- renderUI({
    if (input$regions >= 1 && !is.null(input$world_map_shape_click)) {
      fluidRow(
        column(3, offset = 0,
               selectInput(inputId = "R.birth_view_choice", "Birth View",
                           choices = c("Countries", "Regions"), selected = "Countries",
                           multiple = FALSE)
        ),
        column(3, offset = 0,
               sliderInput(inputId = "decimal_choice", "Decimal Choice", 
                           min = 0, max = 5, value = 2, step = 1, ticks = FALSE)
        )
      )
    }
  })
  
  output$R.birth_view_region_choice <- renderUI({
    if (input$regions >= 1 && input$R.birth_view_choice == "Regions") {
      radioButtons(inputId = "R.birth_view_region_choice", NULL,
                   choices = c("Continental Regions",
                               "Continental Sub-Regions",
                               "Geographic Regions (SDG)",
                               "Income Levels"),
                   selected = "Continental Regions", 
                   inline = TRUE)
    }
  })
  
  observeEvent(input$world_map_shape_click, {
    observeEvent(input$R.birth_view_choice, {
      SelectedBvC <- input$R.birth_view_choice
      if (SelectedBvC == "Countries") {
        output$R.stacked_barplot_share <- renderPlotly({
          SelectedR <- SelectedResidence()
          if (input$region_choice == "Continental Regions") {
            SelectedRCode <- GeoRDATA.CRs.WGS84$ResidenceCode[which(GeoRDATA.CRs.WGS84$Residence == SelectedR)]
          }
          else if (input$region_choice == "Continental Sub-Regions") {
            SelectedRCode <- GeoRDATA.CSRs.WGS84$ResidenceCode[which(GeoRDATA.CSRs.WGS84$SIRegion == SelectedR)]
          }
          else if (input$region_choice == "Geographic Regions (SDG)") {
            SelectedRCode <- GeoRDATA.GRSDG.WGS84$ResidenceCode[which(GeoRDATA.GRSDG.WGS84$ClasseSDG == SelectedR)]
          }
          else if (input$region_choice == "Income Levels") {
            SelectedRCode <- GeoRDATA.IncomeLevel.WGS84$ResidenceCode[which(GeoRDATA.IncomeLevel.WGS84$Residence == SelectedR)]
          }
          
          IMS.FDATA <- IMS.DATA %>% filter(ResidenceCode == SelectedRCode, BirthCode < 900 | BirthCode == 2003,
                                           Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
            distinct(BirthCode, Year, .keep_all = TRUE)
          
          IMSSC <- IMS.FDATA %>% group_by(Year) %>% summarize(IMSTSUM = sum(IMST))
          for (ROW in 1:nrow(IMSSC)) {
            YYYY <- IMSSC$Year[ROW]
            IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                               IMS.DATA$BirthCode == 900 &
                               IMS.DATA$Year == YYYY, "IMST"]
            IMS.OTHER <- IMSS[[1]] - IMSSC$IMSTSUM[ROW]
            IMS.FDATA <- rbind(IMS.FDATA, list(SelectedR, SelectedRCode, "Other", 2003, YYYY, IMS.OTHER, NA, NA))
          }
          
          SValues <- list()
          for (ROW in 1:nrow(IMS.FDATA)) {
            YYYY <- IMS.FDATA$Year[ROW]
            IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                               IMS.DATA$BirthCode == 900 &
                               IMS.DATA$Year == YYYY, "IMST"]
            SHARE <- (IMS.FDATA$IMST[ROW]/IMSS) * 100
            SValues[[ROW]] <- SHARE
          }
          IMS.FDATA$IMST_SHARE <- unlist(SValues)
          
          IMS.FDATA <- IMS.FDATA %>%
            group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>%
            summarise(IMST = sum(IMST), IMSM = sum(IMSM), IMSF = sum(IMSF), IMST_SHARE = sum(IMST_SHARE))
          
          IMS.FDATA$Birth <- factor(IMS.FDATA$Birth, levels = rev(CountriesColorCodeIMS$Residence))
          
          levels(IMS.FDATA$Birth)
          
          X <- Mode((IMS.FDATA %>%
                       group_by(Year) %>%
                       summarize(Count = n_distinct(BirthCode)))$Count)
          
          Colors <- rev(CountriesColorCodeIMS$CLRCode)
          
          plot_ly(data = IMS.FDATA, x = ~Year, y = ~IMST_SHARE, color = ~Birth,
                  type = "bar", colors = Colors, legendgroup = 'Birth',
                  text = with(IMS.FDATA, paste(" Residence:", Residence, "<br>",
                                               "Birth:", Birth, "<br>",
                                               "Year:", Year, "<br>",
                                               "International Migrant Stock:", round(IMST_SHARE, input$decimal_choice), "%"))) %>%
            style(hoverinfo = "text", textposition = "none") %>%
            layout(barmode = "stack",
                   yaxis = list(title = "International Migrant Stock (%)"),
                   title = paste("Share of International Migrant Stocks in", SelectedR, "from", X, "Countries (1990-2020)"),
                   legend = list(title = list(text='<b> Birth </b>'), x=1, y=0.5))
        })
      }
      else if (SelectedBvC == "Regions") {
        observeEvent(input$R.birth_view_region_choice, {
          SelectedBvRC <- input$R.birth_view_region_choice
          if (SelectedBvRC == "Continental Regions") {
            SBirthCode = c(903, 935, 908, 904, 905, 909)
            TT <- "6 Continental Regions"
            ColorsR <- c("#339900", "#FF9900", "#003366", "#FF0000", "#663399", "#FFCC00", "#999999")
            LVs <- c("AFRICA", "ASIA", "EUROPE", "LATIN AMERICA AND THE CARIBBEAN", "NORTHERN AMERICA", "OCEANIA", "OTHER")
          }
          if (SelectedBvRC == "Continental Sub-Regions") {
            SBirthCode = c(927, 915, 916, 5500, 910, 906,
                           923, 928, 954, 911, 912, 905,
                           924, 957, 931, 920, 913, 5501,
                           925, 914, 922, 926)
            TT <- "22 Continental Sub-Regions"
            ColorsR <- c("#CCFF33", "#99CC33", "#66CC33", "#339900", "#336600",
                         "#FFCC99","#CC9966", "#996600", "#FF9900","#FF6600",
                         "#33CCFF", "#3399CC", "#006699", "#003366",
                         "#FF0000", "#B20000", "#660000", "#663399",
                         "#FFCC00", "#FFFF00", "#FFFF99", "#FFFFCC",
                         "#999999")
            LVs <- c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", 'Western Africa',
                     "Central Asia", "Eastern Asia", "South-Eastern Asia", "Southern Asia", "Western Asia",
                     "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
                     "Caribbean", "Central America", "South America", "Northern America",
                     "Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia*",
                     "Other")
          }
          else if (SelectedBvRC == "Geographic Regions (SDG)") {
            SBirthCode = c(927, 921, 1832, 1829, 1830, 1833, 1835, 947)
            TT <- "8 Geographic Regions"
            ColorsR <- c("#FF0000", "#FF6600", "#339900", "#003366", "#33CCFF", "#FF9900", "#CC0000", "#FF3399", "#999999")
            LVs <- c("Australia and New Zealand", "Central and Southern Asia", "Eastern and South-Eastern Asia",
                     "Europe and Northern America", "Latin America and the Caribbean", "Northern Africa and Western Asia",
                     "Oceania (excluding Australia and New Zealand", "Sub-Saharan Africa", "Other")
          }
          else if (SelectedBvRC == "Income Levels") {
            SBirthCode = c(1503, 1500, 1501, 1502)
            TT <- "4 Income Levels"
            ColorsR <- c("#339900", "#99FF66", "#CC99FF", "#663399", "#999999")
            LVs <- c("High-Income Countries", "Upper-Middle-Income Countries",
                     "Lower-Middle-Income Countries", "Low-Income Countries", "Other")
          }
          output$R.stacked_barplot_share <- renderPlotly({
            SelectedR <- SelectedResidence()
            if (input$region_choice == "Continental Regions") {
              SelectedRCode <- GeoRDATA.CRs.WGS84$ResidenceCode[which(GeoRDATA.CRs.WGS84$Residence == SelectedR)]
            }
            else if (input$region_choice == "Continental Sub-Regions") {
              SelectedRCode <- GeoRDATA.CSRs.WGS84$ResidenceCode[which(GeoRDATA.CSRs.WGS84$SIRegion == SelectedR)]
            }
            else if (input$region_choice == "Geographic Regions (SDG)") {
              SelectedRCode <- GeoRDATA.GRSDG.WGS84$ResidenceCode[which(GeoRDATA.GRSDG.WGS84$ClasseSDG == SelectedR)]
            }
            else if (input$region_choice == "Income Levels") {
              SelectedRCode <- GeoRDATA.IncomeLevel.WGS84$ResidenceCode[which(GeoRDATA.IncomeLevel.WGS84$Residence == SelectedR)]
            }
            
            IMS.FDATA <- IMS.DATA %>% filter(ResidenceCode == SelectedRCode, BirthCode %in% SBirthCode | BirthCode == 2003,
                                             Year == c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) %>%
              distinct(BirthCode, Year, .keep_all = TRUE)
            
            IMSSR <- IMS.FDATA %>% group_by(Year) %>% summarize(IMSTSUM = sum(IMST))
            for (ROW in 1:nrow(IMSSR)) {
              YYYY <- IMSSR$Year[ROW]
              IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                                 IMS.DATA$BirthCode == 900 &
                                 IMS.DATA$Year == YYYY, "IMST"]
              IMS.OTHER <- IMSS[[1]] - IMSSR$IMSTSUM[ROW]
              IMS.FDATA <- rbind(IMS.FDATA, list(SelectedR, SelectedRCode, "Other", 2003, YYYY, IMS.OTHER, NA, NA))
            }
            
            if (SelectedBvRC == "Continental Regions") {
              IMS.FDATA <- IMS.FDATA %>%
                mutate(Birth = case_when(
                  Birth == "Other" ~ "OTHER",
                  TRUE ~ Birth))
            }
            
            if (SelectedBvRC == "Continental Sub-Regions") {
              IMS.FDATA <- IMS.FDATA %>%
                mutate(Birth = case_when(
                  Birth == "NORTHERN AMERICA" ~ "Northern America",
                  TRUE ~ Birth))
            }
            
            SValues <- list()
            for (ROW in 1:nrow(IMS.FDATA)) {
              YYYY <- IMS.FDATA$Year[ROW]
              IMSS <- IMS.DATA[IMS.DATA$ResidenceCode == SelectedRCode &
                                 IMS.DATA$BirthCode == 900 &
                                 IMS.DATA$Year == YYYY, "IMST"]
              SHARE <- (IMS.FDATA$IMST[ROW]/IMSS) * 100
              SValues[[ROW]] <- SHARE
            }
            IMS.FDATA$IMST_SHARE <- unlist(SValues)
            
            IMS.FDATA <- IMS.FDATA %>%
              group_by(Year, Residence, ResidenceCode, Birth, BirthCode) %>%
              summarise(IMST = sum(IMST), IMSM = sum(IMSM), IMSF = sum(IMSF), IMST_SHARE = sum(IMST_SHARE))
            
            IMS.FDATA$Birth <- factor(IMS.FDATA$Birth, levels = rev(LVs))
            
            plot_ly(data = IMS.FDATA, x = ~Year, y = ~IMST_SHARE, color = ~Birth,
                    type = "bar", colors = rev(ColorsR), legendgroup = 'Birth',
                    text = with(IMS.FDATA, paste(" Residence:", Residence, "<br>",
                                                 "Birth:", Birth, "<br>",
                                                 "Year:", Year, "<br>",
                                                 "International Migrant Stock:", round(IMST_SHARE, input$decimal_choice), "%"))) %>%
              style(hoverinfo = "text", textposition = "none") %>%
              layout(barmode = "stack",
                     yaxis = list(title = "International Migrant Stock (%)"),
                     title = paste("Share of International Migrant Stocks in", SelectedR, "from", TT, "(1990-2020)"),
                     legend = list(title = list(text='<b> Birth </b>'), x=1, y=0.5))
          })
        })
      }
    })
  })
}

serverWR <- function(input, output) {
  observeEvent(input$countries, {
    output$world_map <- renderLeaflet({
      leaflet(options = leafletOptions(
        crs = leafletCRS(
          crsClass = "L.Proj.CRS", 
          code = "ESRI:54030", 
          proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs", 
          #Resolution = World width in pixels / Map width in pixels
          resolutions = 250000 / (2^(0:20))))) %>%
          #Scale = 1 / Resolution
        #addTiles(urlTemplate = "https://yourtilesource.com/{z}/{x}/{y}.png") %>%
        addTiles(attribution = HTML("© <a href='https://www.openstreetmap.org/copyright/' target='_blank'>OpenStreetMap</a>,
                                    <a href='https://opendatacommons.org/licenses/odbl/' target='_blank'>ODbL</a> |
                                    <a href='https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/' target='_blank'>OpenDataSoft</a> |
                                    <a href='https://cran.r-project.org/web/packages/rnaturalearth/index.html' target='_blank'>RNaturalEarth</a>")) %>%
        addPolygons(data = GeoDATA.WGS84, layerId = ~Residence,
                    color = "blue", stroke = 1, weight = 1.5, opacity = 0.75, fillOpacity = 0.25, 
                    label = lapply(Label, HTML)) %>%
        addScaleBar(position = "bottomleft")
    })
    
    SelectedResidence <- eventReactive(input$world_map_shape_click, {
      event <- input$world_map_shape_click
      clickedPolygon <- event$id
    })
    
    output$selected_residence <- renderText({
      SelectedR <- SelectedResidence()
      paste("Selected Residence:", SelectedR)
    })
  })
}