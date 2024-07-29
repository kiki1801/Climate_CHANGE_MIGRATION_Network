##########################################################################
#####                Climate ChanGe MiGration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   KYLLIAN JAMES | 2023-11-14                   #####
#####                        Mod.: 2024-04-23                        #####
#####                          APPLICATION+                          #####
##########################################################################

##########################
##### Load Libraries #####
##########################

library(shiny)

#######################
##### UI | SERVER #####
#######################

source("UI.R")
source("SERVER.R")

#######################
##### APPLICATION #####
#######################

shinyApp(ui = ui, server = server)