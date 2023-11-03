##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-10-24                   #####
#####                          APPLICATION+                          #####
##########################################################################

###########################
##### Load Librairies #####
###########################

library(shiny)

#######################
##### UI | SERVER #####
#######################

source("UI.R")
source("SERVER.R")

#######################
##### APPLICATION #####
#######################

shinyApp(ui = uiFPNAV, server = serverWR)
