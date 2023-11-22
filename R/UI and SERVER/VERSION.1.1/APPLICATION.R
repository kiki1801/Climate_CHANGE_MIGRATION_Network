##########################################################################
#####                Climate Change Migration Network                #####
##### Fondation Jean-Jacques Laffont - Toulouse Sciences Economiques #####
#####                   Kyllian James | 2023-11-14                   #####
#####                        Mod.: 2023-11-20                        #####
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

shinyApp(ui = ui, server = server)