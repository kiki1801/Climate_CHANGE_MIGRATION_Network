#################
##### Renv. #####
#################

#Get current WORKING-DIRECTORY
getwd() #"C:/Users/james/Desktop"

#Set WORKING-WDIRECTORY To Folder that contains RSHINY Files 
setwd("C:/Users/james/Desktop/R/UI_and_SERVER/CLIMINET")

#Check current WORKING-DIRECTORY
getwd()

#Initialize renv in current WORKING-DIRECTORY
renv::init() #Create renv.lock file and renv folder

#Source files to load libraries (renv will track libraries loaded)
source("DATA.R")
source("UI.R")
source("SERVER.R")
source("APPLICATION.R")

#Snapshot environment to create renv.lock that list all libraries (capture all required libraries)
renv::snapshot()

#How to use renv.lock file?
renv::restore()

#How to activate environment?
renv::activate()

#How to update renv.lock file?
renv::snapshot()