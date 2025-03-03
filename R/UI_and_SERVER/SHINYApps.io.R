####################################
##### SHINYApps.io (rsconnect) #####
####################################

#CONFIGURE Account
rsconnect::setAccountInfo(name='climinet', token='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', secret='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', server = 'shinyapps.io')

#DEPLOY SHINY Application To SHINYApps.io SERVER
rsconnect::deployApp(appDir = paste0(getwd(), "/R/UI_and_SERVER/CLIMINET"), appName = 'demo', appMode = 'shiny', contentCategory = 'site', account = 'climinet', server = 'shinyapps.io')
