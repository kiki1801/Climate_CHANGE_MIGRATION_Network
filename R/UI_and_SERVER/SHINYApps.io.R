####################################
##### SHINYApps.io (rsconnect) #####
####################################

#CONFIGURE Account
rsconnect::setAccountInfo(name='climinet', token='16C4545A62B56510770407F173C363C9', secret='W9il7/nNTFORgmBDJs+xy+0sVDkSYTktHtYftExD', server = 'shinyapps.io')

#DEPLOY SHINY Application To SHINYApps.io SERVER
rsconnect::deployApp(appDir = paste0(getwd(), "/R/UI_and_SERVER/CLIMINET"), appName = 'demo', appMode = 'shiny', contentCategory = 'site', account = 'climinet', server = 'shinyapps.io')