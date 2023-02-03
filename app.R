# app.R

source("global.R")
source("ui.R")
source("server.R")

shiny::shinyApp(ui, server)

#- RENV -#

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::update()       # update project libraries
# renv::snapshot()     # save updated lock file to project
