# app.R

source("global.R")
source("ui.R")
source("server.R")

shiny::shinyApp(ui, server)

#- RENV -#

# renv::update()
# renv::snapshot()
