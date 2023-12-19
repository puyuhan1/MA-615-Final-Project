#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(
    titlePanel("Analysis on Nauru Island"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Select a Category:"),
            selectInput(
                "category", 
                "Choose a category:",
                choices = c("General Description", "Key Demographics", "Comparison", "SWOT", "References")
            )
        ),
        
        mainPanel(
            uiOutput("selected_content")
        )
    )
)
