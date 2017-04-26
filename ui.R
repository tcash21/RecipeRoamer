
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(networkD3)
library(shinyjs)

useShinyjs()

shinyUI(
  pageWithSidebar(
    headerPanel("RecipeRoamer"),

  # Sidebar with a slider input for number of bins
  sidebarPanel(
    includeCSS("www/app.css"),
    singleton(
      tags$head(tags$script(src = "main.js"))
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    conditionalPanel(condition="input.tabs1 == 'Similar Ingredients'",       
                     textInput("ingredient",
                               "Ingredient:", placeholder = 'e.g. chicken'),
                     sliderInput('calories', "Calories", min=0, max = 1200, value=1200 ),
                     sliderInput('sodium', "Sodium", min=0, max=1200, value=1200),
                     sliderInput('fat', 'Fat', min=0, max=500, value=500),
                     actionButton('go', 'Go'),
                     actionButton('reset', 'Reset'),
                     htmlOutput('ingredients'),
                     htmlOutput('About')
    ),    
    conditionalPanel(condition="input.tabs1 == 'In My Fridge'", 
                     textInput('ingredient1', "Ingredient"),
                     uiOutput("textbox_ui"),
                     uiOutput("newInputs"),
                     actionButton("add_btn", "Add Ingredient"),
                     actionButton('reset2', 'Reset')
                     
    )),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='tabs1',
        tabPanel("Similar Ingredients",
          fluidRow(
            tags$div(id='instructions', HTML("<strong><ol><li>Type an ingredient and click Go.</li>
               <li>Click an ingredient in the graph for more related ingredients</li>
               <li>Click Reset to search again. </li></ol></strong>")
            ),
            forceNetworkOutput("force")
            ),
          fluidRow(
            uiOutput('view_recipe'),
            DT::dataTableOutput('recipes')
            )
          ),
        tabPanel("In My Fridge",
                 fluidRow(
                   uiOutput('view_recipe2'),
                   DT::dataTableOutput('recipes2')
                 )
        ),
        tabPanel("About", htmlOutput('about'))
      )
    )
  )
)
