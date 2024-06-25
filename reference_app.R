#' ================================================================================================ #
#' Description: Reference app for demonstrating dynamic numbers of plots
#'
#' Input: User selected
#'
#' Output: User created visualisations and tables
#'
#' Author: Winston Chang (https://gist.github.com/wch)
#'
#' Dependencies: Note
#'
#' Notes:
#' 1) Downloaded 2019-03-14 from https://gist.github.com/wch/5436415/
#' 2) Included in this project as the use of 'local' is uncommon. We make significant use of this
#' technique in the main app. This reference app provides a small example to learn the technique.
#'
#' ================================================================================================ #

library(shiny)
max_plots <- 5

## Define UI --------------------------------------------
ui <- pageWithSidebar(
  
  headerPanel("Dynamic number of plots"),
  
  sidebarPanel(
    sliderInput("n", "Number of plots", value=1, min=1, max=5)
  ),
  
  mainPanel(
    # This is the dynamic UI for the plots
    uiOutput("plots")
  )
)

## Define server logic --------------------------------------------
server <- function(input, output, server) {
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      plot_output_list <- lapply(1:input$n, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 280, width = 250)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          plot(1:my_i, 1:my_i,
               xlim = c(1, max_plots),
               ylim = c(1, max_plots),
               main = paste("1:", my_i, ".  n is ", input$n, sep = "")
          )
        })
      })
    }
}

# Run the app ----
shinyApp(ui = ui, server = server)