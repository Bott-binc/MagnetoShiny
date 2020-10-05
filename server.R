#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #getting image names for the dir selected
    imageNames <- reactive({dir(path = paste0(pwd, input$year, "/") ,pattern = ".png")})

    #changes the imageNumber max for numeric input
    observeEvent(imageNames(), {
        max = length(imageNames())
        updateNumericInput(session, "imageNumber", max = max, value = 1)
    })

    # renders the imageNames for printing in ui
    output$ImageNames <- renderPrint({
        imageNames()
    })

    output$plotInfo <- renderText({
        paste0("x=", input$plot_click$x,
               "y=", input$plot_click$y)
    })

    output$oneImageName <- renderText({as.character(imageNames()[input$imageNumber])})


    output$magPlot <- renderImage({
        return(list(src = paste0(pwd, input$year, "/",
                                 imageNames()[input$imageNumber]),
                    contentType = "image/png"))
    },
    deleteFile = FALSE)

})
