#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book

# Define UI for application that draws a histogram
shinyUI(fluidPage(

            #app title
            headerPanel(title = "Magneto Digitization Checker"),
            theme = shinytheme("darkly"),

            fluidRow(

            ),

            fluidRow(

                column(12,


                       #output of formatted text for the image Name
                       h3(textOutput("oneImageName")),

                       #output : plot of the requested image
                       imageOutput(outputId = "magPlot", width = "auto", height = "auto",
                                   click = "plot_click") #image Output



                )),

            #creating a sidebar for any inputs needed
            fluidRow(

                #not going to use this for now
                #user to set the PWD for where the digitizations are located
                # textInput(inputId = "PWD",
                #           label = "Set PWD",
                #           value = "~/Magnetograms2020/Digitizations/"),

                #selecting the year that will be checked
                column(4,
                       selectInput(inputId = "year",
                                   label = "Select year to be checked",
                                   choices = list.dirs(pwd, full.names = FALSE),
                                   selected = list.dirs(pwd, full.names = FALSE)[2])
                ),
                column(6,

                       # slider for what image to look at
                       numericInput(inputId = "imageNumber",
                                    label = "Select the image number to look at",
                                    value = 1,
                                    min = 1,
                                    max = 100) # Max updates to number of images in spec dir

                ),
                column(2,
                       actionButton("click", "Click me!", class = "btn-danger"),
                       verbatimTextOutput("plotInfo")
                )
            )



        )

)
