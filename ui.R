#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(png)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(shinylogs)
#library(bootstraplib) not aval for the new R vsn

#pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book
pwd <- "~/Magneto/Digitizations/" #This is on corsair



  #when app stop,
  # navigate to the directory containing the logs
  # onStop(function() {
  #   browseURL(url = paste0(pwd, "logs"))
  # })



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),

            #app title
            headerPanel(title = "Magneto Digitization Checker"),
            theme = shinytheme("darkly"),
            # bs_theme_new(),
            # bs_theme_base_colors(bg = "#444", fg = "#e4e4e4"),
            # bs_theme_accent_colors(primary = "#e39777", secondary = "#fdce93"),
            fluidRow(

            ),

            fluidRow(




                column(12,

                       #output of formatted text for the image Name
                       h3(textOutput("oneImageName")),

                       checkboxInput(inputId = "DigitizationChecking",
                                     label = "Would you like to check digitizations?",
                                     value = FALSE
                                    ),

                       #output : plot of the requested image
                       plotOutput(outputId = "magPlot",
                                   click = "plot_click") #%>% withSpinner(color = "#0dc5c1") #image Output

                )),
            fluidRow(
              column(4,
                     #hidden(
                     actionButton("VisGood", "Looks Good", class = "btn-success")
                     #)
                    ),
              column(4,
                     #hidden(
                     actionButton("VisFail", "Needs Improvement", class = "btn-info"),
                     #)
                     #Cancel button that comes up when the user presses Needs improvement button
                     hidden(
                     actionButton("Cancel", "Cancel", class = "btn-danger"),
                     actionButton("traceStartOver", "Start Over", class = "btn-danger")
                     )
              ),
              column(4,
                     #hidden(
                     actionButton("DNP", "Digitization Not Possible", class = "btn-danger")
                     #)
              )
            ),

            fluidRow(column(12, br())), # just to add some space between things

            fluidRow(
              column(3),
              column(6,
              hidden(
                verbatimTextOutput("AdvancedInfo"),
                verbatimTextOutput("TraceInfo")
                )
              ),
              column(3)

            ),

            fluidRow(

              column(3,
                    hidden(
                     actionButton("AdvancedHelpLines", "Lines Touch", class = "btn-info")
                    )
              ),
              column(3,
                     hidden(
                       actionButton("AdvancedHelpEnvelopes", "Envelopes incorrect", class = "btn-info"),
                       radioButtons(inputId="envelopeSelection", label="What Envelope do you want to trace?",
                                    choices=c( "Top of Top Trace" = "TTopTrace" ,
                                              "Bottom of Top Trace" = "BTopTrace",
                                              "Top of Bottom Trace" = "TBottomTrace",
                                              "Bottom of Bottom Trace" = "BBottomTrace"))
                     )
              ),
              column(3,
                     hidden(
                       actionButton("AHTopEnv", "Trace Top Envelopes", class = "btn-info"),
                       actionButton("cancelTrace", "Cancel tracing", class = "btn-danger"),
                       actionButton("AHEnvPlot", "Plot to check trace", class = "btn-info")
                     )
              ),
              column(3,
                     hidden(
                       actionButton("AHBottomEnv", "Trace Bottom Envelopes", class = "btn-info")
                     )
              )


            ),

            fluidRow(column(12, br())), # just to add some space between things

            #creating a sidebar for any inputs needed
            fluidRow(

                #not going to use this for now
                #user to set the PWD for where the digitizations are located
                # textInput(inputId = "PWD",
                #           label = "Set PWD",
                #           value = "~/Magnetograms2020/Digitizations/"),

                #selecting the year that will be checked & what image to look at
                column(6,
                       selectInput(inputId = "year",
                                   label = "Select year to be checked",
                                   choices = list.dirs(pwd, full.names = FALSE),
                                   selected = list.dirs(pwd, full.names = FALSE)[2]),

                       # slider for what image to look at
                       numericInput(inputId = "imageNumber",
                                    label = "Select the image number to look at",
                                    value = 2,
                                    min = 1,
                                    max = 100), # Max updates to number of images in spec dir

                       # tableOutput("plotInfoX"),
                       verbatimTextOutput("plotInfoTTopEnv"),
                       verbatimTextOutput("plotInfoBTopEnv")

                ),

                #selecting what plot options that the user wants to see
                column(6,
                       # sidebarPanel( figure out how to make a good background for this image.
                        checkboxGroupInput(inputId = "plotChoices",
                                            label = "Please choose plot overlays",
                                            choices = c("Top Trace Line" = "topTrLine",
                                                        "Bottom Trace Line" = "btmTrLine",
                                                        "Top Trace Start Line" = "startLineTopTr",
                                                        "Bottom Trace Start Line" = "startLineBtmTr"),
                                            selected = c("topTrLine", "btmTrLine"))


                       #)
                )





            )
)

)
