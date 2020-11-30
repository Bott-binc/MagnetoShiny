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
library(shinybusy)
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
  add_busy_spinner(
    spin = "orbit",
    timeout = 100,
    color = "#1D18CA", #"#00FF00", #1D18CA
    position = "top-right",
    margins = c(350, 400)
  ),

  useShinyjs(),


            #app title
            headerPanel(title = "Magneto Digitization Checker"),
            theme = shinytheme("readable"),
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
                                   click = "plot_click") #image Output%>%

                )),
            fluidRow(
              column(4,
                     #hidden(
                     #actionButton("VisGood", "Looks Good", class = "btn-success")
                     #)
                    ),
              column(4,
                     #hidden(
                     actionButton("VisFail", "Needs Improvement", class = "btn-info"),
                     #)
                     #Cancel button that comes up when the user presses Needs improvement button
                     hidden(
                     actionButton("Cancel", "Cancel", class = "btn-danger"),
                     actionButton("traceStartOver", "Re-Trace Line", class = "btn-danger")
                     )
              ),
              column(4,
                     #hidden(
                     #actionButton("DNP", "Digitization Not Possible", class = "btn-danger")
                     #)
              )
            ),

            fluidRow(column(12, br())), # just to add some space between things

            fluidRow(
              column(12,
              hidden(
                verbatimTextOutput("AdvancedInfo"),
                verbatimTextOutput("TraceInfo"),
                verbatimTextOutput("CutInfo"),
                verbatimTextOutput("StartEndInfo")
                )
              )),

            fluidRow(

              column(3,
                    hidden(
                     actionButton("traceStartEnd", "Re-do start ends for lines", class = "btn-info"),
                     actionButton("AHCutsCancel", "Go Back To Main Page", class = "btn-info"),
                     actionButton("AHStartEndCancel", "Go Back To Main Page", class = "btn-info")
                    )
              ),
              column(3,
                     hidden(
                       actionButton("AHCuts", "Re-Draw Image Trim Lines", class = "btn-info"),
                       numericInput(inputId = "AHCutsTop",
                                    label = "Please select height of top cut
                                    between the writing and the first trace (start around 1200)",
                                    value = 0),
                       numericInput(inputId = "AHStartTop",
                                    label = "Please select the starting point for the top line (usually around 200)",
                                    value = 0),
                       numericInput(inputId = "AHStartBottom",
                                    label = "Please select the starting point for the second line from the top (usually around 200)",
                                    value = 0),

                       radioButtons(inputId="envelopeSelection", label="What Envelope do you want to trace?",
                                    choices=c( "Top of Top Trace" = "TTopTrace" ,
                                              "Bottom of Top Trace" = "BTopTrace",
                                              "Top of Bottom Trace" = "TBottomTrace",
                                              "Bottom of Bottom Trace" = "BBottomTrace"))
                     )
              ),
              column(3,
                     hidden(
                       numericInput(inputId = "AHCutsBottom",
                                    label = "Please select height of bottom cut
                                    between the second trace and the timing lines (start around 600)",
                                    value = 0),
                       numericInput(inputId = "AHEndTop",
                                    label = "Please select the ending point for the top line (usually around 6000)",
                                    value = 0),
                       numericInput(inputId = "AHEndBottom",
                                    label = "Please select the ending point for the second line from the top (usually around 6000)",
                                    value = 0),
                       actionButton("AHEnv", "Manually Redo Envelopes", class = "btn-info"),
                       actionButton("cancelTrace", "Go Back To Main Page", class = "btn-info"),
                       actionButton("AHEnvPlot", "Plot to Check Trace", class = "btn-info")

                     )
              ),
              column(3,
                     hidden(
                       #actionButton("AHBottomEnv", "For Something Else", class = "btn-info"), if needed in the future
                       actionButton("CutCheck", "Plot To Check Cuts", class = "btn-info"),
                       actionButton("StartEndCheck", "plot to Check Start Ends", class = "btn-info")
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
                                    value = 1,
                                    min = 1,
                                    max = 100), # Max updates to number of images in spec dir

                       # tableOutput("plotInfoX"),
                       verbatimTextOutput("plotInfoTTopEnv"),
                       verbatimTextOutput("plotInfoBTopEnv"),
                       verbatimTextOutput("plotInfoTBottomEnv"),
                       verbatimTextOutput("plotInfoBBottomEnv")

                ),

                #selecting what plot options that the user wants to see
                column(6,
                       # sidebarPanel( figure out how to make a good background for this image.
                        checkboxGroupInput(inputId = "plotChoices",
                                            label = "Please choose plot overlays",
                                            choices = c("Top Trace Line" = "topTrLine",
                                                        "Bottom Trace Line" = "btmTrLine",
                                                        "Top Trace Start Line" = "startLineTopTr",
                                                        "Top Trace End Line" = "endLineTopTr",
                                                        "Bottom Trace Start Line" = "startLineBtmTr",
                                                        "Bottom Trace End Line" = "endLineBtmTr",
                                                        "Top and Bottom Trim" = "topBtmCuts"),
                                            selected = c("topTrLine", "btmTrLine", "topBtmCuts")),
                       checkboxGroupInput(inputId = "reRunChoices",
                                          label = "Please choose what you would like to re-run",
                                          choices = c("Top of Top Envelope" = "TTopEnv",
                                                      "Bottom of Top Envelope" = "BTopEnv",
                                                      "Top of Bottom Envelope" = "TBottomEnv",
                                                      "Bottom of Bottom Envelope" = "BBottomEnv",
                                                      "Top Trace Start and Ends" = "TopStartEnds",
                                                      "Bottom Trace Start and Ends" = "BottomStartEnds",
                                                      "Top and Bottom Cuts" = "TBCuts")),
                       actionButton("reRun", "never press this button", class = "btn-info")


                       #)
                )





            )
)

)
