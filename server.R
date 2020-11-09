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
library(magick)
library(stringr)
library(shinyjs)
library(shinylogs)
plotClickX <- vector()

#pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book
pwd <- "~/Magneto/Digitizations/" # this is on the corsair
load(paste0(pwd, "todo-200828.rds"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    track_usage(
        storage_mode = store_rds(path = paste0(pwd, "logs"))
    )

    # if (isFALSE(input$DigitizationChecking)) {
        #getting image names for the dir selected
        imageNames <- reactive({dir(path = paste0(pwd, input$year, "/") ,pattern = ".tif.png")})
        imageNameNoType <- reactive({str_split(imageNames()[input$imageNumber],
                                               pattern = ".p")[[1]][1]})
        #getting image data for the dir selected
        imageDatards <- reactive({dir(path = paste0(pwd, input$year, "/") ,
                                      pattern = paste0(imageNameNoType(), "-Digitized.RDS"))})



    #changes the imageNumber max for numeric input
    observeEvent(imageNames(), {
        max = length(imageNames())
        updateNumericInput(session, "imageNumber", max = max, value = 2)
    })


    # renders the imageNames for printing in ui
    output$ImageNames <- renderPrint({
        imageNames()
    })

    output$AdvancedInfo <- renderText({
        "Please Select where the problem lies"
    })

    output$TraceInfo <- renderText({
        "Please click on the plot and create envelope line"
    })

    # for the points vector

    pointsTTopEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsBTopEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsTBottomEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsBBottomEnv <- reactiveValues(clickx = 0, clicky = 0)

    observeEvent(input$plot_click, {

        if (input$envelopeSelection == "TTopTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsTTopEnv$clickx = c(pointsTTopEnv$clickx,
                                         round(as.numeric(input$plot_click$x), digits = 3))
                pointsTTopEnv$clicky = c(pointsTTopEnv$clicky,
                                         round(as.numeric(input$plot_click$y), digits = 3))
            })
        }
        if (input$envelopeSelection == "BTopTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsBTopEnv$clickx = c(pointsBTopEnv$clickx,
                                         round(as.numeric(input$plot_click$x), digits = 3))
                pointsBTopEnv$clicky = c(pointsBTopEnv$clicky,
                                         round(as.numeric(input$plot_click$y), digits = 3))
            })
        }
        if (input$envelopeSelection == "TBottomTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsTBottomEnv$clickx = c(pointsTBottomEnv$clickx,
                                            round(as.numeric(input$plot_click$x), digits = 3))
                pointsTBottomEnv$clicky = c(pointsTBottomEnv$clicky,
                                            round(as.numeric(input$plot_click$y), digits = 3))
            })
        }
        if (input$envelopeSelection == "BBottomTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsBBottomEnv$clickx = c(pointsBBottomEnv$clickx,
                                            round(as.numeric(input$plot_click$x), digits = 3))
                pointsBBottomEnv$clicky = c(pointsBBottomEnv$clicky,
                                            round(as.numeric(input$plot_click$y), digits = 3))
            })
        }
    })



    observeEvent(input$traceStartOver, {
        if (input$envelopeSelection == "TTopTrace"){
            pointsTTopEnv$clickx = 0
            pointsTTopEnv$clicky = 0
        }
        if (input$envelopeSelection == "BTopTrace"){
            pointsBTopEnv$clickx = 0
            pointsBTopEnv$clicky = 0
        }
        if (input$envelopeSelection == "TBottomTrace"){
            pointsTBottomEnv$clickx = 0
            pointsTBottomEnv$clicky = 0
        }
        if (input$envelopeSelection == "BBottomTrace"){
            pointsBBottomEnv$clickx = 0
            pointsBBottomEnv$clicky = 0
        }
    })



    # observe({
    #     input$plot_click
    #     isolate({ # lets the points to not be re-evaluated
    #         pointsBTopEnv$clickx = c(pointsBTopEnv$clickx, round(as.numeric(input$plot_click$x), digits = 3))
    #         pointsBTopEnv$clicky = c(pointsBTopEnv$clicky, round(as.numeric(input$plot_click$y), digits = 3))
    #     })
    # })
    #
    output$plotInfoTTopEnv <- renderText({
        paste0("x = ", pointsTTopEnv$clickx, ", y = ", pointsTTopEnv$clicky, "\n")
    })
    output$plotInfoBTopEnv <- renderText({
        paste0("x = ", pointsBTopEnv$clickx, ", y = ", pointsBTopEnv$clicky, "\n")
    })
    output$plotInfoTBottomEnv <- renderText({
        paste0("x = ", pointsTBottomEnv$clickx, ", y = ", pointsTBottomEnv$clicky, "\n")
    })
    output$plotInfoBBottomEnv <- renderText({
        paste0("x = ", pointsBBottomEnv$clickx, ", y = ", pointsBBottomEnv$clicky, "\n")
    })





    output$oneImageName <- renderText({as.character(imageNameNoType())})



# Plotting the main digitized image with overlays ------------------------------



    output$magPlot <- renderPlot({
        # return(list(src = paste0(pwd, input$year, "/",
        #                          imageNames()[input$imageNumber]),
        #             contentType = "image/png"))

        #this is working
        magImage <- image_rotate(image_read(paste0(pwd, input$year, "/",
                                      imageNames()[input$imageNumber])), 270)
        magImageDim <- as.numeric(unlist(str_split(image_attributes(magImage)$value[8],
                                                   pattern = ",")))
        magImageWidth <- max(magImageDim)
        #magImageHeight <- min(magImageDim) #just encase the image is non horizontal
        #cropped <- image_crop(magImage, "390x110+61+175") for digitized bad pixelation
        if (length(imageDatards()) == 0) { #there isnt a rds file(not digitized yet)
            plot(image_read(paste0(pwd, "/", "noRDSErrorMessage.png")))
        }
         else{
        magTrace <- readRDS(paste0(pwd,
                                   input$year, "/",
                                   imageDatards()))
        par(mar = c(0, 0, 0, 0))
        plot(magImage)
        input$AHEnvPlot
        input$traceStartOver
        isolate({
            lines(pointsTTopEnv$clickx, pointsTTopEnv$clicky, col = "green")
            lines(pointsBTopEnv$clickx, pointsBTopEnv$clicky, col = "blue")
            lines(pointsTBottomEnv$clickx, pointsTBottomEnv$clicky, col = "red")
            lines(pointsBBottomEnv$clickx, pointsBBottomEnv$clicky, col ="yellow")
        })

        #Options for the plotting ---


        if ("topTrLine" %in% input$plotChoices) {
        #For Top Trace
        lines(c(rep(0, 0.02*magImageWidth), # main line
                rep(0, magTrace$TopTraceStartEnds$Start),
                100 - 8 + magTrace$TopTraceMatrix), lwd = 1.5,col = "red")
        }
        if ("startLineTopTr" %in% input$plotChoices) {
        abline(v = 0.02*magImageWidth + magTrace$TopTraceStartEnds$Start,
               col = "red", lwd = 3) # start line for top trace
        }
        #For Bottom Trace
        if ("btmTrLine" %in% input$plotChoices) {
        lines(c(rep(0, 0.02*magImageWidth), # main line
                rep(0, magTrace$BottomTraceStartEnds$Start),
                100 - 8 + magTrace$BottomTraceMatrix), lwd = 1.5,col = "red")
        }
        if ("startLineBtmTr" %in% input$plotChoices) {
        abline(v = 0.02*magImageWidth + magTrace$BottomTraceStartEnds$Start,
               col = "red", lwd = 3) # start line for bottom trace
        #abline(h = magTrace$Cuts$BottomCut, col = "red")
        }
        }
    })



# Toggling for the buttons when someone presses needs improvement --------------

    #needs improvent toggle
    observeEvent(input$VisFail, {
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AHTopEnv")
        toggle("AHBottomEnv")
        toggle("DNP")
        toggle("VisGood")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("VisFail")
    })

    #when user presses cancel
    observeEvent(input$Cancel, {
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AHTopEnv")
        toggle("AHBottomEnv")
        toggle("DNP")
        toggle("VisGood")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("VisFail")
    })

    #when user decides to do digitization checking
    observeEvent(input$DigitizationChecking, {
        toggle("VisGood")
        toggle("VisFail")
        toggle("DNP")
    })

    #when user decides to look at top envelope improvement
    observeEvent(input$AHTopEnv, {
        toggle("traceStartOver")
        toggle("AHEnvPlot")
        toggle("envelopeSelection")
        toggle("AHTopEnv")
        toggle("Cancel")
        toggle("cancelTrace")
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AHBottomEnv")
        toggle("AdvancedInfo")
        toggle("TraceInfo")

    })

    # for user to cancel the Tracing of the plot
    observeEvent(input$cancelTrace, {
        toggle("traceStartOver")
        toggle("AHEnvPlot")
        toggle("envelopeSelection")
        toggle("AHTopEnv")
        toggle("Cancel")
        toggle("cancelTrace")
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AHBottomEnv")
        toggle("AdvancedInfo")
        toggle("TraceInfo")
    })

})
