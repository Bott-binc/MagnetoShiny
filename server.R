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
library(readr)
library(magneto)
plotClickX <- vector()

#pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book
pwd <- "~/Magneto/Digitizations/" # this is on the corsair
load(paste0(pwd, "todo-200828.rds"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # track_usage(
    #     storage_mode = store_rds(path = paste0(pwd, "logs"))
    # )

    # if (isFALSE(input$DigitizationChecking)) {
        #getting image names for the dir selected

        imageNames <- reactive({dir(path = paste0(pwd, input$year, "/") ,pattern = ".tif.png")})
        imageNameWithType <- reactive({
            imageNames()[input$imageNumber]
        })
        imageNameNoType <- reactive({# no .png type on the end**
            str_split(imageNames()[input$imageNumber],
                                               pattern = ".p")[[1]][1]})
        #getting image data for the dir selected
        #this does both the digitized and the fail to process in the dir
        imageDatards <- reactive({c(dir(path = paste0(pwd, input$year, "/") ,
                                      pattern = paste0(imageNameNoType(),
                                                       "-Digitized.RDS")),
                                    dir(path = paste0(pwd, input$year, "/") ,
                                        pattern = paste0(imageNameNoType(),
                                                         "-FailToProcess-Data.RDS")))
                                    })



    #changes the imageNumber max for numeric input
    observeEvent(imageNames(), {
        max = length(imageNames())
        updateNumericInput(session, "imageNumber", max = max, value = 1)
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

    # for the start end vector

    output$StartEndInfo <- renderText({
        "Make as close to possible to the start or end of lines respectivly,
        do not include written numbers"
    })

    TopTrStartEnds <- reactiveValues(Start = NA, End = NA)
    BottomTrStartEnds <- reactiveValues(Start = NA , End = NA)

    observeEvent(input$AHStartTop,{
        TopTrStartEnds$Start <- as.numeric(input$AHStartTop)
    })
    observeEvent(input$AHStartBottom, {
        BottomTrStartEnds$Start <- as.numeric(input$AHStartBottom)
    })
    observeEvent(input$AHEndTop,{
        TopTrStartEnds$End <- as.numeric(input$AHEndTop)
    })
    observeEvent(input$AHEndBottom, {
        BottomTrStartEnds$End <- as.numeric(input$AHEndBottom)
    })

    # observeEvent(input$AhStartEndCancel, {
    #     TopTrStartEnds <- reactiveValues(Start = NA, End = NA)
    #     BottomTrStartEnds <- reactiveValues(Start = NA , End = NA)
    # })


    # for the cuts vector

    output$CutInfo <- renderText({
        "Make as close to possible to the two image lines without
        intersecting them (0 is bottom)"
    })


    Cuts <- reactiveValues(TopCut = 0, BottomCut = 0)

    observeEvent(input$CutCheck,{
        Cuts$TopCut <- as.numeric(input$AHCutsTop)
    })
    observeEvent(input$AHCutsBottom, {
        Cuts$BottomCut <- as.numeric(input$AHCutsBottom)
    })

    # for the points/envelopes vector ------------------------------------------

    pointsTTopEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsBTopEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsTBottomEnv <- reactiveValues(clickx = 0, clicky = 0)
    pointsBBottomEnv <- reactiveValues(clickx = 0, clicky = 0)

    observeEvent(input$plot_click, {

        if (input$envelopeSelection == "TTopTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsTTopEnv$clickx = c(pointsTTopEnv$clickx,
                                         round(as.numeric(input$plot_click$x), digits = 0))
                pointsTTopEnv$clicky = c(pointsTTopEnv$clicky,
                                         round(as.numeric(input$plot_click$y), digits = 0))
            })
        }
        if (input$envelopeSelection == "BTopTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsBTopEnv$clickx = c(pointsBTopEnv$clickx,
                                         round(as.numeric(input$plot_click$x), digits = 0))
                pointsBTopEnv$clicky = c(pointsBTopEnv$clicky,
                                         round(as.numeric(input$plot_click$y), digits = 0))
            })
        }
        if (input$envelopeSelection == "TBottomTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsTBottomEnv$clickx = c(pointsTBottomEnv$clickx,
                                            round(as.numeric(input$plot_click$x), digits = 0))
                pointsTBottomEnv$clicky = c(pointsTBottomEnv$clicky,
                                            round(as.numeric(input$plot_click$y), digits = 0))
            })
        }
        if (input$envelopeSelection == "BBottomTrace"){
            isolate({ # lets the points to not be re-evaluated
                pointsBBottomEnv$clickx = c(pointsBBottomEnv$clickx,
                                            round(as.numeric(input$plot_click$x), digits = 0))
                pointsBBottomEnv$clicky = c(pointsBBottomEnv$clicky,
                                            round(as.numeric(input$plot_click$y), digits = 0))
            })
        }
    })


# Resetting points if user pushes re-trace button ------------------------------

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


# Temporary for the user to see the points vectors -----------------------------

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

    #think this will work for throwing to TISI
    envelopeDataTTop <- reactive({
        newpointsTTopEnv <- pointsTTopEnv
        # for (i in 1:length(newpointsTTopEnv$clickx)) {
        #     if (newpointsTTopEnv$clickx[i] > 0){
        #         newpointsTTopEnv$clickx[i] <- newpointsTTopEnv$clickx[i] - 120
        #     }
        # }
        data.frame(x = newpointsTTopEnv$clickx, y = newpointsTTopEnv$clicky) # think this might be 240
    })
    envelopeDataBTop <- reactive({
        newpointsBTopEnv <- pointsBTopEnv
        # for (i in 1:length(newpointsBTopEnv$clickx)) {
        #     if (newpointsBTopEnv$clickx[i] > 0){
        #         newpointsBTopEnv$clickx[i] <- newpointsBTopEnv$clickx[i] - 120
        #     }
        # }
        data.frame(x = newpointsBTopEnv$clickx, y = newpointsBTopEnv$clicky)
    })
    envelopeDataTBottom <- reactive({
        newpointsTBottomEnv <- pointsTBottomEnv
        # for (i in 1:length(newpointsTBottomEnv$clickx)) {
        #     if (newpointsTBottomEnv$clickx[i] > 0){
        #         newpointsTBottomEnv$clickx[i] <- newpointsTBottomEnv$clickx[i] - 120
        #     }
        # }
        data.frame(x = newpointsTBottomEnv$clickx, y = newpointsTBottomEnv$clicky)
    })
    envelopeDataBBottom <- reactive({
        newpointsBBottomEnv <- pointsBBottomEnv
        # for (i in 1:length(newpointsBBottomEnv$clickx)) {
        #     if (newpointsBBottomEnv$clickx[i] > 0){
        #         newpointsBBottomEnv$clickx[i] <- newpointsBBottomEnv$clickx[i] - 120
        #     }
        # }
        data.frame(x = newpointsBBottomEnv$clickx, y = newpointsBBottomEnv$clicky)
    })


# Creating new Digitization for an image -------------------------------------
    DigitizednewRDS <- reactiveValues(newImageDataLoc = NA)
    #flagError <- reactiveVal()

    observeEvent(
        input$reRun, { # when button is clicked <---
            Sys.sleep(5)



            #Top Envelopes -----------------------------------------------------
            if ("TTopEnv" %in% input$reRunChoices){
                if (is.na(envelopeDataTTop()$x[2])){ # user hasn't used this yet..
                    imTTopEnv <- data.frame(x = NA, y = NA)
                }
                else{
                    imTTopEnv <- envelopeDataTTop()
                    for (i in 1:length(imTTopEnv$x)) {
                        if (imTTopEnv$x[i] > 0){
                            imTTopEnv$x[i] <- imTTopEnv$x[i] - 120
                        }
                    }
                }
            }
            else {
                imTTopEnv <- data.frame(x = NA, y = NA)
            }
            if ("BTopEnv" %in% input$reRunChoices){
                if (is.na(envelopeDataBTop()$x[2])){ # user hasn't used this yet..
                    imBTopEnv <- data.frame(x = NA, y = NA)
                }
                else{
                    imBTopEnv <- envelopeDataBTop()
                    for (i in 1:length(imBTopEnv$x)) {
                        if (imBTopEnv$x[i] > 0){
                            imBTopEnv$x[i] <- imBTopEnv$x[i] - 120
                        }
                    }
                }
            }
            else{
                imBTopEnv <- data.frame(x = NA, y = NA)
            }

            #TopEnvelopesStartEnds -----------------
            if ("TopStartEnds" %in% input$reRunChoices){
                if (is.na(TopTrStartEnds$Start) & is.na(TopTrStartEnds$End)){ # user hasn't used this yet..
                    imTopStartEnd <- NA
                }
                else{
                    imTopStartEnd <- c(TopTrStartEnds$Start - 120, TopTrStartEnds$End - 120)
                }
            }
            else{
                imTopStartEnd <- NA
            }


            #Bottom Envelopes --------------------------------------------------
            if ("TBottomEnv" %in% input$reRunChoices){
                if (is.na(envelopeDataTBottom()$x[2])){ # user hasn't used this yet..
                    imTBottomEnv <- data.frame(x = NA, y = NA)
                }
                else{
                    imTBottomEnv <- envelopeDataTBottom()
                    for (i in 1:length(imTBottomEnv$x)) {
                        if (imTBottomEnv$x[i] > 0){
                            imTBottomEnv$x[i] <- imTBottomEnv$x[i] - 120
                        }
                    }
                }
            }
            else{
                imTBottomEnv <- data.frame(x = NA, y = NA)
            }
            if ("BBottomEnv" %in% input$reRunChoices){
                if (is.na(envelopeDataBBottom()$x[2])){ # user hasn't used this yet..
                    imBBottomEnv <- data.frame(x = NA, y = NA)
                }
                else{
                    imBBottomEnv <- envelopeDataBBottom()
                    for (i in 1:length(imBBottomEnv$x)) {
                        if (imBBottomEnv$x[i] > 0){
                            imBBottomEnv$x[i] <- imBBottomEnv$x[i] - 120
                        }
                    }
                }
            }
            else{
                imBBottomEnv <- data.frame(x = NA, y = NA)
            }
            # #BottomEnvelopesStartEnd -----------------
            if ("BottomStartEnds" %in% input$reRunChoices){
                if (is.na(BottomTrStartEnds$Start) & is.na(BottomTrStartEnds$End)){ # user hasn't used this yet..
                    imBottomStartEnd <- NA
                }
                else{
                    imBottomStartEnd <- c(BottomTrStartEnds$Start - 120, BottomTrStartEnds$End - 120)
                }
            }
            else{
                imBottomStartEnd <- NA
            }

            #TopBottomCut
            if ("TBCuts" %in% input$reRunChoices){
                if (Cuts$BottomCut == 0 & Cuts$TopCut == 0){
                    imTBCuts = NA
                }
                else{
                    imTBCuts <- c(Cuts$TopCut, Cuts$BottomCut)
                }
            }
            else{
                imTBCuts = NA
            }




            DigitizednewRDS$newImageDataLoc <-  tryCatch(TISI(imageName = imageNameNoType(), fileLoc = paste0(pwd, input$year, "/"),
                 pathToWorkingDir = pwd, improvement = TRUE, HDVcheck = FALSE, plotPNG = TRUE,
                 saveData = TRUE, improveTopBottomCuts = imTBCuts, improveTTopEnvelope = imTTopEnv,
                 improveBTopEnvelope = imBTopEnv, improveTBottomEnvelope = imTBottomEnv,
                 improveBBottomEnvelope = imBBottomEnv, improveTopEnvelopeStartEnd = imTopStartEnd,
                 improveBottomEnvelopeStartEnd = imBottomStartEnd))

            if(strsplit(DigitizednewRDS$newImageDataLoc, split = " ")[[1]][1] == "Error"&
               strsplit(DigitizednewRDS$newImageDataLoc, split = " ")[[1]][12] == "doesn't") {
                output$ErrorInfo <- renderText({

                    as.character("Error, The .tiff file for this image is not in this directory")

                })
                toggle("errorOk")
                DigitizednewRDS$newImageDataLoc <- NA
            }
            else{
                DigitizednewRDS$newImagedataLoc <-  DigitizednewRDS$newImagedataLoc$newImageLoc
            }


        }
    )

    # renders image name for the plot title ------------------------------------

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
        magImageHeight <- min(magImageDim)
        #magImageHeight <- min(magImageDim) #just encase the image is non horizontal
        #cropped <- image_crop(magImage, "390x110+61+175") for digitized bad pixelation
        if (length(imageDatards()) == 0) { #there isnt a rds file(not digitized yet)
            plot(image_read(paste0(pwd, "/", "noRDSErrorMessage.png")))
        }
         else{
             #if (is.null(data())){
                 magTrace <- readRDS(paste0(pwd,
                                            input$year, "/",
                                            imageDatards()))
            # observeEvent(data(),{

             if(!is.na(DigitizednewRDS$newImageDataLoc)){
                 magTrace <- readRDS(as.character(DigitizednewRDS$newImageDataLoc))
             }
             #})
                 par(mar = c(0, 0, 0, 0))
                 plot(magImage, xlim = c(121, magImageWidth)) #121 for trimming and to make sure never get negative numbers with TISI offset
                 input$AHEnvPlot
                 input$traceStartOver
                 input$CutCheck
                 input$StartEndCheck

                 isolate({
                     abline(h = Cuts$TopCut, col = "red", lwd = 2)
                     abline(h = Cuts$BottomCut, col = "orange", lwd = 2)
                     abline(v = TopTrStartEnds$Start, col = "green", lwd = 2)
                     abline(v = TopTrStartEnds$End, col = "green", lwd = 2)
                     abline(v= BottomTrStartEnds$Start, col = "blue", lwd = 2)
                     abline(v = BottomTrStartEnds$End, col = "blue", lwd = 2)
                     lines(pointsTTopEnv$clickx, pointsTTopEnv$clicky, col = "green")
                     lines(pointsBTopEnv$clickx, pointsBTopEnv$clicky, col = "blue")
                     lines(pointsTBottomEnv$clickx, pointsTBottomEnv$clicky, col = "red")
                     lines(pointsBBottomEnv$clickx, pointsBBottomEnv$clicky, col = "yellow")
                 })


                 #Options for the plotting ---


                 if ("topTrLine" %in% input$plotChoices & !is.null(magTrace$TopTraceStartEnds$Start)
                     & !is.null(magTrace$TopTraceMatrix)) {
                     #For Top Trace
                     lines(c(rep(NA, 0.02*magImageWidth ), # main line
                             rep(NA, magTrace$TopTraceStartEnds$Start),
                             100 - 8 + magTrace$TopTraceMatrix), lwd = 1.5,col = "red")
                 }
                 if ("startLineTopTr" %in% input$plotChoices & !is.null(magTrace$TopTraceStartEnds$Start)) {
                     abline(v = 0.02*magImageWidth + magTrace$TopTraceStartEnds$Start,
                            col = "red", lwd = 3) # start line for top trace
                 }
                 if ("endLineTopTr" %in% input$plotChoices & !is.null(magTrace$TopTraceStartEnds$End)) {
                     abline(v = 0.02*magImageWidth + magTrace$TopTraceStartEnds$End,
                            col = "red", lwd = 3) # start line for top trace
                 }
                 #For Bottom Trace
                 if ("btmTrLine" %in% input$plotChoices & !is.null(magTrace$BottomTraceStartEnds$Start)
                     & !is.null(magTrace$BottomTraceMatrix)) {
                     lines(c(rep(NA, 0.02*magImageWidth), # main line
                             rep(NA, magTrace$BottomTraceStartEnds$Start),
                             100 - 8 + magTrace$BottomTraceMatrix), lwd = 1.5,col = "red")
                 }
                 if ("startLineBtmTr" %in% input$plotChoices &
                     !is.null(magTrace$BottomTraceStartEnds$Start)) {
                     abline(v = 0.02*magImageWidth + magTrace$BottomTraceStartEnds$Start,
                            col = "red", lwd = 3) # start line for bottom trace
                     #abline(h = magTrace$Cuts$BottomCut, col = "red")
                 }
                 if ("endLineBtmTr" %in% input$plotChoices &
                     !is.null(magTrace$BottomTraceStartEnds$End)) {
                     abline(v = 0.02*magImageWidth + magTrace$BottomTraceStartEnds$End,
                            col = "red", lwd = 3) # start line for bottom trace
                     #abline(h = magTrace$Cuts$BottomCut, col = "red")
                 }
                 if ("topBtmCuts" %in% input$plotChoices & !is.null(magTrace$Cuts$TopCut)
                     & !is.null(magTrace$Cuts$BottomCut)) {
                     abline(h = abs(magTrace$Cuts$TopCut - magImageHeight) - 225, col = "orange")
                     abline(h = abs( magTrace$Cuts$BottomCut - magImageHeight) -225,
                            col = "red")
                 }


         }
    })




# Toggling for the buttons when someone presses needs improvement --------------

    #needs improvent toggle
    observeEvent(input$VisFail, {
        toggle("traceStartEnd")
        toggle("AHCuts")
        toggle("AHEnv")
        toggle("AHBottomEnv")
        toggle("DNP")
        toggle("VisGood")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("VisFail")
    })

    #when user presses cancel
    observeEvent(input$Cancel, {
        toggle("traceStartEnd")
        toggle("AHCuts")
        toggle("AHEnv")
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
    observeEvent(input$AHEnv, {
        toggle("traceStartOver")
        toggle("AHEnvPlot")
        toggle("envelopeSelection")
        toggle("AHEnv")
        toggle("Cancel")
        toggle("cancelTrace")
        toggle("traceStartEnd")
        toggle("AHCuts")
        toggle("AHBottomEnv")
        toggle("AdvancedInfo")
        toggle("TraceInfo")

    })

    # for user to cancel the Tracing of the plot
    observeEvent(input$cancelTrace, {
        toggle("traceStartOver")
        toggle("AHEnvPlot")
        toggle("envelopeSelection")
        toggle("AHEnv")
        toggle("Cancel")
        toggle("cancelTrace")
        toggle("traceStartEnd")
        toggle("AHCuts")
        toggle("AHBottomEnv")
        toggle("AdvancedInfo")
        toggle("TraceInfo")
    })

    # for user to start finding the cuts
    observeEvent(input$AHCuts, {
        toggle("AHCutsTop")
        toggle("AHCutsBottom")
        toggle("traceStartEnd")
        toggle("AHEnv")
        toggle("AHBottomEnv")
        toggle("AHCutsCancel")
        toggle("AHCuts")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("CutCheck")
        toggle("CutInfo")
    })

    #for user to cancel finding the cuts
    observeEvent(input$AHCutsCancel, {
        toggle("AHCutsTop")
        toggle("AHCutsBottom")
        toggle("traceStartEnd")
        toggle("AHEnv")
        toggle("AHBottomEnv")
        toggle("AHCutsCancel")
        toggle("AHCuts")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("CutCheck")
        toggle("CutInfo")
    })

    # for user to start finding the start ends
    observeEvent(input$traceStartEnd, {
        toggle("AHStartTop")
        toggle("AHStartBottom")
        toggle("AHEndTop")
        toggle("AHEndBottom")
        toggle("traceStartEnd")
        toggle("AHEnv")
        toggle("AHBottomEnv")
        toggle("AHStartEndCancel")
        toggle("AHCuts")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("StartEndCheck")
        toggle("StartEndInfo")
    })

    #for user to cancel finding the start ends
    observeEvent(input$AHStartEndCancel, {
        toggle("AHStartTop")
        toggle("AHStartBottom")
        toggle("AHEndTop")
        toggle("AHEndBottom")
        toggle("traceStartEnd")
        toggle("AHEnv")
        toggle("AHBottomEnv")
        toggle("AHStartEndCancel")
        toggle("AHCuts")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("StartEndCheck")
        toggle("StartEndInfo")
    })



    observeEvent(input$errorOk, {
        toggle("errorOk")
        toggle("ErrorInfo")
    })

})
