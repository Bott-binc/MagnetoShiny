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


pwd <- "~/Magnetograms2020/Digitizations/" # This is on botts-book
#pwd <- "~/Documents/Magnetograms2020/Digitizations/" # this is on the corsair
load(paste0(pwd, "todo-200828.RDS"))

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

     # } else{
     #
     #    imageNames <- reactive({
     #        c(dir(path = paste0(pwd, input$year, "/") ,pattern = ".tif.png"),
     #          dir(path = paste0(pwd, input$year, "/", "
     #        })
     #
     #    }

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
    output$plotInfoX <- renderText({
        paste0("x = ", input$plot_click$x)
    })
    output$plotInfoY <- renderText({
        paste0("y = ", input$plot_click$y)
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


    observeEvent(input$VisFail, {
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AdvancedHelpTopTrace")
        toggle("AdvancedHelpBottomTrace")
        toggle("DNP")
        toggle("VisGood")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("VisFail")
    })

    observeEvent(input$Cancel, {
        toggle("AdvancedHelpLines")
        toggle("AdvancedHelpEnvelopes")
        toggle("AdvancedHelpTopTrace")
        toggle("AdvancedHelpBottomTrace")
        toggle("DNP")
        toggle("VisGood")
        toggle("AdvancedInfo")
        toggle("Cancel")
        toggle("VisFail")
    })

    observeEvent(input$DigitizationChecking, {
        toggle("VisGood")
        toggle("VisFail")
        toggle("DNP")
    })
})
