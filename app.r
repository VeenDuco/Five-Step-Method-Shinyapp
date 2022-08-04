library(png)
library(shiny)
library(MASS)
library(fGarch)

##############################################################################################################
##############################################################################################################
##             Server part right below, further on user interface part
##############################################################################################################
##############################################################################################################

server <- function(input, output, session) {
  
  
  ####################################################################################
  # Disclaimer
  ####################################################################################
  showModal(modalDialog(
    title = "Disclaimer",
    "Purpose of the service 'utrecht-university.shinyapps.io' is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited and/or can be referenced to. 
    
The web application is provided 'as is' and 'as available' and is without any warranty. Your use of this web application is solely at your own risk.

This web application is a demonstration of methods used in the manuscript 'A Five-Step Method to Elicit Expert Judgement' All correspondes regarding this application, including requests for code, can be send to d.veen@uu.nl.

By continuing you agree to be bound by by the above terms.
    " ))
  
  
  ####################################################################################
  # server part elicitation tool 1
  ####################################################################################
  
  ####################################################################################
  # defining x location for drawing
  ####################################################################################
  v <- reactiveValues(
    click1 = NULL,  # Represents the mouse click, if any
    range = NULL    # this stores the range of x for each click
  )
  
  observeEvent(input$image_click, {
    width  <- session$clientData$output_image1_width
    if(input$image_click$x < 12){
      v$range[[length(v$range)+1]] <- 12
    } else {
      if(input$image_click$x > (width-11)){
        v$range[[length(v$range)+1]] <- (width-11)
      } else {
        v$range[[length(v$range)+1]] <- input$image_click$x
        # Make a range from the previous click and this one.  
        # the if else functions make sure that it does not crash if a click
        # is close to the edge of the field and places the dot at the edge 
        # of the field if it is to close
      }
    }
  })
  
  
  ####################################################################################
  # defining y location for drawing
  ####################################################################################
  vv <- reactiveValues(
    click1 = NULL,  # Represents the first mouse click, if any
    range = NULL    # this stores the range of x for each click
  )
  
  observeEvent(input$image_click, {
    height <- session$clientData$output_image1_height
    if(input$image_click$y < 12){
      vv$range[[length(vv$range)+1]] <- 12
    } else {
      if(input$image_click$y > (height-11)){
        vv$range[[length(vv$range)+1]] <- (height-11)
      } else {
        vv$range[[length(vv$range)+1]] <- input$image_click$y
        # Make a range from the previous click and this one.  
        # the if else functions make sure that it does not crash if a click
        # is close to the edge of the field and places the dot at the edge 
        # of the field if it is to close
      }
    }
  })
  
  ####################################################################################
  # reset and undo button
  ####################################################################################
  observeEvent(input$reset, {
    # Reset for y axis
    vv$range <- NULL
    vv$click1 <- NULL
    # Reset for x axis
    v$range <- NULL
    v$click1 <- NULL
  })
  
  observeEvent(input$Undo, {
    lengte <- length(vv$range) 
    #which x and y number in the vector need to be undone
    vv$range <- vv$range[0:(lengte-1)]
    #undo function for y axis
    v$range <- v$range[0:(lengte-1)]
    #undo function for x axis
  })
  
  ####################################################################################
  # The input image where the participant can draw
  ####################################################################################
  
  
  output$image1 <- renderImage({
    # Get width and height of image output
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    npixels <- width * height
    
    # Fill the pixels for R, G, B
    m <- matrix(1, nrow = height, ncol = width)
    # Add gray vertical and horizontal lines to split in 10 boxes wide 5 up
    m[seq_len(ceiling(height/(width/10))) * (width/10), ] <- 0.5
    m[, (seq_len(ceiling(width/(width/10))) * (width/10) )[1:9]]  <- 0.5
    
    #fixed side blocks for min and maximum value:
    ## right dot fixed
    m[(height-5),(width-11)-0:8] <- 0
    m[(height-6),(width-10)-0:10] <- 0
    m[(height-7),(width-9)-0:12] <- 0
    m[(height-8),(width-8)-0:14] <- 0
    m[(height-9),(width-7)-0:16] <- 0
    m[(height-10),(width-6)-0:18] <- 0
    m[(height-11),(width-5)-0:20] <- 0
    m[(height-18)+0:6,(width-4)-0:22] <- 0
    #center block of sticker
    m[(height-19),(width-5)-0:20] <- 0
    m[(height-20),(width-6)-0:18] <- 0
    m[(height-21),(width-7)-0:16] <- 0
    m[(height-22),(width-8)-0:14] <- 0
    m[(height-23),(width-9)-0:12] <- 0
    m[(height-24),(width-10)-0:10] <- 0
    m[(height-25),(width-11)-0:8] <- 0
    
    ## left dot fixed
    m[(height-5),11:19] <- 0
    m[(height-6),10:20] <- 0
    m[(height-7),9:21] <- 0
    m[(height-8),8:22] <- 0
    m[(height-9),7:23] <- 0
    m[(height-10),6:24] <- 0
    m[(height-11),5:25] <- 0
    m[(height-18)+0:6,4:26] <- 0
    #center block of sticker
    m[(height-19),5:25] <- 0
    m[(height-20),6:24] <- 0
    m[(height-21),7:23] <- 0
    m[(height-22),8:22] <- 0
    m[(height-23),9:21] <- 0
    m[(height-24),10:20] <- 0
    m[(height-25),11:19] <- 0
    
    ## 50 blocks are allowed to be placed
    for(i in 1:50){
      if(length(vv)!=0){
        if(length(v)!= 0){
          
          m[(vv$range[i]+10),(v$range[i])+-4:4] <- 0
          m[(vv$range[i]+9),(v$range[i])+-5:5] <- 0
          m[(vv$range[i]+8),(v$range[i])+-6:6] <- 0
          m[(vv$range[i]+7),(v$range[i])+-7:7] <- 0
          m[(vv$range[i]+6),(v$range[i])+-8:8] <- 0
          m[(vv$range[i]+5),(v$range[i])+-9:9] <- 0
          m[(vv$range[i]+4),(v$range[i])+-10:10] <- 0
          m[(vv$range[i])+-3:3,(v$range[i])+-11:11] <- 0
          #center block of sticker
          m[(vv$range[i]-4),(v$range[i])+-10:10] <- 0
          m[(vv$range[i]-5),(v$range[i])+-9:9] <- 0
          m[(vv$range[i]-6),(v$range[i])+-8:8] <- 0
          m[(vv$range[i]-7),(v$range[i])+-7:7] <- 0
          m[(vv$range[i]-8),(v$range[i])+-6:6] <- 0
          m[(vv$range[i]-9),(v$range[i])+-5:5] <- 0
          m[(vv$range[i]-10),(v$range[i])+-4:4] <- 0
        }
      }
    }
    
    # Convert the vector to an array with 3 planes
    img <- array(c(m, m, m), dim = c(height, width, 3))
    
    # Write it to a temporary file
    outfile <- tempfile(fileext = ".png")
    writePNG(img, target = outfile)
    
    # Return a list containing information about the image
    list(
      src = outfile,
      contentType = "image/png",
      width = width,
      height = height,
      alt = "This is alternate text"
    )
  })
  ####################################################################################
  ####################################################################################
  
  ####################################################################################
  # The fitted distribution drawn beneath the input
  ####################################################################################
  
  output$fitdistr <- renderPlot({
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    npixels <- width * height
    
    # Fill the pixels for R, G, B
    m <- matrix(.75, nrow = height, ncol = width)
    
    #fixed side blocks for min and maximum value:
    ## right dot fixed
    m[(height-5),(width-11)-0:8] <- 1
    m[(height-6),(width-10)-0:10] <- 1
    m[(height-7),(width-9)-0:12] <- 1
    m[(height-8),(width-8)-0:14] <- 1
    m[(height-9),(width-7)-0:16] <- 1
    m[(height-10),(width-6)-0:18] <- 1
    m[(height-11),(width-5)-0:20] <- 1
    m[(height-18)+0:6,(width-4)-0:22] <- 1
    #center block of sticker
    m[(height-19),(width-5)-0:20] <- 1
    m[(height-20),(width-6)-0:18] <- 1
    m[(height-21),(width-7)-0:16] <- 1
    m[(height-22),(width-8)-0:14] <- 1
    m[(height-23),(width-9)-0:12] <- 1
    m[(height-24),(width-10)-0:10] <- 1
    m[(height-25),(width-11)-0:8] <- 1
    
    ## left dot fixed
    m[(height-5),11:19] <- 1
    m[(height-6),10:20] <- 1
    m[(height-7),9:21] <- 1
    m[(height-8),8:22] <- 1
    m[(height-9),7:23] <- 1
    m[(height-10),6:24] <- 1
    m[(height-11),5:25] <- 1
    m[(height-18)+0:6,4:26] <- 1
    #center block of sticker
    m[(height-19),5:25] <- 1
    m[(height-20),6:24] <- 1
    m[(height-21),7:23] <- 1
    m[(height-22),8:22] <- 1
    m[(height-23),9:21] <- 1
    m[(height-24),10:20] <- 1
    m[(height-25),11:19] <- 1
    
    
    ## 50 blocks are allowed to be placed
    for(i in 1:50){
      if(length(vv)!=0){
        if(length(v)!= 0){
          
          m[(vv$range[i]+10),(v$range[i])+-4:4] <- 1
          m[(vv$range[i]+9),(v$range[i])+-5:5] <- 1
          m[(vv$range[i]+8),(v$range[i])+-6:6] <- 1
          m[(vv$range[i]+7),(v$range[i])+-7:7] <- 1
          m[(vv$range[i]+6),(v$range[i])+-8:8] <- 1
          m[(vv$range[i]+5),(v$range[i])+-9:9] <- 1
          m[(vv$range[i]+4),(v$range[i])+-10:10] <- 1
          m[(vv$range[i])+-3:3,(v$range[i])+-11:11] <- 1
          #center block of sticker
          m[(vv$range[i]-4),(v$range[i])+-10:10] <- 1
          m[(vv$range[i]-5),(v$range[i])+-9:9] <- 1
          m[(vv$range[i]-6),(v$range[i])+-8:8] <- 1
          m[(vv$range[i]-7),(v$range[i])+-7:7] <- 1
          m[(vv$range[i]-8),(v$range[i])+-6:6] <- 1
          m[(vv$range[i]-9),(v$range[i])+-5:5] <- 1
          m[(vv$range[i]-10),(v$range[i])+-4:4] <- 1
        }
      }
    }
    
    matrix  <- m
    matrix[which(m != 1)] <- NA
    ## create matrix with only 1's where is clicked
    x <- matrix(NA,nrow =height,ncol=width)
    for(i in 1:height){
      x[i,] <- seq(input$Minimum,input$Maximum,length.out = width)
    }
    ## create matrix for values running from min to max values
    values <- matrix * x
    onlyvalues <- na.omit(as.vector(values))
    onlyvalues <- as.matrix(onlyvalues)
    #multiply the matrixes so only the values remain that have
    #been selected by placing a sticker
    
    # fit a normal distribution for the first 5 values 
    # thereafter fit a skewed normal distribution
    if(length(v$range) < 6){
      out <- fitdistr(onlyvalues,"normal")
      out2 <- as.matrix(out$estimate)
      xas <- seq(input$Minimum,input$Maximum,length.out=width)
      yas <- dnorm(xas,out2[1],out2[2])
      plot(xas,yas, ylab = " ", type = "l",yaxt="n", xlab = "")
    } else {
      out <- snormFit(onlyvalues)
      out2 <- as.matrix(out$par)
      xas <- seq(input$Minimum,input$Maximum,length.out=width)
      yas <- dsnorm(xas,out2[1],out2[2],out2[3])
      plot(xas,yas, ylab = " ", type = "l",yaxt="n", xlab = "")
    }
    
    
  })
  
  ####################################################################################
  # The fitted distribution for the download option of the parameters
  ####################################################################################
  datasetInput <- reactive({
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    npixels <- width * height
    
    # Fill the pixels for R, G, B
    m <- matrix(.75, nrow = height, ncol = width)
    
    #fixed side blocks for min and maximum value:
    ## right dot fixed
    m[(height-5),(width-11)-0:8] <- 1
    m[(height-6),(width-10)-0:10] <- 1
    m[(height-7),(width-9)-0:12] <- 1
    m[(height-8),(width-8)-0:14] <- 1
    m[(height-9),(width-7)-0:16] <- 1
    m[(height-10),(width-6)-0:18] <- 1
    m[(height-11),(width-5)-0:20] <- 1
    m[(height-18)+0:6,(width-4)-0:22] <- 1
    #center block of sticker
    m[(height-19),(width-5)-0:20] <- 1
    m[(height-20),(width-6)-0:18] <- 1
    m[(height-21),(width-7)-0:16] <- 1
    m[(height-22),(width-8)-0:14] <- 1
    m[(height-23),(width-9)-0:12] <- 1
    m[(height-24),(width-10)-0:10] <- 1
    m[(height-25),(width-11)-0:8] <- 1
    
    ## left dot fixed
    m[(height-5),11:19] <- 1
    m[(height-6),10:20] <- 1
    m[(height-7),9:21] <- 1
    m[(height-8),8:22] <- 1
    m[(height-9),7:23] <- 1
    m[(height-10),6:24] <- 1
    m[(height-11),5:25] <- 1
    m[(height-18)+0:6,4:26] <- 1
    #center block of sticker
    m[(height-19),5:25] <- 1
    m[(height-20),6:24] <- 1
    m[(height-21),7:23] <- 1
    m[(height-22),8:22] <- 1
    m[(height-23),9:21] <- 1
    m[(height-24),10:20] <- 1
    m[(height-25),11:19] <- 1
    
    
    ## 50 blocks are allowed to be placed
    for(i in 1:50){
      if(length(vv)!=0){
        if(length(v)!= 0){
          
          m[(vv$range[i]+10),(v$range[i])+-4:4] <- 1
          m[(vv$range[i]+9),(v$range[i])+-5:5] <- 1
          m[(vv$range[i]+8),(v$range[i])+-6:6] <- 1
          m[(vv$range[i]+7),(v$range[i])+-7:7] <- 1
          m[(vv$range[i]+6),(v$range[i])+-8:8] <- 1
          m[(vv$range[i]+5),(v$range[i])+-9:9] <- 1
          m[(vv$range[i]+4),(v$range[i])+-10:10] <- 1
          m[(vv$range[i])+-3:3,(v$range[i])+-11:11] <- 1
          #center block of sticker
          m[(vv$range[i]-4),(v$range[i])+-10:10] <- 1
          m[(vv$range[i]-5),(v$range[i])+-9:9] <- 1
          m[(vv$range[i]-6),(v$range[i])+-8:8] <- 1
          m[(vv$range[i]-7),(v$range[i])+-7:7] <- 1
          m[(vv$range[i]-8),(v$range[i])+-6:6] <- 1
          m[(vv$range[i]-9),(v$range[i])+-5:5] <- 1
          m[(vv$range[i]-10),(v$range[i])+-4:4] <- 1
        }
      }
    }
    
    matrix  <- m
    matrix[which(m != 1)] <- NA
    ## create matrix with only 1's where is clicked
    x <- matrix(NA,nrow =height,ncol=width)
    for(i in 1:height){
      x[i,] <- seq(input$Minimum,input$Maximum,length.out = width)
    }
    ## create matrix for values running from min to max values
    values <- matrix * x
    onlyvalues <- na.omit(as.vector(values))
    onlyvalues <- as.matrix(onlyvalues)
    #multiply the matrixes so only the values remain that have
    #been selected by placing a sticker
    
    # fit a normal distribution for the first 5 values 
    # thereafter fit a skewed normal distribution
    if(length(v$range) < 6){
      out <- fitdistr(onlyvalues,"normal")
      out2 <- as.matrix(out$estimate)
      out2 <- c(out2,1)
    } else {
      out <- snormFit(onlyvalues)
      out2 <- as.matrix(out$par)
    }
    #out <- snormFit(onlyvalues)
    #out2 <- as.matrix(out$par)
    ## create a return matrix with the parameters of interrest
    out3 <- matrix(c(out2,input$Aantal.verkopers),nrow=4)
    rownames(out3) <- c("Mean","SD","Skewness","Number of Sales")
    colnames(out3) <- c("Distribution average sales")
    return(out3)
  })
  
  ####################################################################################
  # Download handler for the download button
  ####################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$name, Sys.Date(), '.txt', sep='') 
      # returns the matrix with parameters of interrest
      # names the file as name input + date
    },
    content = function(file) {
      write.table(datasetInput(), file)
    },
    contentType = "text/csv"
  )
  ####################################################################################
  ####################################################################################
  
  output$count <- renderPrint({
    count <- length(v$range) + 2
    return(count)
  })
  ## counts how many values are entered including the minimum and maximum value  
  output$mean <- renderPrint({
    output <- datasetInput()
    return(round(output[1]))
  })
  output$total <- renderPrint({
    input2 <- datasetInput()
    mean <- round(input2[1])
    aantal <- input$Aantal.verkopers
    total <- aantal * mean
    return(total)
  })
  
  ####################################################################################
  # server part elicitation tool 2
  ####################################################################################
  
  
  output$fitdistr2 <- renderPlot({
    min2 = rep(input$Minimum2,10) # repeat min value 10 times
    max2 = rep(input$Maximum2,10) # repeat max value 10 times
    mean2 = rep(input$Gemiddelde2,100) # repeat mean value 100 times
    data2 <- c(min2,mean2,max2) # get "data" together
    out22 <- snormFit(data2) # fit skewednormal on "data" input
    
    xas2 <- seq(min2[1]-(mean2[1]/10),max2[1]+(mean2[1]/10),length.out=600) # get xaxis for plot
    yas2 <- dsnorm(xas2,input$Gemiddelde2,out22$par[2],out22$par[3]) # get y axis for plot
    plot(xas2,yas2, ylab = " ", type = "l",yaxt="n", xlab = "") # plot
    
  })
  
  
  datasetInput2 <- reactive({
    min2 = rep(input$Minimum2,10)
    max2 = rep(input$Maximum2,10)
    mean2 = rep(input$Gemiddelde2,100)
    data2 <- c(min2,mean2,max2)
    out22 <- snormFit(data2)
    out3 <- matrix(c(input$Gemiddelde2,out22$par[2],out22$par[3],input$Aantal.verkopers),nrow=4)
    rownames(out3) <- c("Mean","SD","Skewness","Number of Sales")
    colnames(out3) <- c("Distribution total sales")
    return(out3)
    
  })
  
  
  observeEvent(input$submit1, {
  test <- datasetInput()
  updateNumericInput(session, "Gemiddelde2", value = round(x=(test[1,1]*test[4,1]),digits=2))
  })
    ## get total value to be reactive
    
  
  ####################################################################################
  # Download handler for the download button
  ####################################################################################
  output$downloadData2 <- downloadHandler(
    filename = function() { 
      paste(Sys.Date(), '.txt', sep='') 
      # returns the matrix with parameters of interrest
      # names the file as name input + date
    },
    content = function(file) {
      write.table(datasetInput2(), file)
    },
    contentType = "text/csv"
  )
  
  
  
}

##############################################################################################################
##############################################################################################################
##             User interface part below
##############################################################################################################
##############################################################################################################




ui <- shinyUI(
  navbarPage("Five-Step Method Elicitation App",
             tabPanel("Elicitation Location Parameter", #tabPanel1
                      fluidPage(    ## application 1: trial roulette method elicitation
                        sidebarLayout(
                          sidebarPanel(
                            numericInput("Aantal.verkopers",
                                         "Count",
                                         value = NA, step = 1),
                            numericInput("Minimum",
                                         "Mimimum value",
                                         value = NA, step = 1),
                            # minimum value of interrest box
                            numericInput("Maximum",
                                         "Maximum value",
                                         value = NA, step = 1),
                            # maximum value of interrest box
                            actionButton("reset", "Reset drawing"),
                            # reset button
                            actionButton("Undo", "Undo"),
                            # undo button
                            actionButton("submit1", "Submit")
                            # submit button
                            ),
                         
                          mainPanel(
                            # Some custom CSS for a smaller font for preformatted text
                            tags$head(
                              tags$style(HTML("
                                              pre, table.table {
                                              font-size: smaller;
                                              }
                                              ")),
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }")
                              ),
                            
                            fluidRow(
                              ## first column containing the input frame where stickers can be added
                              column(width = 12,
                                     plotOutput("fitdistr",width = 720, height = 350),
                                     column(width = 12,offset = 1,
                                            # In a imageOutput, passing values for click, dblclick, hover, or brush
                                            # will enable those interactions.
                                            imageOutput("image1",width=600, height = 300,
                                                        # Equivalent to: click = clickOpts(id = "image_click")
                                                        click = "image_click" ))
                                     
                                     # second column containing the fitted distribution plot
                              )
                            ) ) )
                        
                        
                        
                        
                        )#fluidpage
                      
                      ),#tabpanel
             tabPanel("Elicitation Scale & Shape Parameters", #tabPanel2
                                 fluidPage(    
                                   sidebarLayout(
                                     sidebarPanel(
                                       numericInput("Gemiddelde2","Total",value=NA,step=1),
                                       numericInput("Minimum2",
                                                    "Reasonable lowerbound",
                                                    value = NA, step = 1),
                                       # minimum value of interrest box
                                       numericInput("Maximum2",
                                                    "Reasonable upperbound",
                                                    value = NA, step = 1),
                                       # maximum value of interrest box
                                       
                                       #numericInput("Staart","Onzekerheid",value=2,step=1),
                                       downloadButton('downloadData2', 'Submit')),
                                     # count of amount of "stickers"
                                     mainPanel(
                                       
                                       plotOutput("fitdistr2",width = 720, height = 350)
                                       # second column containing the fitted distribution plot
                                     ) )
                                   
                                   
                                   
                                 )#fluidpage
                        )#tabPanel1
            )#navbarPage
)#shinyUI

shinyApp(ui, server)
