# Load Packages ----
library(shiny)
library(dplyr)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)
library(shinyanimate)
library(magick)
library(gganimate)
library(boot)
library(shinycssloaders)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Bootstrapping Distributions", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Bootstrapping_Distributions")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        # menuItem("Game", tabName = "game", icon = icon("gamepad")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Bootstrapping Distributions"), # This should be the full name.
          p("In this app, you will observe the bootstrapping process through simulations."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Sean Burke",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/8/2023 by SB.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Pre-req 1--Technical/Conceptual Prerequisites are ideas that
                    users need to have in order to engage with your app fully."),
            tags$li("Pre-req 2--Contextual Prerequisites refer to any information
                    about a context in your app that will enrich a user's
                    understandings."),
            tags$li("Pre-req 3"),
            tags$li("Pre-req 4")
          ),
          p("Notice the use of an unordered list; users can move through the
            list any way they wish."),
          box(
            title = strong("Null Hypothesis Significance Tests (NHSTs)"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In the Confirmatory Data Analysis tradition, null hypothesis
            significance tests serve as a critical tool to confirm that a
            particular theoretical model describes our data and to make a
            generalization from our sample to the broader population
            (i.e., make an inference). The null hypothesis often reflects the
            simpler of two models (e.g., 'no statistical difference',
            'there is an additive difference of 1', etc.) that we will use to
            build a sampling distribution for our chosen estimator. These
            methods let us test whether our sample data are consistent with this
            simple model (null hypothesis)."
          ),
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the
            p-value, the more often we would expect our estimator to take on a
            value at least as extreme as what we've seen; the smaller, the less
            often."
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Boot strapping Simulations"),
          p("Draw a sample of candy from the Candy machine. 
            Draw bootstrap samples from the original sample and watch how the 
            proportion of blue candies in each bootstrapped sample lies on the histogram.
            Click on the individual plots of a single bootstrap sample and see where it falls in the histogram."),
          fluidRow(
            column(
              width = 5,
              wellPanel(
                sliderInput(
                  inputId = "prop",
                  label = "Set Population Proportion",
                  min = 0,
                  max = 1,
                  value = 0.55,
                  step = 0.05
                ),
                br(),
                # sliderInput(
                #   inputId = "sam",
                #   label = "Set Sample Size",
                #   min = 10,
                #   max = 50,
                #   value = 30,
                #   step = 1
                # ),
                radioGroupButtons(
                  inputId = "sam",
                  label = "Choose a Sample Size",
                  choices = c(
                    "small",
                    "medium",
                    "large"
                  ),
                  selected = "small",
                  disabled = FALSE
                ),
                br(),
                bsButton(
                  inputId = "drawSample", 
                  label = "Draw Sample", 
                  icon = icon("paper-plane"),
                  class = "circle grow",
                  disabled = FALSE
                ),
                br(),
                br(),
                numericInput(
                  inputId = "bootSamp",
                  label = "Boot Samples",
                  min = 4,
                  max = 10000,
                  value = 4
                ),
                br(),
                bsButton(
                  inputId = "drawBoot", 
                  label = "Draw BootStrap Samples", 
                  icon = icon("paper-plane"),
                  class = "circle grow",
                  disabled = TRUE
                ),
                bsButton(
                  inputId = "reset", 
                  label = "Reset!",
                  class = "circle grow"
                )
              )
            ),
            column(
              width = 7,
              h3("Candy Machine", align = "center"),
              br(),
              plotOutput("machine") %>% 
                withSpinner(color = boastUtils::psuPalette[1]),
              uiOutput("sampleFreq1"),
              uiOutput("sampleProp1"),
              uiOutput("sampleSize1"),
            ),
            # column(
            #   width = ,
            #   #h3("Latest Bootstrap Sample"),
            #   br(),
            #   plotOutput("latestBootGridPlot"),
            #   br(),
            #   plotOutput("bootProportionsPlot"),
            # )
          ),
          fluidRow(
            h3("Bootstrap Samples"),
            br(),
            plotOutput(
              "bootPlots",
              click = clickOpts(id = "clicker")
            ) %>%
              withSpinner(color = boastUtils::psuPalette[1])
          ),
          fluidRow(
            bsButton(
              inputId = "firstBoot", 
              label = "First Bootstrap",
              class = "circle grow"
            ),
            bsButton(
              inputId = "secondBoot", 
              label = "Second Bootstrap",
              class = "circle grow"
            ),
            bsButton(
              inputId = "dotBoot", 
              label = "Random Bootstrap",
              class = "circle grow"
            ),
            bsButton(
              inputId = "lastBoot", 
              label = "Last Bootstrap",
              class = "circle grow"
            )

          ),
          plotOutput("bootProportionsPlot") %>%
            withSpinner(color = boastUtils::psuPalette[1])
        ),
          #### Set up a Game Page ----
          tabItem(
            tabName = "game",
            withMathJax(),
            h2("Practice/Test Yourself with [Type of Game]"),
            p("On this type of page, you'll set up a game for the user to play.
            Game types include Tic-Tac-Toe, Matching, and a version Hangman to
            name a few. If you have ideas for new game type, please let us know.")
          ),
          #### Set up the References Page ----
          tabItem(
            tabName = "references",
            withMathJax(),
            h2("References"),
            p("You'll need to fill in this page with all of the appropriate
            references for your app."),
            p(
              class = "hangingindent",
              "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
            ),
            br(),
            br(),
            br(),
            boastUtils::copyrightInfo()
          )
        )
      )
    )
  )



# Define server logic ----
server <- function(input, output, session) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  ## machine set up ----
  img <- readPNG("C:/Users/Sean/Github_Files/Bootstrapping_Distributions/www/slot.png")
  CoinSlot <- rasterGrob(img, interpolate = TRUE)
  
  lab1Pop <- reactive(
    x = {
      prop <- input$prop
      N <- 1000
      blueNum <- round(prop * N)
      redNum <- N - blueNum
      
      data.frame(
        color = rep(c("blue", "red"), times = c(blueNum, redNum)),
        x = runif(N, min = -5, max = 5),
        y = runif(N, min = 0, max = 5)
      )
    }
  )
  
  # outPlot <- ggplot()
  storePlot <- reactiveVal(NULL)
  
  output$machine <- renderPlot(
    expr = {
      data <- lab1Pop()
      
      candyBox <- ggplot(data = data,
                  mapping = aes(x = x, y = y, color = color, shape = color)) +
        geom_point(size = 4, aes(fill = color), shape = 21, color = "black") +  
        scale_fill_manual(
          values = c("blue" = boastUtils::psuPalette[1], "red" =  boastUtils::psuPalette[2])
        ) +
        geom_hline(yintercept = -0.5, linewidth = 2, color = "black") +
        theme_bw() +
        theme(
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.margin = unit(rep(0, 4), "cm") 
        ) +
        labs(title = NULL) +
        xlab(NULL) +
        ylab(NULL)
      
      candyMachineBody <- ggplot() +
        geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill =  boastUtils::psuPalette[2], color = "black") +
        theme_void() +
        geom_rect(aes(xmin = -3, xmax = 3, ymin = -8, ymax = -5), fill = "black") +
        theme_void()
      
      
      FullCandyMachine <- candyMachineBody +
        annotation_custom(ggplotGrob(candyBox), xmin = -5, xmax = 5, ymin = 0, ymax = 7) +
        annotation_custom(CoinSlot, xmin = -4, xmax = 4, ymin = -6, ymax = 0.5) +
        coord_cartesian(
          xlim = c(-12, 12),
          expand = FALSE
        )
      
      storePlot(FullCandyMachine)
      
      FullCandyMachine
    }
  )
  
  sampleReact <- reactiveVal()
  sizeReact <- reactiveVal()
  
  sampleSizeFunc <- function() {
    if (input$sam == "small") {
      36
    } else if (input$sam == "medium") {
      100
    } else if (input$sam == "large") {
     400
    }
  }
  
  storePlot1 <- reactiveVal(NULL)
  storeSampleBlue <- reactiveVal(NULL)
  ## Draw the sample for blue/red ----
  observeEvent(
    eventExpr = input$drawSample, 
    handlerExpr = {
      
      updateButton(
        session = session,
        inputId = "drawBoot",
        disabled = FALSE
      )
      
      sampleSize <- sampleSizeFunc()
      sampleData <- lab1Pop()
      sample1 <- sampleData[sample.int(nrow(sampleData), size = sampleSize), ]
      numRow <- ceiling(sqrt(sampleSize))
      numCol <- ceiling(sampleSize / numRow)
      
      xVal <- rep(seq(from = -5, to = 5, length.out = numCol), each = numRow)
      yVal <- rep(seq(from = -10, to = -14, length.out = numRow), times = numCol)
      
      sample1$x <- xVal[1:sampleSize]
      sample1$y <- yVal[1:sampleSize]
      
      nBlue <- length(which(sample1$color == "blue"))
      
      storeSampleBlue(round(nBlue / sampleSize, 2))
      sampleReact(sample1)
      sizeReact(sampleSize)
      
      # print(sample1)
      
      output$machine <- renderPlot(
        expr = {
          data <- lab1Pop()
          FullCandyMachine <- storePlot() +
            geom_point(data = sample1,
                       mapping = aes(x = x, y = y, fill = color),
                       shape = 21,
                       size = 4,
                       color = "black") +
            scale_fill_manual(values = c("blue" = boastUtils::psuPalette[1],
                                          "red" = boastUtils::psuPalette[2])) +
            geom_hline(yintercept = -15, linewidth = 1, color = "black") +
            theme(legend.position = "none")
          
          storePlot1(FullCandyMachine)
          FullCandyMachine
          
        }
      )
      
      output$sampleFreq1 <- renderUI(
        expr = {
          paste("There are", nBlue, "blue-colored candies in your sample.")
        }
      )
      
      output$sampleProp1 <- renderUI(
        expr = {
          paste0("Your sample's proportion of blue-colored candies is ",
                storeSampleBlue(), ".")
        }
      )
      
      output$sampleSize1 <- renderUI(
        expr = {
          paste0("n = ", sampleSize)
        }
      )
    }
  )
  
  resampleFunction <- function(data, indices) {
    resampleData <- data[indices, ]
    return(resampleData)
  }
  
  firstBootSampleData <- reactiveVal(NULL) 
  secondBootSampleData <- reactiveVal(NULL) 
  latestBootSampleData <- reactiveVal(NULL)  # To store the latest resampled data
  proportionsAccumulated <- reactiveVal(NULL)
  
  firstInitialized <- reactiveVal(FALSE)
  secondInitialized <- reactiveVal(FALSE)

  # lists for samples and proportions
  firstBootSampleList <- list()
  secondBootSampleList <- list()
  latestBootSampleList <- list()
  
  clickVal <- reactiveValues(panelvar1 = NULL)
  
  # observe(
  #   x = {
  #     # Initially will be empty
  #     if (is.null(input$clicker)) {
  #       return()
  #     } 
  #     isolate(
  #       expr = {
  #         clickVal$panelvar1 <- input$clicker$panelvar1
  #       }
  #     )
  #   }
  # )
  
  
  #flag for highlighted plot in facet
  highlighted <- reactiveVal(NULL)
  

  bootPlots1 <- reactiveVal(NULL)
  bootPlotsOg <- reactiveVal(NULL)
  
  output$bootPlots <- renderPlot(
    expr = {
      NULL
    }
  )
  
  output$bootProportionsPlot <- renderPlot(
    expr = {
      NULL
    }
  )
  
  observeEvent(input$clicker, {
    # Check if the clicker input is not NULL
    if (!is.null(input$clicker)) {
      # Extract the panelvar1 value
      clickVal$panelvar1 <- input$clicker$panelvar1
      
      # Check if the highlighted value is NULL or different from the clicked value
      if (is.null(highlighted()) || highlighted() != clickVal$panelvar1) {
        # Highlight the specified region in the plot
        shadedRegion <- data.frame(
          type = clickVal$panelvar1,
          xmin = -8,
          xmax = 8,
          ymin = -22,
          ymax = 0
        )
        
        # Update the plot with the highlighted region
        bootPlots1(bootPlotsOg() +
                     geom_rect(data = shadedRegion, 
                               aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                               fill = boastUtils::boastPalette[2], alpha = 0.3, inherit.aes = FALSE)
        )
        
        # Update the highlighted value
        highlighted(clickVal$panelvar1)
      } else {
        # If the highlighted value is the same or NULL, remove the highlighting
        bootPlots1(bootPlotsOg())
        highlighted(NULL)
      }
    }
  })
  
  ## Draw the Boot sample  ----
  observeEvent(
    eventExpr = input$drawBoot,
    handlerExpr = {
    
      updateRadioGroupButtons(
        session = session,
        inputId = "sam",
        disabled = TRUE
      )
      
      updateButton(
        session = session,
        inputId = "drawSample",
        disabled = TRUE
      )
      
      sample1 <- sampleReact()
      sampleSize <- sizeReact()
      numBootSamp <- input$bootSamp
      
      proportionList <- list()

      # print(firstInitialized) 
      
      for (i in 1:numBootSamp) {
        sampleBoot <- boot(data = sample1, statistic = resampleFunction, R = 1)  # Perform one bootstrap
        resampleDataBoot <- sampleBoot$t
        
        # Calculate the proportion of blue points
        proportionBlue <- sum(resampleDataBoot[[1]] == "blue") / sampleSize
        proportionList[[i]] <- proportionBlue

        # Store the resampled data in the list
        # bootSampleList[[i]] <- resampleDataBoot
        # print(proportionBlue)
        
        if (i == 1) {
          firstBootSampleList[[i]] <- resampleDataBoot
          # firstInitialized(TRUE)
        } else if (i == 2) {
          secondBootSampleList[[i]] <- resampleDataBoot
          # secondInitialized(TRUE)
        } else {
          latestBootSampleList[[i]] <- resampleDataBoot
        }
      
        # print(latestBootSampleList)
      }
      
      # print(firstInitialized)
      # Store bootstrapped samples
      if (is.null(firstBootSampleData())) {
      firstBootSampleData(firstBootSampleList)
      secondBootSampleData(secondBootSampleList)
      }
      
      latestBootSampleData(latestBootSampleList)
      
      firstSampleDataList <- firstBootSampleData()
      secondSampleDataList <- secondBootSampleData()
      latestSampleDataList <- latestBootSampleData()
      
      
      if (!is.null(firstSampleDataList) & !is.null(secondSampleDataList) & !is.null(latestSampleDataList)) {
        
        firstSampleData <- firstSampleDataList[[length(firstSampleDataList)]]
        secondSampleData <- secondSampleDataList[[length(secondSampleDataList)]]
        lastSampleData <- latestSampleDataList[[length(latestSampleDataList)]]
        
        colorsFirst <- firstSampleData[[1]]
        colorsSecond <- secondSampleData[[1]]
        colorsLast <- lastSampleData[[1]]
        
         # print(colorsFirst)
        # print(colorsLast)
        
        numRow <- ceiling(sqrt(length(colorsLast)))
        numCol <- ceiling(length(colorsLast) / numRow)
        
        xVal <- rep(seq(from = -5, to = 5, length.out = numCol), each = numRow)
        yVal <- rep(seq(from = -20, to = -16, length.out = numRow), times = numCol)
        
        # Create a data frame for plotting
        firstBootSamplePlot <- data.frame(
          color = colorsFirst,
          x = xVal[1:sampleSize],
          y = yVal[1:sampleSize]
        ) 
        
        
        secondBootSamplePlot <- data.frame(
          color = colorsSecond,
          x = xVal[1:sampleSize],
          y = yVal[1:sampleSize]
        )
        
        latestBootSamplePlot <- data.frame(
          color = colorsLast,
          x = xVal[1:sampleSize],
          y = yVal[1:sampleSize]
        )
        
        nPlot <- data.frame(
          color = c(""),
          x = c(0),
          y = c(0)
        )
      }
      
      # print(latestBootSamplePlot)
      allData <- rbind(
        transform(firstBootSamplePlot, type = "First Bootstrap"),
        transform(secondBootSamplePlot, type = "Second Bootstrap"),
        transform(latestBootSamplePlot, type = "Last Bootstrap"),
        transform(nPlot, type = "..., num, ...")
      )
      
      # glimpse(allData)
      
      textLabels <- data.frame(
        type = c(
          "First Bootstrap",
          "Second Bootstrap",
          "Last Bootstrap",
          "..., num, ..."
        ),
        label = c(
          paste("Proportion of Blue: \n ", round(sum(colorsFirst == "blue") / sampleSize, 2), "\n\n Sample Size: \n", sampleSize),
          paste("Proportion of Blue: \n", round(sum(colorsSecond == "blue") / sampleSize, 2), "\n\n Sample Size: \n", sampleSize),
          paste("Proportion of Blue: \n", round(sum(colorsLast == "blue") / sampleSize, 2), "\n\n Sample Size: \n", sampleSize),
          ""
        )
      )
      
      # nTextLabel <- data.frame(
      #   type = c(
      #     "First Bootstrap",
      #     "Second Bootstrap",
      #     "Last Bootstrap",
      #     "..., num, ..."
      #   ),
      #   label = c(
      #     "",
      #     "",
      #     "",
      #     "..."
      #   )
      # )
      
      # ggplot(latestBootSamplePlot, aes(x = x, y = y, fill = color)) +
      # FullCandyMachine <- storePlot1() + 
      
      bootPlots <- ggplot(data = allData, mapping = aes(x = x, y = y, fill = color)) +
        geom_point(shape = 21, size = 4, color = "black") +
        scale_fill_manual(
          values = c(
            "blue" = boastUtils::psuPalette[1],
            "red" =  boastUtils::psuPalette[2],
            "black" = boastUtils::boastPalette[5]
          )
        ) +
        geom_hline(yintercept = -21, linewidth = 1, color = "black") +
        facet_grid(
          ~factor(
            type,
            levels = c(
              "First Bootstrap",
              "Second Bootstrap",
              "..., num, ...",
              "Last Bootstrap"
            )
          )
        ) +
        coord_cartesian(
          xlim = c(-8, 8),
          ylim = c(-25, -6),  
          expand = FALSE
        ) +
        theme_void() +
        theme(legend.position = "none") +
        theme(strip.text.x = element_text(size = 18)) +
        geom_text(data = textLabels, x = 0, y = -10, aes(label = label), size = 5, inherit.aes = FALSE) 
        # geom_text(data = nTextLabel, x = 0, y = -17, aes(label = label), size = 28, inherit.aes = FALSE) 
        
      bootPlots1(bootPlots)
      bootPlotsOg(bootPlots)
      
      # observeEvent(
      #   eventExpr = input$clicker, 
      #   handlerExpr = {
      #     # print(highlighted())
      #     
      #     shadedRegion <- data.frame(
      #       type = clickVal$panelvar1,
      #       xmin = -8,
      #       xmax = 8,
      #       ymin = -22,
      #       ymax = 0
      #     )
      #     
      #     
      #     print(highlighted())
      #     print(clickVal$panelvar1)
      #     
      #     if (is.null(highlighted()) || highlighted() != clickVal$panelvar1) {
      #       
      #       bootPlots <- bootPlots +
      #         geom_rect(data = shadedRegion, 
      #                   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,),
      #                   fill = boastUtils::boastPalette[2], alpha = 0.3, inherit.aes = FALSE)
      #       
      #       bootPlots1(bootPlots)
      #       highlighted(clickVal$panelvar1)
      #       
      #     } else {
      #       
      #       bootPlots1(bootPlots)
      #       highlighted(NULL)
      #     }
      #     
      #   }
      # )
     
      output$bootPlots <- renderPlot(
        expr = {
          bootPlots1()
        }
      )

      # Update the accumulated proportions ###REMOVE Accumulation property###
      # if (!is.null(proportionsAccumulated())) {
      #   accumulatedProportions <- c(proportionsAccumulated(), sapply(proportionList, identity))
      #   proportionsAccumulated(accumulatedProportions)
      # } else {
      #   proportionsAccumulated(sapply(proportionList, identity))
      # }
      proportionsAccumulated(sapply(proportionList, identity))
      ####
      
      output$bootProportionsPlot <- renderPlot(
        expr = {
          proportions <- proportionsAccumulated()
          # print(proportions)
          if (!is.null(proportions)) {
            proportionDf <- data.frame(Proportion = proportions, numberBoot = 1:(length(proportions)))
            
            
            proportionDf$numberBoot <- ifelse(proportionDf$numberBoot == 1, "First Bootstrap",
                                              ifelse(proportionDf$numberBoot == 2, "Second Bootstrap",
                                                     ifelse(proportionDf$numberBoot == length(proportions), "Last Bootstrap", proportionDf$numberBoot)))
            
            # print(proportionDf)
            # print(length(proportionDf$numberBoot))
            
            if (is.null(clickVal$panelvar1)) {
              # Default 'selected' to "No" for all rows if clickVal$panelvar1 is NULL
              proportionDf$selected <- "No"
            } else {
              if (!is.null(highlighted()) && highlighted() == clickVal$panelvar1) {
              proportionDf <- proportionDf %>%
                mutate(selected = ifelse(abs(Proportion - Proportion[which(numberBoot == clickVal$panelvar1)]) <= 0.05, "Yes", "No"))
              
              
              } else if (clickVal$panelvar1 == "..." ) {
                proportionDf$selected <- "No"
                proportionDf$selected[sample(3:(length(proportionDf) - 1), 1)] <- "Yes"
                
                
              } else {
                proportionDf$selected <- "No"
              }
            }
            
            # print(proportionDf)
            
            alpha <- 0.05
            z <- qnorm(1 - alpha/2)
            propMean <- mean(proportionDf$Proportion)
            sdData <- sd(proportionDf$Proportion)
            n <- length(proportionDf$Proportion)
            
            confInt <- propMean + c(-z, z) * sdData / sqrt(n)
            
            popDat <- input$prop
            sampDat <-  storeSampleBlue()
            
            breaks <- seq(0,1, by = 0.05)
            
              p <- ggplot(proportionDf, aes(x = Proportion, fill = selected)) +
                # Note for Binwidth: *can set center and boundaries closed left/right
                
                geom_histogram(
                  breaks = breaks,
                  # binwidth = 0.05,
                  color = "black",
                  alpha = 0.7
                ) +
                labs(title = "Distribution of Proportions of Blue in Bootstrapped Samples",
                     x = "Proportion of Blue",
                     y = "Frequency") +
                # geom_segment(
                #   x = confInt[1],
                #   xend = confInt[1],
                #   y = 0,
                #   yend = Inf,
                #   color = "red",
                #   linetype = "dotted"
                # ) +
                # geom_segment(
                #   x = confInt[2],
                #   xend = confInt[2],
                #   y = 0,
                #   yend = Inf,
                #   color = "red",
                #   linetype = "dotted"
                # ) +
                scale_fill_manual(values = c("No" = boastUtils::boastPalette[8], "Yes" = boastUtils::boastPalette[2]), guide = "none") +
                theme_minimal() +
                theme(
                  plot.title = element_text(size = 18),
                  axis.title = element_text(size = 14)
                ) +
                theme(legend.position = "None")
                
              p + 
                geom_vline(
                  mapping = aes(xintercept = popDat, color = "Population"), 
                  linewidth = 1
                ) +
                geom_vline(
                  mapping = aes(xintercept = sampDat, color = "Original Sample"), 
                  linewidth = 1
                ) +
                scale_color_manual(
                  name = "",
                  values = c(
                    "Population" = boastUtils::boastPalette[5], 
                    "Original Sample" = boastUtils::psuPalette[2]
                  )
                ) +
                theme(
                  legend.position = "bottom",
                  legend.text = element_text(size = 18)
                )
              # }
          }
        }
      )
    }
  )
  
  observeEvent(
    eventExpr = input$reset,
    handlerExpr = {
      
      updateRadioGroupButtons(
        session = session,
        inputId = "sam",
        disabled = FALSE
      )
      
      updateButton(
        session = session,
        inputId = "drawSample",
        disabled = FALSE
      )
      
      updateButton(
        session = session,
        inputId = "drawBoot",
        disabled = TRUE
      )
      
      proportionsAccumulated(NULL)
      firstBootSampleData(NULL)
      secondBootSampleData(NULL)
      latestBootSampleData(NULL)
      storePlot1(NULL)
      storeSampleBlue(NULL)
      highlighted(NULL)
      
      output$machine <- renderPlot(
        expr = {
          
              data <- lab1Pop()
              
              candyBox <- ggplot(data = data,
                                 mapping = aes(x = x, y = y, color = color, shape = color)) +
                geom_point(size = 4, aes(fill = color), shape = 21, color = "black") +  
                scale_fill_manual(
                  values = c("blue" = boastUtils::psuPalette[1], "red" =  boastUtils::psuPalette[2])
                ) +
                geom_hline(yintercept = -0.5, linewidth = 2, color = "black") +
                theme_bw() +
                theme(
                  legend.position = "none",
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  plot.margin = unit(rep(0, 4), "cm") 
                ) +
                labs(title = NULL) +
                xlab(NULL) +
                ylab(NULL)
              
              candyMachineBody <- ggplot() +
                geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill =  boastUtils::psuPalette[2], color = "black") +
                theme_void() +
                geom_rect(aes(xmin = -3, xmax = 3, ymin = -8, ymax = -5), fill = "black") +
                theme_void()
              
              
              FullCandyMachine <- candyMachineBody +
                annotation_custom(ggplotGrob(candyBox), xmin = -5, xmax = 5, ymin = 0, ymax = 7) +
                annotation_custom(CoinSlot, xmin = -4, xmax = 4, ymin = -6, ymax = 0.5) +
                coord_cartesian(
                  xlim = c(-12, 12),
                  expand = FALSE
                )
              
              storePlot(FullCandyMachine)
              
              FullCandyMachine
        }
      )
      output$bootPlots <- renderPlot(NULL)
      
      output$sampleFreq1 <- renderUI(
        expr = {
          paste0(NULL)
        }
      )
      
      output$sampleProp1 <- renderUI(
        expr = {
          paste0(NULL)
        }
      )
      
      output$sampleSize1 <- renderUI(
        expr = {
          paste0(NULL)
        }
      )
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
