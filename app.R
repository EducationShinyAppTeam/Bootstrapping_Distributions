# Load Packages ----
library(shiny)
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

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "App Template", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "App_Template")
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
        menuItem("Game", tabName = "game", icon = icon("gamepad")),
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
          h1("Sample Application for BOAST Apps"), # This should be the full name.
          p("This is a sample Shiny application for BOAST. Remember, this page
            will act like the front page (home page) of your app. Thus you will
            want to have this page catch attention and describe (in general terms)
            what the user can do in the rest of the app."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Challenge yourself."),
            tags$li("Play the game to test how far you've come.")
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
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
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
            div(class = "updated", "Last Update: 11/8/2022 by NJH.")
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
          h2("Explore the Concept"),
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
                sliderInput(
                  inputId = "sam",
                  label = "Set Sample Size",
                  min = 10,
                  max = 50,
                  value = 30,
                  step = 1
                ),
                br(),
                bsButton(
                  inputId = "drawSample", 
                  label = "Draw Sample", 
                  icon = icon("paper-plane"),
                  class = "circle grow"
                ),
                br(),
                br(),
                numericInput(
                  inputId = "bootSamp",
                  label = "Boot Samples",
                  min = 1,
                  max = 10000,
                  value = 1
                ),
                br(),
                bsButton(
                  inputId = "drawBoot", 
                  label = "Draw BootStrap Sample", 
                  icon = icon("paper-plane"),
                  class = "circle grow",
                  disabled = TRUE
                )
              )
            ),
            column(
              width = 3,
              plotOutput("machine"),
              uiOutput("sampleFreq1"),
              uiOutput("sampleProp1"),
              uiOutput("sampleSize1")
            ),
            column(
              width = 4,
              h3("Latest Bootstrap Sample"),
              br(),
              plotOutput("latestBootGridPlot"),
              br(),
              h3("Distribution of Proportions"),
              plotOutput("bootProportionsPlot")
            )
          )
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
  grob_img <- rasterGrob(img, interpolate = TRUE)
  
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
  
  output$machine <- renderPlot(
    expr = {
      data <- lab1Pop()
      
      p <- ggplot(data = data,
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
      
      additionalLayer <- ggplot() +
        geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill =  boastUtils::psuPalette[2], color = "black") +
        theme_void()
      
      additionalLayer2 <- ggplot() +
        geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill = "black") +
        theme_void()
      
      combinedPlot <- additionalLayer +
        annotation_custom(ggplotGrob(p), xmin = -5, xmax = 5, ymin = 0, ymax = 7) +
        annotation_custom(ggplotGrob(additionalLayer2), xmin = -3, xmax = 3, ymin = -8, ymax = -5 ) +
        annotation_custom(grob_img, xmin = -4, xmax = 4, ymin = -6, ymax = 0.5) 
      
      combinedPlot
    }
  )
  
  sampleReact <- reactiveVal()
  sizeReact <- reactiveVal()
  
  ## Draw the sample for blue/red ----
  observeEvent(
    eventExpr = input$drawSample, 
    handlerExpr = {
      
      updateButton(
        session = session,
        inputId = "drawBoot",
        disabled = FALSE
      )
      
      sampleSize <- input$sam
      sampleData <- lab1Pop()
      sample1 <- sampleData[sample.int(nrow(sampleData), size = sampleSize), ]
      numRow <- ceiling(sqrt(sampleSize))
      numCol <- ceiling(sampleSize / numRow)
      
      xVal <- rep(seq(from = -5, to = 5, length.out = numCol), each = numRow)
      yVal <- rep(seq(from = -10, to = -14, length.out = numRow), times = numCol)
      
      sample1$x <- xVal[1:sampleSize]
      sample1$y <- yVal[1:sampleSize]
      
      nBlue <- length(which(sample1$color == "blue"))
      
      sampleReact(sample1)
      sizeReact(sampleSize)
      
      output$machine <- renderPlot(
        expr = {
          data <- lab1Pop()
          
          p <- ggplot(data = data,
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
          
          additionalLayer <- ggplot() +
            geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill =  boastUtils::psuPalette[2], color = "black") +
            theme_void()
          
          additionalLayer2 <- ggplot() +
            geom_rect(aes(xmin = -6, xmax = 6, ymin = -8, ymax = 8), fill = "black") +
            theme_void()
          
          combinedPlot <- additionalLayer +
            annotation_custom(ggplotGrob(p), xmin = -5, xmax = 5, ymin = 0, ymax = 7) +
            annotation_custom(ggplotGrob(additionalLayer2), xmin = -3, xmax = 3, ymin = -8, ymax = -5 ) +
            annotation_custom(grob_img, xmin = -4, xmax = 4, ymin = -6, ymax = 0.5) 
          
          combinedPlot <- combinedPlot +
            geom_point(data = sample1,
                       mapping = aes(x = x, y = y, fill = color),
                       shape = 21,
                       size = 4,
                       color = "black") +
            scale_fill_manual(values = c("blue" = boastUtils::psuPalette[1],
                                          "red" = boastUtils::psuPalette[2])) +
            geom_hline(yintercept = -15, linewidth = 1, color = "black") +
            theme(legend.position = "none")
          
          combinedPlot
          
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
                 round(nBlue / sampleSize, 2), ".")
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
  
  latestBootSampleData <- reactiveVal(NULL)  # To store the latest resampled data
  proportionsAccumulated <- reactiveVal(NULL)
  
  ## Draw the Boot sample  ----
  observeEvent(
    eventExpr = input$drawBoot, 
    handlerExpr = {
      sample1 <- sampleReact()
      sampleSize <- sizeReact()
      numBootSamp <- input$bootSamp
      
      # lists for samples and proportions
      bootSampleList <- list()
      proportionList <- list()
      
      for (i in 1:numBootSamp) {
        sampleBoot <- boot(data = sample1, statistic = resampleFunction, R = 1)  # Perform one bootstrap
        resampleDataBoot <- sampleBoot$t
        
        # Calculate the proportion of blue points
        proportionBlue <- sum(resampleDataBoot[[1]] == "blue") / sampleSize
        proportionList[[i]] <- proportionBlue
        
        # Store the resampled data in the list
        bootSampleList[[i]] <- resampleDataBoot
        # print(proportionBlue)
        
      }
      
      # Store bootstrapped samples
      latestBootSampleData(bootSampleList)
      
      output$latestBootGridPlot <- renderPlot(
        expr = {
          sampleDataList <- latestBootSampleData()
          
          if (!is.null(sampleDataList)) {
            lastSampleData <- sampleDataList[[length(sampleDataList)]]
            
            colors <- lastSampleData[[1]]
            
            numRow <- ceiling(sqrt(length(colors)))
            numCol <- ceiling(length(colors) / numRow)
            
            xVal <- rep(seq(from = -5, to = 5, length.out = numCol), each = numRow)
            yVal <- rep(seq(from = -10, to = -14, length.out = numRow), times = numCol)
            
            # Create a data frame for plotting
            latestBootSamplePlot <- data.frame(
              color = colors,
              x = xVal,
              y = yVal
            )
            
            ggplot(latestBootSamplePlot, aes(x = x, y = y, fill = color)) +
              geom_point(shape = 21, size = 4, color = "black") +
              scale_fill_manual(values = c("blue" = boastUtils::psuPalette[1], "red" = boastUtils::psuPalette[2])) +
              geom_hline(yintercept = -15, linewidth = 1, color = "black") +
              theme_void() +
              theme(legend.position = "none")
          }
        }
      )
      
      # Update the accumulated proportions
      if (!is.null(proportionsAccumulated())) {
        accumulatedProportions <- c(proportionsAccumulated(), sapply(proportionList, identity))
        proportionsAccumulated(accumulatedProportions)
      } else {
        proportionsAccumulated(sapply(proportionList, identity))
      }
      
      output$bootProportionsPlot <- renderPlot(
        expr = {
          proportions <- proportionsAccumulated()
          # print(proportions)
          if (!is.null(proportions)) {
            proportion_df <- data.frame(Proportion = proportions)
            
            if (length(proportions) <= 200) {
              # Create a dot plot for fewer points
              ggplot(proportion_df, aes(x = Proportion)) +
                geom_dotplot(binwidth = 0.03, dotsize = 1) +
                labs(title = "Distribution of Proportions of Blue in Bootstrapped Samples",
                     x = "Proportion of Blue",
                     y = "") +
                theme_minimal() +
                theme(
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()
                ) 
            } else {
              # Create a histogram for higher number of points
              ggplot(proportion_df, aes(x = Proportion)) +
                geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7) +
                labs(title = "Distribution of Proportions of Blue in Bootstrapped Samples",
                     x = "Proportion of Blue",
                     y = "Frequency") +
                theme_minimal()
            }
          }
        }
      )
    }
  )
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
