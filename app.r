library(shiny)
library(ggplot2)
library(reshape2)

#exploration


patient <- read.table("Random_PatientLevelInfo_2020.tsv",
                      sep = "\t", stringsAsFactors = F,
                      header = T)
labVals <- read.table("Random_LabValuesInfo_2020.tsv",
                      sep = "\t", stringsAsFactors = F,
                      header = T)
colnames(patient) <- c("studyId",
                       "userId",
                       "age",
                       "sex",
                       "race",
                       "treatment",
                       "arm")
colnames(labVals) <- c("studyId",
                       "userId",
                       "bio1",
                       "bio2",
                       "testShort",
                       "test",
                       "type",
                       "value",
                       "unit",
                       "visit")
patient[!patient$sex %in% c("M", "F"),]$sex <- "U"

demoPlots <- c("age", "race", "sex", "sexAge", "sexRace", "ageRace")
names(demoPlots) <- c("Age", "Race", "Sex", "Age by Sex", "Race by Sex", "Age by Race")

patient$shortRace <- patient$race
patient[patient$shortRace == "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",]$shortRace  <-
  "Native Hawaiian/\nPacific Islander"
patient[patient$shortRace == "AMERICAN INDIAN OR ALASKA NATIVE",]$shortRace <-
  "Am. Indian/\n AK Native"
patient[patient$shortRace == "WHITE",]$shortRace = "White"
patient[patient$shortRace == "BLACK OR AFRICAN AMERICAN",]$shortRace = "Black/AfAm"
patient[patient$shortRace == "ASIAN",]$shortRace = "Asian"
patient[patient$shortRace == "MULTIPLE",]$shortRace = "Multiple"
patient[patient$shortRace == "OTHER",]$shortRace = "Other"


fullData  <- merge(patient, labVals, by = c("studyId", "userId"))

fullData[fullData$visit == "SCREENING",]$visit <- "Screening"
fullData[fullData$visit == "BASELINE",]$visit <- "Baseline"
fullData$visit <- gsub("WEEK", "Week", fullData$visit)
fullData$visit <- gsub("DAY", "Day", fullData$visit)
fullData$visitFactor <- gsub(" Day", "\nDay", fullData$visit)



fullData$visitFactor <- factor(fullData$visitFactor,
                                 levels = c("Screening", "Baseline",
                                            "Week 1\nDay 8", "Week 2\nDay 15",
                                            "Week 3\nDay 22", "Week 4\nDay 29",
                                            "Week 5\nDay 36"))

ui <- fluidPage(
  titlePanel('R Shiny Exercise: Cameron Gilbert'),
  hr(),
  
  #The demographic summary area allows the user to see high-level information
  #about the patients that participated in the study. 
  
  #Plots are set to display at the click of a button, and the user
  #can download the plot with a second button. 
  
  h2('Demographic summary'),
  
  sidebarLayout(
    sidebarPanel(radioButtons("demArm", "Which arm to examine?", 
                              selected = "All",
                              inline = FALSE, width = NULL, 
                              choiceNames = c("All", "Drug X", "Placebo", "Combination"),
                              choiceValues = c("all", "ARM A", "ARM B", "ARM C")),
                 
                 uiOutput("demArmOrOverall"),
                 
                 selectInput("demType",
                             label = "Choose a plot to examine",
                             choices = demoPlots, width = "50%",
                             selected = "age",
                             multiple = F
                 ),
                 
                 actionButton("goDemo", label = "Update Demographics"),
                 hr(),
                 downloadButton("downPlot1", label = "Download Plot")
    ),
    
    mainPanel(plotOutput('demSum'))
  ),
  
  hr(),
  
  h2("Treatment time series"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("timeType", "Break down in which way?", 
                   selected = "allArms",
                   inline = FALSE, width = NULL, 
                   choiceNames = c("Single test, all arms", "Single arm, all tests"),
                   choiceValues = c("allArms", "allTests")),
      uiOutput("armTestSelector"),
      
      radioButtons("extraFilters", "Apply additional demographic filters?",
                   selected = F, inline = F, width = NULL,
                   choiceNames = c("No", "Yes"), choiceValues = c(F, T)),
      
      uiOutput("ageFilter"),
      uiOutput("sexFilter"),
      uiOutput("biomarker1Filter"),
      uiOutput("biomarker2Filter"),
      
      
      
      actionButton("goTime", label = "Update Time Series"),
      hr(),
      downloadButton("downPlot2", label = "Download Plot"),
      hr(),
      downloadButton("downTimeData", label = "Download Underlying Dataset")
      
      
    ),
    mainPanel(plotOutput('timePlot'))
  ),
  hr()
  
  
    
)




server <- function(input, output) {
  
  rv <- reactiveValues(armToPlot = "awaitingInput",
                       armOrOverall = "Overall",
                       demType = NULL,
                       timeType = "allArms",
                       armTestButtons = NULL,
                       extraFilters = F,
                       minmaxAge = c(10, 80),
                       sexToPlot = c("M", "F", "U"),
                       bio1ToPlot = c(0,25),
                       bio2ToPlot = c("HIGH", "MEDIUM", "LOW"))
  
  
  armLabeler = as_labeller(c("ARM A" = "Drug X", 
                           "ARM B" = "Placebo",
                           "ARM C" = "Combination"))

  observeEvent(input$goDemo, { rv$demType <- input$demType })
  
  ## Demographic summary
  
  
  observeEvent(input$demArm, { rv$armToPlot <- input$demArm })
  
  output$demArmOrOverall <- renderUI({
    if(rv$armToPlot == "all"){
      radioButtons("armOrOverall", "Plot overall demographics or break down by study arm?",
                   choices = c("Overall", "By Arm"), selected = "Overall")
    }
  })
  
  observeEvent(input$armOrOverall, {rv$armOrOverall <- input$armOrOverall})
  
  
  #choosing to do an overall plot by arm will trigger the use of facet plots
  
  
  demPlotInput <- eventReactive(
    input$goDemo, {
      armLabel <- switch(rv$armToPlot,
                         "all" = "(All participants)",
                         "ARM A" = "(Received Drug X)",
                         "ARM B" = "(Received placebo)",
                         "ARM C" = "(Received combination)")
      
      if(rv$armToPlot == "all") {
        plotPatient <- patient
        } else {
        plotPatient <- patient[patient$arm == rv$armToPlot,]
      }
      if(rv$demType == 'age'){
        if(rv$armToPlot == "all"){
          if(rv$armOrOverall == "Overall") {
            p <- ggplot(plotPatient, aes(x = age)) + 
              geom_histogram(binwidth = 5, color = "black", fill = "orange") +
            labs(title = paste0("Participant age ", armLabel), 
                                x = "Participant age", y = 'Number of individuals')
            p
          } else {
            p <- ggplot(plotPatient, aes(x = age)) +
              geom_histogram(binwidth = 5, color = 'black', fill = 'orange') +
              facet_grid(arm ~ ., labeller = armLabeler)  + 
              labs(title = paste0("Participant age by treatment arm"), 
              x = "Participant age", y = 'Number of individuals')
            p
          }
        } else {
          p <- ggplot(plotPatient, aes(x = age)) + 
            geom_histogram(binwidth = 5, color = "black", fill = "orange") +
            labs(title = paste0("Participant age ", armLabel), 
                 x = "Participant age", y = 'Number of individuals')
          p
        }
      }
      
      else if(rv$demType == 'sexAge'){
        if(rv$armToPlot == "all"){
          
          if(rv$armOrOverall == "Overall") {
            
            p <- ggplot(plotPatient, aes(x = age, fill = sex)) +
              geom_histogram(position = "dodge", binwidth = 5, alpha= 1) +
              labs(title = paste0("Participant age ", armLabel, ", grouped by sex"), 
                   x = "Participant age", y = 'Number of individuals')
            p
          } else {
            p <- ggplot(plotPatient, aes(x = age, fill = sex)) +
              geom_histogram(binwidth = 5, alpha = 1, position = "dodge") +
              facet_grid(arm ~ ., labeller = armLabeler)  + 
              labs(title = paste0("Participant age by treatment arm, grouped by sex"), 
                   x = "Participant age", y = 'Number of individuals')
            p
          }
        } else {
            p <- ggplot(plotPatient, aes(x = age, fill = sex)) + 
              geom_histogram(binwidth = 5, position = "dodge", alpha =1) +
              labs(title = paste0("Participant age ", armLabel, ", grouped by sex"), 
                   x = "Participant age", y = 'Number of individuals')
            p
        }
      }
      
      else if(rv$demType == 'ageRace'){
        if(rv$armToPlot == "all"){
          
          if(rv$armOrOverall == "Overall"){
            
            p <- ggplot(plotPatient, aes(x = shortRace, y = age, fill = shortRace)) +
              geom_boxplot(position = "dodge")+
              theme(legend.position = "none") +
              labs(title = paste0("Participant age distribution by race ", armLabel), 
                   x = "", y = 'Participant age')
            p
          } else {
            p <- ggplot(plotPatient, aes(x = shortRace, y = age, fill = shortRace)) +
              geom_boxplot(position = "dodge")+
              theme(legend.position = "none") +
              labs(title = paste0("Participant age distribution by race ", armLabel), 
                   x = "", y = 'Participant age') + 
              facet_grid(arm~., labeller = armLabeler)
            p
          }
        } else {
          
          p <- ggplot(plotPatient, aes(x = shortRace, y = age, fill = shortRace)) +
            geom_boxplot(position = "dodge") +
            theme(legend.position = "none") +
            labs(title = paste0("Participant age distribution by race ", armLabel), 
                 x = "", y = 'Participant age')
          p
        }
        
      }
      
      else if(rv$demType == "sex"){
        
        if(rv$armToPlot == "all") {
          
          if(rv$armOrOverall == "Overall"){
            p <- ggplot(plotPatient, aes(x= sex, fill = sex))+geom_bar() + 
              labs(x="", y = "Number of participants",
                   title = paste0("Sex breakdown of study participants ", armLabel))+
              theme(legend.position = "none")
            p
          } else {
            p <- ggplot(plotPatient, aes(x= sex, fill = sex))+geom_bar() + 
              labs(x="", y = "Number of participants",
                   title = paste0("Sex breakdown of study participants ", armLabel))+
              theme(legend.position = "none") + 
              facet_grid(arm~., labeller = armLabeler)
            p
          }
          
        } else {
          p <- ggplot(plotPatient, aes(x= sex, fill = sex))+geom_bar() + 
            labs(x="", y = "Number of participants",
                 title = paste0("Sex breakdown of study participants ", armLabel))+
            theme(legend.position = "none")
          p
          
        } 
      
      }
      else if(rv$demType == "race") {
        if(rv$armToPlot == "all") {
         if(rv$armOrOverall == "Overall") {
           p <- ggplot(plotPatient, aes(x= shortRace, fill = shortRace))+geom_bar() + 
             labs(x="", y = "Number of participants",
                  title = paste0("Racial breakdown of study participants ", armLabel))+
             theme(legend.position = "none")
           p
           
         } else {
           p <- ggplot(plotPatient, aes(x= shortRace, fill = shortRace))+geom_bar() + 
             labs(x="", y = "Number of participants",
                  title = paste0("Racial breakdown of study participants ", armLabel))+
             theme(legend.position = "none") + 
             facet_grid(arm~., labeller = armLabeler)
           p
         }
        } else {
          p <- ggplot(plotPatient, aes(x= shortRace, fill = shortRace))+geom_bar() + 
            labs(x="", y = "Number of participants",
                 title = paste0("Racial breakdown of study participants ", armLabel))+
            theme(legend.position = "none")
          p
          
        }
      } else if (rv$demType == "sexRace"){
        if(rv$armToPlot == "all") {
          if(rv$armOrOverall == "Overall"){
            p <- ggplot(plotPatient, aes(x= shortRace, fill = sex)) +
              geom_bar(position = "dodge") + 
              labs(x = '', y = "Number of participants",
                   title = paste0("Racial breakdown of study participants, split by sex ", armLabel))
  
            p
          } else {
            p <- ggplot(plotPatient, aes(x= shortRace, fill = sex)) +
              geom_bar(position = "dodge") + 
              labs(x = '', y = "Number of participants",
                  title = paste0("Racial breakdown of study participants, split by sex ", armLabel)) +
              facet_grid(arm~., labeller = armLabeler)
          
            p
          }
        } else {
          p <- ggplot(plotPatient, aes(x= shortRace, fill = sex)) +
            geom_bar(position = "dodge") + 
            labs(x = '', y = "Number of participants",
                 title = paste0("Racial breakdown of study participants, split by sex ", armLabel))
          
          p
        }
      }
    }
  )
  
  output$demSum <- renderPlot({
    print(demPlotInput())
  })
  
  output$downPlot1 <- downloadHandler(
    filename = function() {
      paste(rv$demType, "DemographicSummary.png", sep = "_")
    },
    content = function(file) {
      png(file, width = 1920, height = 1080)
      print(demPlotInput())
      dev.off()
    }
  )
  
  
  
  ###########Time Series (Server) ##############
  
  
  observeEvent(input$timeType, {rv$timeType <- input$timeType})
  
  
  output$armTestSelector <- renderUI({
    if(rv$timeType == "allArms") {
      radioButtons("armTestButtons", "Select which test results to visualize",
                   choiceNames = unique(labVals$test), choiceValues = unique(labVals$testShort),
                   selected = unique(labVals$testShort)[1])
    } else if(rv$timeType == "allTests"){
      radioButtons("armTestButtons", "Select which treatment arm to visualize",
                   choiceNames = c("Drug X", "Placebo", "Combination"),
                   choiceValues = c("ARM A", "ARM B", "ARM C"),
                   selected = "ARM A")
    }
  })
  
  observeEvent(input$armTestButtons, {rv$armTestButtons <- input$armTestButtons})
  
  observeEvent(input$extraFilters, {
    rv$extraFilters <- input$extraFilters})
  
  output$ageFilter <- renderUI({
    if(rv$extraFilters){
      sliderInput("minmaxAge", "Choose age range",
                  min = 10, max = 80, value = c(10,80))
    }
  })
  
  observeEvent(input$minmaxAge, {rv$minmaxAge <- input$minmaxAge} )
  output$sexFilter <- renderUI({
    if(rv$extraFilters){
      checkboxGroupInput("sexToPlot", "Choose which patients to include",
                   choiceNames = c("Male", "Female", "Undifferentiated"), 
                   choiceValues = c("M", "F", "U"),
                   selected = c("M", "F", "U"))
    }
  })
  observeEvent(input$sexToPlot, {rv$sexToPlot <- input$sexToPlot} )
  
  output$biomarker1Filter <- renderUI({
    if(rv$extraFilters){
      sliderInput("bio1ToPlot", "Choose range for Biomarker 1 (constant across all timepoints)",
                  min = 0, max = 25, value = c(0,25))
    }
  })
  
  observeEvent(input$bio1ToPlot, {rv$bio1ToPlot <- input$bio1ToPlot} )
  
  
  output$biomarker2Filter <- renderUI({
    if(rv$extraFilters){
      checkboxGroupInput("bio2ToPlot", "Choose values for Biomarker 2 (constant across all timepoints)",
                         choiceNames = c("High", "Medium", "Low"), 
                         choiceValues = c("HIGH", "MEDIUM", "LOW"),
                         selected = c("HIGH", "MEDIUM", "LOW"))
    }
  })
  observeEvent(input$bio2ToPlot, {rv$bio2ToPlot <- input$bio2ToPlot} )
  
  timePlotInput <- eventReactive(input$goTime, {
    
    toPlotData <- fullData

    if(rv$extraFilters){
      toPlotData <- toPlotData[toPlotData$sex  %in% rv$sexToPlot,]
      toPlotData <- toPlotData[toPlotData$age >= rv$minmaxAge[1],]
      toPlotData <- toPlotData[toPlotData$age <= rv$minmaxAge[2],]
      toPlotData <- toPlotData[toPlotData$bio1 >= rv$bio1ToPlot[1],]
      toPlotData <- toPlotData[toPlotData$bio1 <= rv$bio1ToPlot[2],]
      toPlotData <- toPlotData[toPlotData$bio2 %in% rv$bio2ToPlot,]
      
      if(length(rv$sexToPlot) == 3) {
        sexFilter = "All sexes"
      } else {
        sexFilter = paste("Sexes:", paste(rv$sexToPlot, collapse = ", "))
      }
      
      if(rv$minmaxAge[1] == 10 & rv$minmaxAge[2] == 80){
        ageFilter = "all ages"
      } else {
        ageFilter = paste("age between", rv$minmaxAge[1],
                          "-", rv$minmaxAge[2])
      }
      
      if(rv$bio1ToPlot[1] == 0 & rv$bio1ToPlot[2] == 25) {
        bio1Filter <- "all values for biomarker 1"
      } else {
        bio1Filter <- paste("biomarker 1 between", rv$bio1ToPlot[1],
                            " - ", rv$bio1ToPlot[2])
      }
      
      if(length(rv$bio2ToPlot) == 3){
        bio2Filter = "all values for biomarker 2"
      } else {
        bio2Filter <- paste("biomarker 2: ", paste(rv$bio2ToPlot, collapse = ", "))
      }
      
      fullFilter <- paste(sexFilter, ageFilter,
                          bio1Filter, bio2Filter, sep = ", ")
    }
    
    
    
    if(rv$timeType == "allArms"){
      
      test <- rv$armTestButtons
      
      toPlotData <- toPlotData[toPlotData$testShort == test,]
      unit <- toPlotData$unit[1]
      
      testLabel <- toPlotData$test[1]
      
      if(rv$extraFilters == F) {
        fullFilter <- ""
      }
      
      p <- ggplot(toPlotData, aes(x = visitFactor,
                                  y = value,
                                  fill = arm)) +
        geom_boxplot() + facet_grid(arm~., labeller = armLabeler) +
        theme(legend.position = "none", text = element_text(size = rel(5)), 
              plot.title = element_text(size = rel(4))) +
        labs(x = '', y = paste0("Test result (", unit, ")"),
             title = paste0(test, " results for all treatment arms\n", fullFilter)
            ) 
      
      
      p
    } else if(rv$timeType == "allTests"){
      
      
      arm <- rv$armTestButtons
      
      toPlotData <- toPlotData[toPlotData$arm == arm,]
      
      armTimeLabel <-   switch(arm,
                                      "ARM A" = "Drug X",
                                      "ARM B" = "Placebo",
                                      "ARM C" = "Combination")
      
      if(rv$extraFilters == F) {
        fullFilter <- ""
      }
      timeTitle <- paste0("All test results, ", armTimeLabel, "\n", fullFilter)
      
      
      p <- ggplot(toPlotData, aes(x = visitFactor,
                                  y = value,
                                  fill = unit)) +
        geom_boxplot() + facet_grid(testShort~.) +
        theme(text = element_text(size = rel(5)), 
               plot.title = element_text(size = rel(4)),
              legend.text = element_text(size = rel(5))) +
        labs(x = '', y = "Test result (see legend for unit)",
             title = timeTitle
        )
      
      
      p
      
      
      
      
      
    }
  })
  
  output$timePlot <- renderPlot({
    print(timePlotInput())
  })
  
  
  output$downPlot2 <- downloadHandler(
    filename = function() {
      paste(rv$timeType, "TreatmentTimeSeries", Sys.time(), ".png", sep = "_")
    },
    content = function(file) {
      png(file, width = 1920, height = 1080)
      print(timePlotInput())
      dev.off()
    }
  )
  
  output$downTimeData <- downloadHandler(
    filename = function() {
      "rShinyExercise - filteredTimeSeriesData.csv"
    },
    content = function(file) {
      write.csv(file = file, toPlotData, row.names = F)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)