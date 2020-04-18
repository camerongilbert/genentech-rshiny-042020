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

demoPlots <- c("age", "race", "sex", "sexAge", "sexRace", "ageRace")
names(demoPlots) <- c("Age", "Race", "Sex", "Age by Sex", "Race by Sex", "Age by Race")

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
  )
    
)




server <- function(input, output) {
  
  rv <- reactiveValues(armToPlot = "all",
                       armOrOverall = "Overall",
                       demType = NULL)
  armLabeler = as_labeller(c("ARM A" = "Drug X", 
                           "ARM B" = "Placebo",
                           "ARM C" = "Combination"))

  observeEvent(input$goDemo, { rv$demType <- input$demType })
  
  ## Demographic summary
  
  
  observeEvent(input$demArm, { rv$armToPlot <- input$demArm })
  
  output$demArmOrOverall <- renderUI({
    if(rv$armToPlot == "All"){
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
      
      if(armLabel == "(All participants)") {plotPatient <- patient} else {
        plotPatient <- patient[patient$arm == rv$armToPlot,]
      }
      if(rv$demType == 'age'){
        
        if(rv$armOrOverall == "Overall") {
          p <- ggplot(plotPatient, aes(x = age)) + xlim(10,80) + 
            geom_histogram(binwidth = 5, color = "black", fill = "orange") +
          labs(title = paste0("Participant age ", armLabel), 
                              x = "Participant age", y = 'Number of individuals')
        } else {
          p <- ggplot(plotPatient, aes(x = age)) + xlim(10,80) +
            geom_histogram(binwidth = 5, color = 'black', fill = 'orange') +
            facet_grid(arm ~ ., labeller = armLabeler)  + 
            labs(title = paste0("Participant age by treatment arm"), 
            x = "Participant age", y = 'Number of individuals')
        }
      }
      
      if(rv$demType == 'sex'){
        
      }
      
      if(rv$demType == 'race'){
        
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
      png(file)
      print(demPlotInput())
      dev.off()
    }
  )
}


shinyApp(ui = ui, server = server)