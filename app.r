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
  
  rv <- reactiveValues(armToPlot = "awaitingInput",
                       armOrOverall = "Overall",
                       demType = NULL)
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
  #test
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