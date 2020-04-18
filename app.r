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
