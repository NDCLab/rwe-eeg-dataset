# rwe-eeg-dataset PsychoPy Preprocessing for Valence Reading Task
# Author: Jessica M. Alexander
# Last Updated: 2022-08-19
### SECTION 1: SETTING UP
#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
input_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/sourcedata/checked/psychopy/'
out_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/'

#identify participant folders within input dir
sub_folders <- list.files(input_path, pattern = "sub")

#create dataframes for storing output data and define output file names
readAloudValenceSummaryDat <- data.frame(matrix(ncol=2, nrow=0))
colnames(readAloudValenceSummaryDat) <- c("id", "challengeACC")
readAloudValence_out_subjectLevel <- paste("readAloud_valence_subject-level_summary_", today, ".csv", sep="", collapse=NULL)

readAloudValenceChallengeDat <- data.frame(matrix(ncol=1, nrow=20))
colnames(readAloudValenceChallengeDat) <- c("passage")
readAloudValenceChallengeDat[1,1] <- "A dam is a"
readAloudValenceChallengeDat[2,1] <- "Air travel "
readAloudValenceChallengeDat[3,1] <- "Bats are th"
readAloudValenceChallengeDat[4,1] <- "Broccoli is"
readAloudValenceChallengeDat[5,1] <- "Buying a ho"
readAloudValenceChallengeDat[6,1] <- "Colony coll"
readAloudValenceChallengeDat[7,1] <- "Dog shows, "
readAloudValenceChallengeDat[8,1] <- "Dolphins ar"
readAloudValenceChallengeDat[9,1] <- "Even on fro"
readAloudValenceChallengeDat[10,1] <- "Gasoline-en"
readAloudValenceChallengeDat[11,1] <- "Las Vegas w"
readAloudValenceChallengeDat[12,1] <- "Lumens meas"
readAloudValenceChallengeDat[13,1] <- "Making cara"
readAloudValenceChallengeDat[14,1] <- "Six percent"
readAloudValenceChallengeDat[15,1] <- "The first e"
readAloudValenceChallengeDat[16,1] <- "The Great D"
readAloudValenceChallengeDat[17,1] <- "The great h"
readAloudValenceChallengeDat[18,1] <- "The grizzly"
readAloudValenceChallengeDat[19,1] <- "The mantis "
readAloudValenceChallengeDat[20,1] <- "Tooth decay"

readAloudValence_out_passageLevel <- paste("readAloud_Valence_passage-level_summary_", today, ".csv", sep="", collapse=NULL)


### SECTION 2: PARTICIPANT LOOP
#loop over participants (subfolders)
for(i in 1:length(sub_folders)){
  
  #for this participant, find the Valence csv file
  psychopy_file <- list.files(paste(input_path,sub_folders[i], sep = "", collapse = NULL), pattern = ".*read-aloud-val.*.csv")
  
  #logical to make sure there is a dccs file for this participant before loading, else skip to next participant
  if (!identical(psychopy_file, character(0))) {
    print(paste("Woohoo! Processing", sub_folders[i], "!"))
  
    #read in the data for this participant, establish id, and remove extraneous variables
    psychopyDat <- read.csv(file = paste(input_path,sub_folders[i],'/',psychopy_file, sep = "", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
    id <- psychopyDat$id[1]
    psychopyDatTrim <- psychopyDat[c("id",
                                     "firstListA",
                                     "secondListA",
                                     "challengeResponse1.corr",
                                     "challengeResponse2.corr")]

    #collapse the two passage lists into a single list,  trim passage text as passage identifier
    readAloudDatA <- psychopyDatTrim[!is.na(psychopyDatTrim$firstListA),]
    readAloudDatA$passageId <- substr(readAloudDatA$firstListA, 1, 11)
    readAloudDatA <- readAloudDatA[c("id", "passageId", "challengeResponse1.corr")]
    colnames(readAloudDatA) <- c("id", "passage", "challengeACC")
    readAloudDatB <- psychopyDatTrim[!is.na(psychopyDatTrim$secondListA),]
    readAloudDatB$passageId <- substr(readAloudDatB$secondListA, 1, 11)
    readAloudDatB <- readAloudDatB[c("id", "passageId", "challengeResponse2.corr")]
    colnames(readAloudDatB) <- c("id", "passage", "challengeACC")
    
    readAloudDat <- rbind(readAloudDatA, readAloudDatB)
    readAloudDat <- readAloudDat[order(readAloudDat$passage),]
        
    #order columns alphabetically by passage
    readAloudDat <- readAloudDat[order(readAloudDat$passage),]
    
    #calculate accuracy for individual participant
    challengeACC <- mean(readAloudDat$challengeACC)
    
    #store output data in summary matrices
    readAloudValenceSummaryDat[nrow(readAloudValenceSummaryDat) + 1,] <-c(id,challengeACC)
    
    newcol <- ncol(readAloudValenceChallengeDat) + 1
    readAloudValenceChallengeDat[newcol] <- readAloudDat$challengeACC
    names(readAloudValenceChallengeDat)[newcol] <- id
  }    
    
#if participant did not have a PsychoPy file, skip to next participant
  else {
  print(paste("Booo! Missing file for", sub_folders[i], "..."))
  }
}

### SECTION 3: READALOUD GROUP AVERAGE
#calculate group average for readAloudChallengeDat and append to dataframe
currentNoCols <- ncol(readAloudValenceChallengeDat)
newCol <- currentNoCols + 1
readAloudValenceChallengeDat[newCol] <- rowMeans(readAloudValenceChallengeDat[,2:currentNoCols])
names(readAloudValenceChallengeDat)[newCol] <- c("groupAvg")

### SECTION 4: OUTPUT DATA
#write the extracted summary scores to CSV
write.csv(readAloudValenceChallengeDat,paste(out_path,readAloudValence_out_passageLevel, sep = "", collapse = NULL), row.names=FALSE)
write.csv(readAloudValenceSummaryDat,paste(out_path,readAloudValence_out_subjectLevel, sep = "", collapse = NULL), row.names=FALSE)

### SECTION 5: UPDATE CENTRAL TRACKER FOR STUDY
#load central tracker
track_path <- '/home/data/NDClab/datasets/readAloud-valence-dataset/data-monitoring/central-tracker_rwe-eeg.csv'
trackerDat <- read.csv(track_path, header=TRUE, check.names=FALSE)

for (row in 1:nrow(readAloudValenceSummaryDat)) {
  accuracy <- readAloudValenceSummaryDat[row, "challengeACC"]
  id <- readAloudValenceSummaryDat[row, "id"]
  if (accuracy >= 0.7) {
    trackerDat[trackerDat$id == id, ]$dvalenceChallenge_s1_r1_e1 = "1"
  } else {
    trackerDat[trackerDat$id == id, ]$valenceChallenge_s1_r1_e1 = "0"
  } 
}
print("Updated valenceChallenge_s1_r1_e1!")