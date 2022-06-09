# rwe-eeg-dataset PsychoPy Preprocessing for Darwin Reading Task
# Author: Jessica M. Alexander
# Last Updated: 2022-06-08

### SECTION 1: SETTING UP
#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
#hpc
#input_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/sourcedata/checked/psychopy/'
#out_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/'
#local
input_path <- '/Users/jalexand/github/rwe-eeg-dataset/sourcedata/raw/psychopy/'
out_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/'

#identify participant folders within input dir
sub_folders <- list.files(input_path, pattern = "sub")

#create dataframes for storing output data and define output file names
readAloudDarwinSummaryDat <- data.frame(matrix(ncol=2, nrow=0))
colnames(readAloudDarwinSummaryDat) <- c("id", "challengeACC")
readAloudDarwin_out_subjectLevel <- paste("readAloud_darwin_subject-level_summary_", today, ".csv", sep="", collapse=NULL)

readAloudDarwinChallengeDat <- data.frame(matrix(ncol=1, nrow=8))
colnames(readAloudDarwinChallengeDat) <- c("passage")
readAloudDarwinChallengeDat[1,1] <- "In a broad"
readAloudDarwinChallengeDat[2,1] <- "On several"
readAloudDarwinChallengeDat[3,1] <- "The climat"
readAloudDarwinChallengeDat[4,1] <- "The countr"
readAloudDarwinChallengeDat[5,1] <- "The day ro"
readAloudDarwinChallengeDat[6,1] <- "The next d"
readAloudDarwinChallengeDat[7,1] <- "The only l"
readAloudDarwinChallengeDat[8,1] <- "The tortoi"

readAloudDarwin_out_passageLevel <- paste("readAloud_darwin_passage-level_summary_", today, ".csv", sep="", collapse=NULL)


### SECTION 2: PARTICIPANT LOOP
#loop over participants (subfolders)
for(i in 1:length(sub_folders)){
  
  #for this participant, find the darwin csv file
  psychopy_file <- list.files(paste(input_path,sub_folders[i], sep = "", collapse = NULL), pattern = ".*rwe-eeg.*.csv")
  
  #logical to make sure there is a dccs file for this participant before loading, else skip to next participant
  if (!identical(psychopy_file, character(0))) {
    print(paste("Woohoo! Processing", sub_folders[i], "!"))
  
    #read in the data for this participant, establish id, trim passage as passage identifier, and remove extraneous variables
    psychopyDat <- read.csv(file = paste(input_path,sub_folders[i],'/',psychopy_file, sep = "", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
    id <- psychopyDat$id[1]
    psychopyDat$passageId <- substr(psychopyDat$passageText, 1, 10)
    psychopyDatTrim <- psychopyDat[c("id",
                                     "passageId",
                                     "challengeResponse1.corr")]
    
    #order columns alphabetically by passageId
    psychopyDatTrim <- psychopyDatTrim[order(psychopyDatTrim$passageId),]
    
    #calculate accuracy for individual participant
    challengeACC <- mean(psychopyDatTrim$challengeResponse1.corr)
    
    #store output data in summary matrices
    readAloudDarwinSummaryDat[nrow(readAloudDarwinSummaryDat) + 1,] <-c(id,challengeACC)
    
    newcol <- ncol(readAloudDarwinChallengeDat) + 1
    readAloudDarwinChallengeDat[newcol] <- psychopyDatTrim$challengeResponse1.corr
    names(readAloudDarwinChallengeDat)[newcol] <- id
  }    
    
#if participant did not have a PsychoPy file, skip to next participant
  else {
  print(paste("Booo! Missing file for", sub_folders[i], "..."))
  }
}

### SECTION 3: READALOUD GROUP AVERAGE
#calculate group average for readAloudChallengeDat and append to dataframe
currentNoCols <- ncol(readAloudDarwinChallengeDat)
newCol <- currentNoCols + 1
readAloudDarwinChallengeDat[newCol] <- rowMeans(readAloudDarwinChallengeDat[,2:currentNoCols])
names(readAloudDarwinChallengeDat)[newCol] <- c("groupAvg")

### SECTION 4: OUTPUT DATA
#write the extracted summary scores to CSV
write.csv(readAloudDarwinChallengeDat,paste(out_path,readAloudDarwin_out_passageLevel, sep = "", collapse = NULL), row.names=FALSE)
write.csv(readAloudDarwinSummaryDat,paste(out_path,readAloudDarwin_out_subjectLevel, sep = "", collapse = NULL), row.names=FALSE)

### SECTION 5: UPDATE CENTRAL TRACKER FOR STUDY
# tbd (central tracker not yet implemented)