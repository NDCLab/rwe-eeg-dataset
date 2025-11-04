# rwe-eeg-dataset Inter-Rater Reliability for Error Coding
# Authors: Jessica M. Alexander, George A. Buzzell
# Last Updated: 2022-06-07

### SECTION 1: SETTING UP
library(readxl)
library(irr)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
#hpc
#scaffolds <- '/home/data/NDClab/datasets/rwe-eeg-dataset/code/scaffolds.xlsx'
#input_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/error-coding/'
#out_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/'
#local
scaffolds <- '/Users/jalexand/github/rwe-eeg-dataset/code/scaffolds.xlsx'
input_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/error-coding/'
out_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/'

#create vectors of passage names
darwinPassages <- c("10-the-climate",
                "10-the-next-day",
                "13-the-day-rose",
                "17-the-only",
                "17-the-tortoise",
                "2-on-several",
                "21-the-country",
                "3-in-a-broad")
valencePassages <- c("antarctica",
                    "bats",
                    "bees",
                    "broccoli",
                    "caramel",
                    "cars",
                    "congo",
                    "dams",
                    "dentist",
                    "depression",
                    "dogshow",
                    "dolphins",
                    "flying",
                    "grizzly",
                    "icefishing",
                    "mantis",
                    "realty",
                    "skunkowl",
                    "sun",
                    "vegas")

#identify participant folders within input dir
sub_folders <- list.files(input_path)

#create dataframe for storing output data and define output file name
kappaSummaryDat <- data.frame(matrix(ncol=5, nrow=0))
colnames(kappaSummaryDat) <- c("id", "passage", "kappaMispron", "kappaElong", "kappaHes")
kappa_out <- paste("interrater-reliability-by-passage_", today, ".csv", sep="", collapse=NULL)


### SECTION 2: START PARTICIPANT LOOP
#loop over participants (subfolders)
for(i in 1:length(sub_folders)){
    
  #identify participant id
  id <- strsplit(sub_folders[i], "-")[[1]][2]
  print(paste("Starting loop for sub-", id, sep="", collapse=NULL))

    
  ### SECTION 3: DARWIN CODER LOOP
  allDarwin <- list.files(paste(input_path,sub_folders[i], "/darwin", sep = "", collapse = NULL))
  
  #prime matrices for study team set of coders
  ad <- data.frame(matrix(ncol=8, nrow=0))
  ag <- data.frame(matrix(ncol=8, nrow=0))
  aln <- data.frame(matrix(ncol=8, nrow=0))
  br <- data.frame(matrix(ncol=8, nrow=0))
  
  #loop over both coders (subfolders)
  for(j in 1:length(allDarwin)){
    
    #identify coder
    coder <- strsplit(allDarwin[j], "_")[[1]][2]
    
    #identify list of passages
    allPassageFiles <- list.files(paste(input_path, sub_folders[i], "/darwin/", allDarwin[j], sep = "", collapse = NULL))
    
    ### SECTION 4: DARWIN PASSAGE LOOP
    #loop over coded passages (subfiles)
    for(k in 1:length(darwinPassages)){
      passageName <- darwinPassages[k]
      passageNameXLSX <- paste(passageName, ".xlsx", sep="", collapse=NULL)
      passageIdx <- match(passageNameXLSX, allPassageFiles)
      passageFile <- paste(input_path, sub_folders[i], "/darwin/", allDarwin[j], "/", allPassageFiles[passageIdx], sep = "", collapse = NULL)
      
      passageScaffold <- read_xlsx(scaffolds, sheet=passageName)
      
      #read in coded file for passage
      errorData <- read_xlsx(passageFile, sheet=NULL, range=anchored("B2", dim=c(9, length(passageScaffold$syllable_id)), col_names=FALSE))
      errorDataT <- t(errorData) #transpose matrix
      colnames(errorDataT) <- c("mispron", "wordstress", "duplicate", "insertion", "hesitation", "elongation", "omission", "flipped")
      errorDataT <- data.frame(errorDataT) #convert matrix to dataframe
      
      #add passage name as new column for future subsetting
      errorDataT$passage <- rep(passageName, nrow(errorDataT))
      errorDataT$sub <- rep(as.numeric(id), nrow(errorDataT)) #add subject id for visual inspection during debugging
      
      #bind coded data to matrix for specific coder
      if(coder=="ad"){
        ad <- rbind(ad, errorDataT)
        ad[, 1:8] <- as.numeric(as.matrix(ad[, 1:8]))
      } else if(coder=="ag"){
        ag <- rbind(ag, errorDataT)
        ag[, 1:8] <- as.numeric(as.matrix(ag[, 1:8]))
      } else if(coder=="aln"){
        aln <- rbind(aln, errorDataT)
        aln[, 1:8] <- as.numeric(as.matrix(aln[, 1:8]))
      } else if(coder=="br"){
        br <- rbind(br, errorDataT)
        br[, 1:8] <- as.numeric(as.matrix(br[, 1:8]))
      } else{
        print("Oops. Couldn't find that coder.")
      }
    }
  }    
  
  ### SECTION 5: RE-LOOP DARWIN PASSAGES TO CALCULATE COHEN'S D
  for (o in 1:length(darwinPassages)){
    passageName <- darwinPassages[o]
    
    #trim coder dataframes down to specific passage
    adTrim <- ad[ad$passage==passageName,]
    agTrim <- ag[ag$passage==passageName,]
    alnTrim <- aln[aln$passage==passageName,]
    brTrim <- br[br$passage==passageName,]
    
    #create arrays for each error type across coders for this particular passage
    mispron <- cbind(adTrim$mispron, agTrim$mispron, alnTrim$mispron, brTrim$mispron)
    elongation <- cbind(adTrim$elongation, agTrim$elongation, alnTrim$elongation, brTrim$elongation)
    hesitation <- cbind(adTrim$hesitation, agTrim$hesitation, alnTrim$hesitation, brTrim$hesitation)
    
    #calculate kappas
    kappaMispron <- kappa2(mispron[, c(1,2)])$value
    kappaElong <- kappa2(elongation[, c(1,2)])$value
    kappaHes <- kappa2(hesitation[, c(1,2)])$value
    
    kappaSummaryDat[nrow(kappaSummaryDat) + 1,] <-c(id, passageName, kappaMispron, kappaElong, kappaHes)
  }
  
  ### SECTION 6: VALENCE CODER LOOP
  allValence <- list.files(paste(input_path,sub_folders[i], "/valence", sep = "", collapse = NULL))
  
  #prime matrices for study team set of coders
  ad <- data.frame(matrix(ncol=8, nrow=0))
  ag <- data.frame(matrix(ncol=8, nrow=0))
  aln <- data.frame(matrix(ncol=8, nrow=0))
  br <- data.frame(matrix(ncol=8, nrow=0))
  
  #loop over both coders (subfolders)
  for(p in 1:length(allValence)){
    
    #identify coder
    coder <- strsplit(allValence[p], "_")[[1]][2]
    
    #identify list of passages
    allPassageFiles <- list.files(paste(input_path, sub_folders[i], "/valence/", allValence[p], sep = "", collapse = NULL))
    
    ### SECTION 7: VALENCE PASSAGE LOOP
    #loop over coded passages (subfiles)
    for(q in 1:length(valencePassages)){
      passageName <- valencePassages[q]
      passageNameXLSX <- paste(passageName, ".xlsx", sep="", collapse=NULL)
      passageIdx <- match(passageNameXLSX, allPassageFiles)
      passageFile <- paste(input_path, sub_folders[i], "/valence/", allValence[p], "/", allPassageFiles[passageIdx], sep = "", collapse = NULL)
      
      passageScaffold <- read_xlsx(scaffolds, sheet=passageName)
      
      #read in coded file for passage
      errorData <- read_xlsx(passageFile, sheet=NULL, range=anchored("B2", dim=c(9, length(passageScaffold$syllable_id)), col_names=FALSE))
      errorDataT <- t(errorData) #transpose matrix
      colnames(errorDataT) <- c("mispron", "wordstress", "duplicate", "insertion", "hesitation", "elongation", "omission", "flipped")
      errorDataT <- data.frame(errorDataT) #convert matrix to dataframe
      
      #add passage name as new column for future subsetting
      errorDataT$passage <- rep(passageName, nrow(errorDataT))
      errorDataT$sub <- rep(as.numeric(id), nrow(errorDataT)) #add subject id for visual inspection during debugging
      
      #bind coded data to matrix for specific coder
      if(coder=="ad"){
        ad <- rbind(ad, errorDataT)
        ad[, 1:8] <- as.numeric(as.matrix(ad[, 1:8]))
      } else if(coder=="ag"){
        ag <- rbind(ag, errorDataT)
        ag[, 1:8] <- as.numeric(as.matrix(ag[, 1:8]))
      } else if(coder=="aln"){
        aln <- rbind(aln, errorDataT)
        aln[, 1:8] <- as.numeric(as.matrix(aln[, 1:8]))
      } else if(coder=="br"){
        br <- rbind(br, errorDataT)
        br[, 1:8] <- as.numeric(as.matrix(br[, 1:8]))
      } else{
        print("Oops. Couldn't find that coder.")
      }
    }
  }    
  
  ### SECTION 8: RE-LOOP VALENCE PASSAGES TO CALCULATE COHEN'S D
  for (r in 1:length(valencePassages)){
    passageName <- valencePassages[r]
    
    #trim coder dataframes down to specific passage
    adTrim <- ad[ad$passage==passageName,]
    agTrim <- ag[ag$passage==passageName,]
    alnTrim <- aln[aln$passage==passageName,]
    brTrim <- br[br$passage==passageName,]
    
    #create arrays for each error type across coders for this particular passage
    mispron <- cbind(adTrim$mispron, agTrim$mispron, alnTrim$mispron, brTrim$mispron)
    elongation <- cbind(adTrim$elongation, agTrim$elongation, alnTrim$elongation, brTrim$elongation)
    hesitation <- cbind(adTrim$hesitation, agTrim$hesitation, alnTrim$hesitation, brTrim$hesitation)
    
    #calculate kappas
    kappaMispron <- kappa2(mispron[, c(1,2)])$value
    kappaElong <- kappa2(elongation[, c(1,2)])$value
    kappaHes <- kappa2(hesitation[, c(1,2)])$value
    
    kappaSummaryDat[nrow(kappaSummaryDat) + 1,] <-c(id, passageName, kappaMispron, kappaElong, kappaHes)
  }
}  

### SECTION 9: OUTPUT DATA
write.csv(kappaSummaryDat,paste(out_path, kappa_out, sep = "", collapse = NULL), row.names=FALSE)