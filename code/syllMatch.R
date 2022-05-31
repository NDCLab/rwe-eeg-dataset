# readAloud-valence-dataset Syllable Matching
# Authors: Jessica M. Alexander, George. A. Buzzell
# Last Updated: 2022-05-31

### SECTION 1: SETTING UP
library(readxl)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
#local
blank_scaffolds <- '/Users/jalexand/github/rwe-eeg-dataset/code/scaffolds.xlsx'
lexical_data <- '/Users/jalexand/github/rwe-eeg-dataset/code/lexical-characteristics.xlsx'
input_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/error-coding/'
out_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/'

#identify participant folders within input dir
sub_folders <- list.files(input_path, pattern = "sub")

#create dataframe for storing output data and define output file name
syllDat <- data.frame(matrix(ncol=5, nrow=0))
colnames(syllDat) <- c("id",
                       "passage",
                       "errorType",
                       "errorSyll",
                       "matchSyll")
syll_out <- paste("syllable-match_", today, ".csv", sep="", collapse=NULL)

### SECTION 2: START PARTICIPANT LOOP
#loop over participants (subfolders)
for(i in 1:length(sub_folders)){
  
  #identify the list of error-coded files for the participant along with participant id
  sub_files <- list.files(paste(input_path, sub_folders[i], sep = "", collapse = NULL))
  id <- strsplit(sub_folders[i], "-")[[1]][2]
  
  
  ### SECTION 3: START PASSAGE LOOP ON RECONCILED CODING
  #empty vector to store matches in order to avoid re-use within the same passage
  matchedSyllables <- c()
  
  #establish passage name
  for(j in 1:length(sub_files)){
    errorCoded_file <- paste(input_path, sub_folders[i], "/", sub_files[j], sep = "", collapse = NULL)
    errorCoded_filename <- sub_files[j]
    passage <- strsplit(errorCoded_filename, "\\.")[[1]][1]
    
    #load disfluency data onto scaffold
    scaffold <- read_xlsx(blank_scaffolds, sheet=passage)
    lexDat <- read_xlsx(lexical_data, sheet=passage, skip=1)
    
    errorData <- read_xlsx(errorCoded_file, sheet=NULL, range=anchored("B2", dim=c(9, dim(scaffold)[1]), col_names=FALSE))
    errorDataT <- t(errorData) #transpose matrix to align with scaffold
    colnames(errorDataT) <- c("mispron", "wordstress", "duplicate", "insertion", "hesitation", "elongation", "omission","flipped")
    
    passageErrors <- cbind(scaffold, errorDataT)
    
    #add column to indicate all disfluent syllables
    passageErrors$disfluent <- rowSums(passageErrors[,6:13])>0
    
    
    ### SECTION 4: START MISPRONUNCIATION LOOP
    errorType <- "mispron"
    
    #create vector with identities of mispronounced syllables
    mispron <- which(passageErrors$mispron==1)
    
    #loop over each mispronounced syllable
    if(length(mispron)>0){
      for(m in 1:length(mispron)){
        #clear matchSyll variable
        matchSyll <- NULL
        
        #characteristics of the error syllable
        errorSyll <- paste(passage, "_syll", mispron[m], sep="", collapse=NULL)
        wordOnset <- passageErrors$wordOnset[mispron[m]]
        
        #characteristics of the word in which the error syllable appears
        errorWord <- passageErrors$word_id[mispron[m]]
        errorWordIdent <- lexDat$stimWord[match(errorWord, lexDat$word_id)]
        logfreqError <- as.numeric(lexDat$logFreqHAL[match(errorWord, lexDat$word_id)])
        if(is.na(logfreqError)){logfreqError <- 0} #use 0 if ELP did not have frequency data for word, which will match with most uncommon words
        
        #create array of syllables that match on onset value, are greater than 5 syllables from errorSyll, and free of disfluencies
        startBlock <- max(mispron[m]-5, 1)
        endBlock <- min(mispron[m]+5, nrow(passageErrors))
        matchDat <- passageErrors[-c(startBlock:endBlock),]
        matchDat <- matchDat[matchDat$wordOnset==wordOnset,]
        matchDat <- matchDat[matchDat$disfluent==FALSE,]
        
        #find words in matchDat with log frequency closest to that of the error word
        freqVector <- c()
        for(n in 1:length(matchDat$word_id)){
          freqVector[n] <- as.numeric(lexDat$logFreqHAL[match(matchDat$word_id[n], lexDat$word_id)])
        }
        
        distToErrorFreq <- function(x){
          distance <- abs(x - logfreqError)
          return(distance)
        }
        distVector <- lapply(freqVector, distToErrorFreq)
        matchDat$distFreq <- unlist(distVector)
        
        matchDat <- matchDat[!is.na(matchDat$distFreq),] #remove rows where no word frequency data was available
        
        freqMatches <- which(matchDat$distFreq==min(matchDat$distFreq)) #vector of row numbers in matchDat of syllables that are possible matches
        
        #select best match
        for(q in 1:length(freqMatches)){
          possMatch <- lexDat$stimWord[match(matchDat$word_id[freqMatches[q]], lexDat$word_id)] #word to which possible match syllable belongs
          if((possMatch==errorWordIdent) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #best match is syllable from same word in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          } 
        }
        if(is.null(matchSyll)){
          for(p in 1:length(freqMatches)){
            possMatchId <- matchDat$syllable_id[freqMatches[p]]
            if(is.element(possMatchId, matchedSyllables)){
              next
            } else {
              matchSyll <- matchDat$syllable_id[freqMatches[p]] #otherwise, take first option from the list of possible matches that hasn't been put into our list of correct syllables
              matchedSyllables <- c(matchedSyllables, matchSyll)
              break
            }
          }
        }
        
        #store output data in summary matrix if adequate match found
        if(!is.null(matchSyll)){
          syllDat[nrow(syllDat) + 1,] <-c(id,
                                          passage,
                                          errorType,
                                          errorSyll,
                                          matchSyll)
        }
      }
    }

    
    ### SECTION 5: START HESITATION LOOP
    errorType <- "hes"
    
    #create vector with identities of post-hesitation syllables
    hes <- which(passageErrors$hesitation==1)
    
    #loop over each post-hesitation syllable
    if(length(hes)>0){
      for(h in 1:length(hes)){
        #clear matchSyll variable
        matchSyll <- NULL
        
        #characteristics of the error syllable
        errorSyll <- paste(passage, "_syll", hes[h], sep="", collapse=NULL)
        wordOnset <- passageErrors$wordOnset[hes[h]]
        
        #characteristics of the word in which the error syllable appears
        errorWord <- passageErrors$word_id[hes[h]]
        errorWordIdent <- lexDat$stimWord[match(errorWord, lexDat$word_id)]
        logfreqError <- as.numeric(lexDat$logFreqHAL[match(errorWord, lexDat$word_id)])
        if(is.na(logfreqError)){logfreqError <- 0} #use 0 if ELP did not have frequency data for word, which will match with most uncommon words
        
        #create array of syllables that match on onset value, are greater than 5 syllables from errorSyll, and free of disfluencies
        startBlock <- max(hes[h]-5, 1)
        endBlock <- min(hes[h]+5, nrow(passageErrors))
        matchDat <- passageErrors[-c(startBlock:endBlock),]
        matchDat <- matchDat[matchDat$wordOnset==wordOnset,]
        matchDat <- matchDat[matchDat$disfluent==FALSE,]
        
        #find words in matchDat with log frequency closest to that of the error word
        freqVector <- c()
        for(n in 1:length(matchDat$word_id)){
          freqVector[n] <- as.numeric(lexDat$logFreqHAL[match(matchDat$word_id[n], lexDat$word_id)])
        }
        
        distToErrorFreq <- function(x){
          distance <- abs(x - logfreqError)
          return(distance)
        }
        distVector <- lapply(freqVector, distToErrorFreq)
        matchDat$distFreq <- unlist(distVector)
        
        matchDat <- matchDat[!is.na(matchDat$distFreq),] #remove rows where no word frequency data was available
        
        freqMatches <- which(matchDat$distFreq==min(matchDat$distFreq)) #vector of row numbers in matchDat of syllables that are possible matches
        
        #select best match
        for(q in 1:length(freqMatches)){
          possMatch <- lexDat$stimWord[match(matchDat$word_id[freqMatches[q]], lexDat$word_id)] #word to which possible match syllable belongs
          if((possMatch==errorWordIdent) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #best match is syllable from same word in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          } 
        }
        if(is.null(matchSyll)){
          for(p in 1:length(freqMatches)){
            possMatchId <- matchDat$syllable_id[freqMatches[p]]
            if(is.element(possMatchId, matchedSyllables)){
              next
            } else {
              matchSyll <- matchDat$syllable_id[freqMatches[p]] #otherwise, take first option from the list of possible matches that hasn't been put into our list of correct syllables
              matchedSyllables <- c(matchedSyllables, matchSyll)
              break
            }
          }
        }
        
        #store output data in summary matrix if adequate match found
        if(!is.null(matchSyll)){
          syllDat[nrow(syllDat) + 1,] <-c(id,
                                          passage,
                                          errorType,
                                          errorSyll,
                                          matchSyll)
        }
      }
    }      
    
    
    ### SECTION 6: START ELONGATION LOOP
    errorType <- "elong"
    
    #create vector with identities of elongated syllables
    elong <- which(passageErrors$elongation==1)
    
    #loop over each elongated syllable
    if(length(elong)>0){
      for(e in 1:length(elong)){
        #clear matchSyll variable
        matchSyll <- NULL
        
        #characteristics of the error syllable
        errorSyll <- paste(passage, "_syll", elong[e], sep="", collapse=NULL)
        wordOnset <- passageErrors$wordOnset[elong[e]]
        
        #characteristics of the word in which the error syllable appears
        errorWord <- passageErrors$word_id[elong[e]]
        errorWordIdent <- lexDat$stimWord[match(errorWord, lexDat$word_id)]
        logfreqError <- as.numeric(lexDat$logFreqHAL[match(errorWord, lexDat$word_id)])
        if(is.na(logfreqError)){logfreqError <- 0} #use 0 if ELP did not have frequency data for word, which will match with most uncommon words
        
        #create array of syllables that match on onset value, are greater than 5 syllables from errorSyll, and free of disfluencies
        startBlock <- max(elong[e]-5, 1)
        endBlock <- min(elong[e]+5, nrow(passageErrors))
        matchDat <- passageErrors[-c(startBlock:endBlock),]
        matchDat <- matchDat[matchDat$wordOnset==wordOnset,]
        matchDat <- matchDat[matchDat$disfluent==FALSE,]
        
        #find words in matchDat with log frequency closest to that of the error word
        freqVector <- c()
        for(n in 1:length(matchDat$word_id)){
          freqVector[n] <- as.numeric(lexDat$logFreqHAL[match(matchDat$word_id[n], lexDat$word_id)])
        }
        
        distToErrorFreq <- function(x){
          distance <- abs(x - logfreqError)
          return(distance)
        }
        distVector <- lapply(freqVector, distToErrorFreq)
        matchDat$distFreq <- unlist(distVector)
        
        matchDat <- matchDat[!is.na(matchDat$distFreq),] #remove rows where no word frequency data was available
        
        freqMatches <- which(matchDat$distFreq==min(matchDat$distFreq)) #vector of row numbers in matchDat of syllables that are possible matches
        
        #select best match
        for(q in 1:length(freqMatches)){
          possMatch <- lexDat$stimWord[match(matchDat$word_id[freqMatches[q]], lexDat$word_id)] #word to which possible match syllable belongs
          if((possMatch==errorWordIdent) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #best match is syllable from same word in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          } 
        }
        if(is.null(matchSyll)){
          for(p in 1:length(freqMatches)){
            possMatchId <- matchDat$syllable_id[freqMatches[p]]
            if(is.element(possMatchId, matchedSyllables)){
              next
            } else {
              matchSyll <- matchDat$syllable_id[freqMatches[p]] #otherwise, take first option from the list of possible matches that hasn't been put into our list of correct syllables
              matchedSyllables <- c(matchedSyllables, matchSyll)
              break
            }
          }
        }
        
        #store output data in summary matrix if adequate match found
        if(!is.null(matchSyll)){
          syllDat[nrow(syllDat) + 1,] <-c(id,
                                          passage,
                                          errorType,
                                          errorSyll,
                                          matchSyll)
        }
      }
    }            
  }
}

### SECTION 7: OUTPUT DATA
write.csv(syllDat,paste(out_path, syll_out, sep = "", collapse = NULL), row.names=FALSE)