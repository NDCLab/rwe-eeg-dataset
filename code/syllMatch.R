# rwe-eeg-dataset Syllable Matching
# Authors: Jessica M. Alexander, George. A. Buzzell
# Last Updated: 2022-08-16
# This script identifies mispronunciations in each passage read by each participant and attempts to match to a correctly
# produced syllable, provided that there is a distance of at least seven correct syllables produced since the last error.
# The spacing of seven syllables was chosen due to the results of Laubrock & Kliegl 2015 (10.3389/fpsyg.2015.01432), which
# found an average eye-voice span during reading aloud of 16.2 letters, plus one standard deviation of 5.2 letters = 21.4
# letters. Across all unique syllables in the stimuli reading passages, average syllable length was 3.521401 letters.
# Therefore, 21.4 / 3.5 ≈ 6.11, which we rounded to 7 in order to be conservative. Error syllables are matched to potential
# non-error syllables first on a three-digit code that indicates:
# 1) whether syllable is word initial (1) or word internal (0)
# 2) whether syllable is followed by any kind of punctuation (1=yes, 0=no)
# 3) whether the following syllable is word initial (1) or word internal (0) (this was motivated by Cutler & Butterfield
#    1990 (10.1016/0167-6393(90)90024-4), showing that speakers use pauses to cue listeners to word boundaries)
# The set of possible matches is limited (again) to syllables at least seven syllables away from the error syllable. 
# The set is reviewed for a perfect match to the word that contains the error syllable; if an instance
# of the same word appears in the set of possible matches, it is selected.  Otherwise, the same search is applied to the
# word lemma.  If neither the word nor its lemma exists in the set of possible matches, the set is ordered, in descending
# order, by the distance between averaged frequency from the error syllable pair and the potential match pair.  Averaged
# frequency is computed by averaging across the word frequency of the word that contains the syllable in question and
# the word that contains the next syllable. The SUBTLEXUS corpus is used for word frequency given its common usage in the
# field and practical application (free txt download). The first match in the ordered set that is not already used as a
# match for an error syllable is selected.


### SECTION 1: SETTING UP
library(readxl) #read_xlsx function
library(textstem) #lemmatize_words function

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
#local
blank_scaffolds <- '/Users/jalexand/github/rwe-eeg-dataset/code/scaffolds.xlsx'
input_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/error-coding/'
out_path <- '/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/'

#load subtlexus frequency data (downloaded from https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus on 06/13/2022)
subtlexus <- read.table('/Users/jalexand/github/rwe-eeg-dataset/code/SUBTLEXus74286wordstextversion.txt', header=TRUE)
subtlexus$Word <- tolower(subtlexus$Word)

#identify participant folders within input dir
sub_folders <- list.files(input_path, pattern = "sub")

#create dataframes for storing output data and define output file names
syllDat <- data.frame(matrix(ncol=5, nrow=0))
colnames(syllDat) <- c("id",
                       "passage",
                       "errorType",
                       "errorSyll",
                       "matchSyll")
syll_out <- paste("syllable-match_", today, ".csv", sep="", collapse=NULL)

syllDatTimestamps <- data.frame(matrix(ncol=4, nrow=0))
colnames(syllDatTimestamps) <- c("id",
                               "passage",
                               "marker",
                               "syllable")
sylltime_out <- paste("syllable-match-timestamps_", today, ".csv", sep="", collapse=NULL)

syllDatTotals <- data.frame(matrix(ncol=3, nrow=0))
colnames(syllDatTotals) <- c("id",
                             "passage",
                             "totalMispron")
sylltotal_out <- paste("syllable-match-totals_", today, ".csv", sep="", collapse=NULL)

### SECTION 2: START PARTICIPANT LOOP
#loop over participants (subfolders)
for(i in 1:length(sub_folders)){
  
  id <- strsplit(sub_folders[i], "-")[[1]][2]
  
  #identify the list of error-coded files for the participant along with participant id
  darwinReconciled <- list.files(paste(input_path,sub_folders[i], "/darwin", sep = "", collapse = NULL), pattern="._reconciled")
  allDarwin <- list.files(paste(input_path,sub_folders[i], "/darwin/", darwinReconciled, sep = "", collapse = NULL))

  valenceReconciled <- list.files(paste(input_path,sub_folders[i], "/valence", sep = "", collapse = NULL), pattern="._reconciled")
  allValence <- list.files(paste(input_path,sub_folders[i], "/valence/", valenceReconciled, sep = "", collapse = NULL))
  
  ### SECTION 3: DARWIN PASSAGE LOOP
  #establish passage name
  for(j in 1:length(allDarwin)){
    #empty vector to store matches in order to avoid re-use within the same passage
    matchedSyllables <- c()
    
    errorCoded_file <- paste(input_path, sub_folders[i], "/darwin/", darwinReconciled, "/", allDarwin[j], sep = "", collapse = NULL)
    errorCoded_filename <- allDarwin[j]
    passage <- strsplit(strsplit(errorCoded_filename, "\\.")[[1]][1], "_")[[1]][1]
    
    #load disfluency data onto scaffold
    scaffold <- read_xlsx(blank_scaffolds, sheet=passage)
    
    errorData <- read_xlsx(errorCoded_file, sheet=NULL, range=anchored("B2", dim=c(9, dim(scaffold)[1]), col_names=FALSE))
    errorDataT <- t(errorData) #transpose matrix to align with scaffold
    colnames(errorDataT) <- c("mispron", "wordstress", "duplicate", "insertion", "hesitation", "elongation", "omission", "flipped")
    errorDataT[is.na(errorDataT)] <- 0 #replace accidental deletions with default 0
    
    passageErrors <- cbind(scaffold, errorDataT)
    
    #add column to indicate all disfluent syllables
    passageErrors$disfluent <- rowSums(passageErrors[,8:15])>0
    
    #add column to indicate whether syllable is at least 7 syllables from any disfluent syllable
    for(s in 1:nrow(passageErrors)){
      lookback <- sum(passageErrors$disfluent[max((s-7),0):s])
      lookforw <- sum(passageErrors$disfluent[s:min((s+7),length(passageErrors$disfluent))])
      passageErrors$errorSpace[s] <- (lookback==0) & (lookforw==0)
    }
    
    #add column to indicate the onset status of the next syllable
    passageErrors$palOnset <- as.numeric(c(passageErrors$wordOnset[2:length(passageErrors$wordOnset)], 2)>0)
    
    #add column to indicate whether syllable is followed by a punctuation boundary
    passageErrors$punctuation <- passageErrors$palOnset &
                                !grepl("[a-z]|[A-Z]|[0-9]", substr(passageErrors$word_identity, nchar(passageErrors$word_identity), nchar(passageErrors$word_identity)))
    passageErrors$punctuation <- as.numeric(passageErrors$punctuation)
    
    #add column that strips any punctuation from the word_identity and renders it lowercase
    wordTrimmed <- passageErrors$word_identity
    wordsToTrim <- which(!grepl("[a-z]|[A-Z]|[0-9]", substr(passageErrors$word_identity, nchar(passageErrors$word_identity), nchar(passageErrors$word_identity)))==TRUE)
    for(word in 1:length(wordTrimmed)){
      if(word %in% wordsToTrim){
        wordTrimmed[word] <- tolower(substr(wordTrimmed[word], 1, (nchar(wordTrimmed[word])-1)))
      } else {wordTrimmed[word] <- tolower(wordTrimmed[word])}
    }
    passageErrors$wordTrimmed <- wordTrimmed
    
    #add column that extracts word lemma from wordTrimmed
    passageErrors$wordLemma <- lemmatize_words(passageErrors$wordTrimmed)
      
    #add column with log frequency of the word associated with each syllable
    get_log_freq <- function(x){
      index <- match(x, passageErrors$word_id)
      word <- passageErrors$wordTrimmed[index]
      lemma <- passageErrors$wordLemma[index]
      if(!is.na(match(word, subtlexus$Word))){logfreq <- subtlexus$Lg10WF[index]} #if word has exact match in SUBTLEXus, use its log frequency
      else if(!is.na(match(lemma, subtlexus$Word))){logfreq <- subtlexus$Lg10WF[index]} #otherwise, try to match on word's lemma in SUBTLEXus
      else{logfreq <- 0} #use 0 if SUBTLEXus did not have frequency data for word or its lemma, which will match with most uncommon words
      return(logfreq)
    }
    passageErrors$logFreqError <- unlist(lapply(passageErrors$word_id, get_log_freq))

    #add column with averaged log frequency over the syllable in question and the syllable that follows it
    passageErrors$pairLogFreqAvg <- c(passageErrors$logFreqError[2:length(passageErrors$logFreqError)], 0)
    passageErrors$pairLogFreqAvg <- rowMeans(passageErrors[,c("logFreqError", "pairLogFreqAvg")])
    
    #add column with string representing the syllable's onset value + existence of a punctuation boundary + next syllable's onset value
    passageErrors$pairCode <- paste(passageErrors$wordOnset, passageErrors$punctuation,passageErrors$palOnset)
    
    #compose list of selected errors (at least seven correct syllables between error syllables)
    allErrors <- which(passageErrors$disfluent==TRUE)
    spacedErrors <- diff(allErrors)>7
    allMinusOne <- allErrors[2:length(allErrors)]
    selectErrors <- c(allErrors[1], allMinusOne[spacedErrors])
    
    ### SECTION 4: START MISPRONUNCIATION LOOP
    errorType <- "mispron"
    errorMarker <- 110
    correctMarker <- 111

    #create vector with identities of mispronounced syllables
    lastSyll <- tail(passageErrors$syllable_id, n=1)
    mispron <- which(passageErrors$mispron==1 & passageErrors$syllable_id!=lastSyll) #exclude last syllable (which cannot be paired with following syllable)
    mispronTrim <- mispron[mispron %in% selectErrors] #include only mispron errors from list selected to ensure correct syllables appear between errors
    
    #add all mispronounced syllables (if any) to matchedSyllables to avoid re-use during matching process
    if(!is.null(mispronTrim)){for(err in mispronTrim){matchedSyllables <- c(matchedSyllables, passageErrors$syllable_id[err])}}
    
    #loop over each mispronounced syllable to ascertain features of the error and find its best match
    if(length(mispronTrim)>0){
      for(m in 1:length(mispronTrim)){
        #clear matchSyll variable
        matchSyll <- NULL
        
        #characteristics of the error syllable: its id, the average of the log frequency of its word and the word of the following syllable in the stimulus passage, the pair code indicating the word onset/punctuation structure around the syllable, and the identity of the associated word (full word and lemma)
        errorSyll <- paste(passage, "_syll", mispronTrim[m], sep="", collapse=NULL)
        errorSyllPairFreq <- passageErrors$pairLogFreqAvg[mispronTrim[m]]
        errorSyllPairCode <- passageErrors$pairCode[mispronTrim[m]]
        errorWordIdent <- passageErrors$wordTrimmed[mispronTrim[m]]
        errorWordLemma <- passageErrors$wordLemma[mispronTrim[m]]
        
        #create array of syllables from which correct syllables can be drawn
        matchDat <- passageErrors
        matchDat <- matchDat[matchDat$pairCode==errorSyllPairCode,] #has the same errorSyllPairCode as the error syllable to be matched
        matchDat <- matchDat[matchDat$errorSpace==TRUE,] #at least seven syllables away from any error syllable
        
        #created a sorted vector of indices, based on log frequency average for possible matches (syllable plus following syllable)
        matchDat$distToErrorFreq <- abs(matchDat$pairLogFreqAvg - errorSyllPairFreq)
        freqMatches <- sort(matchDat$distToErrorFreq, index.return=TRUE)
        freqMatches <- freqMatches$ix
        
        #select best match: first try to match on exact word or word lemma
        for(q in 1:length(freqMatches)){
          possMatch <- matchDat$wordTrimmed[freqMatches[q]] #word to which possible match syllable belongs
          possLemma <- matchDat$wordLemma[freqMatches[q]] #lemma to which possible match syllable belongs
          if((possMatch==errorWordIdent) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #best match is syllable from same word in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          }else if(((possLemma==errorWordLemma) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables)))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #second best match is syllable from same lemma in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          }
        }
        
        #if no luck with exact word or word lemma, select best match from remaining options, which is first option from sorted frequency list that hasn't been put into our list of correct syllables
        if(is.null(matchSyll)){
          for(p in 1:length(freqMatches)){
            possMatchId <- matchDat$syllable_id[freqMatches[p]]
            if(is.element(possMatchId, matchedSyllables)){
              next
            } else {
              matchSyll <- matchDat$syllable_id[freqMatches[p]]
              matchedSyllables <- c(matchedSyllables, matchSyll)
              break
            }
          }
        }
        
        #store output data in syllDat summary matrix (one row per error syllable)
        if(!is.null(matchSyll)){
          syllDat[nrow(syllDat) + 1,] <-c(id,
                                          passage,
                                          errorType,
                                          errorSyll,
                                          matchSyll)
          syllDatTimestamps[nrow(syllDatTimestamps) + 1,] <-c(id,
                                                              passage,
                                                              errorMarker,
                                                              errorSyll)
          syllDatTimestamps[nrow(syllDatTimestamps) + 1,] <-c(id,
                                                              passage,
                                                              correctMarker,
                                                              matchSyll)
        }
      }
      
    syllDatTotals[nrow(syllDatTotals) + 1,] <-c(id,
                                                passage,
                                                length(mispron))
    }
  }
  
  
  ### SECTION 5: VALENCE PASSAGE LOOP
  #establish passage name
  for(j in 1:length(allValence)){
    #empty vector to store matches in order to avoid re-use within the same passage
    matchedSyllables <- c()
    
    errorCoded_file <- paste(input_path, sub_folders[i], "/valence/", valenceReconciled, "/", allValence[j], sep = "", collapse = NULL)
    errorCoded_filename <- allValence[j]
    passage <- strsplit(strsplit(errorCoded_filename, "\\.")[[1]][1], "_")[[1]][1]
    
    #load disfluency data onto scaffold
    scaffold <- read_xlsx(blank_scaffolds, sheet=passage)
    
    errorData <- read_xlsx(errorCoded_file, sheet=NULL, range=anchored("B2", dim=c(9, dim(scaffold)[1]), col_names=FALSE))
    errorDataT <- t(errorData) #transpose matrix to align with scaffold
    colnames(errorDataT) <- c("mispron", "wordstress", "duplicate", "insertion", "hesitation", "elongation", "omission", "flipped")
    errorDataT[is.na(errorDataT)] <- 0 #replace accidental deletions with default 0
    
    passageErrors <- cbind(scaffold, errorDataT)
    
    #add column to indicate all disfluent syllables
    passageErrors$disfluent <- rowSums(passageErrors[,8:15])>0
    
    #add column to indicate whether syllable is at least 7 syllables from any disfluent syllable
    for(s in 1:nrow(passageErrors)){
      lookback <- sum(passageErrors$disfluent[max((s-7),0):s])
      lookforw <- sum(passageErrors$disfluent[s:min((s+7),length(passageErrors$disfluent))])
      passageErrors$errorSpace[s] <- (lookback==0) & (lookforw==0)
    }
    
    #add column to indicate the onset status of the next syllable
    passageErrors$palOnset <- as.numeric(c(passageErrors$wordOnset[2:length(passageErrors$wordOnset)], 2)>0)
    
    #add column to indicate whether syllable is followed by a punctuation boundary
    passageErrors$punctuation <- passageErrors$palOnset &
      !grepl("[a-z]|[A-Z]|[0-9]", substr(passageErrors$word_identity, nchar(passageErrors$word_identity), nchar(passageErrors$word_identity)))
    passageErrors$punctuation <- as.numeric(passageErrors$punctuation)
    
    #add column that strips any punctuation from the word_identity and renders it lowercase
    wordTrimmed <- passageErrors$word_identity
    wordsToTrim <- which(!grepl("[a-z]|[A-Z]|[0-9]", substr(passageErrors$word_identity, nchar(passageErrors$word_identity), nchar(passageErrors$word_identity)))==TRUE)
    for(word in 1:length(wordTrimmed)){
      if(word %in% wordsToTrim){
        wordTrimmed[word] <- tolower(substr(wordTrimmed[word], 1, (nchar(wordTrimmed[word])-1)))
      } else {wordTrimmed[word] <- tolower(wordTrimmed[word])}
    }
    passageErrors$wordTrimmed <- wordTrimmed
    
    #add column that extracts word lemma from wordTrimmed
    passageErrors$wordLemma <- lemmatize_words(passageErrors$wordTrimmed)
    
    #add column with log frequency of the word associated with each syllable
    get_log_freq <- function(x){
      index <- match(x, passageErrors$word_id)
      word <- passageErrors$wordTrimmed[index]
      lemma <- passageErrors$wordLemma[index]
      if(!is.na(match(word, subtlexus$Word))){logfreq <- subtlexus$Lg10WF[index]} #if word has exact match in SUBTLEXus, use its log frequency
      else if(!is.na(match(lemma, subtlexus$Word))){logfreq <- subtlexus$Lg10WF[index]} #otherwise, try to match on word's lemma in SUBTLEXus
      else{logfreq <- 0} #use 0 if SUBTLEXus did not have frequency data for word or its lemma, which will match with most uncommon words
      return(logfreq)
    }
    passageErrors$logFreqError <- unlist(lapply(passageErrors$word_id, get_log_freq))
    
    #add column with averaged log frequency over the syllable in question and the syllable that follows it
    passageErrors$pairLogFreqAvg <- c(passageErrors$logFreqError[2:length(passageErrors$logFreqError)], 0)
    passageErrors$pairLogFreqAvg <- rowMeans(passageErrors[,c("logFreqError", "pairLogFreqAvg")])
    
    #add column with string representing the syllable's onset value + existence of a punctuation boundary + next syllable's onset value
    passageErrors$pairCode <- paste(passageErrors$wordOnset, passageErrors$punctuation,passageErrors$palOnset)
    
    #compose list of selected errors (at least seven correct syllables between error syllables)
    allErrors <- which(passageErrors$disfluent==TRUE)
    spacedErrors <- diff(allErrors)>7
    allMinusOne <- allErrors[2:length(allErrors)]
    selectErrors <- c(allErrors[1], allMinusOne[spacedErrors])
    
    
    ### SECTION 6: START MISPRONUNCIATION LOOP
    errorType <- "mispron"
    errorMarker <- 110
    correctMarker <- 111
    
    #create vector with identities of mispronounced syllables
    lastSyll <- tail(passageErrors$syllable_id, n=1)
    mispron <- which(passageErrors$mispron==1 & passageErrors$syllable_id!=lastSyll) #exclude last syllable (which cannot be paired with following syllable)
    mispronTrim <- mispron[mispron %in% selectErrors] #include only mispron errors from list selected to ensure correct syllables appear between errors
    
    #add all mispronounced syllables (if any) to matchedSyllables to avoid re-use during matching process
    if(!is.null(mispronTrim)){for(err in mispronTrim){matchedSyllables <- c(matchedSyllables, passageErrors$syllable_id[err])}}
    
    #loop over each mispronounced syllable to ascertain features of the error and find its best match
    if(length(mispronTrim)>0){
      for(m in 1:length(mispronTrim)){
        #clear matchSyll variable
        matchSyll <- NULL
        
        #characteristics of the error syllable: its id, the average of the log frequency of its word and the word of the following syllable in the stimulus passage, the pair code indicating the word onset/punctuation structure around the syllable, and the identity of the associated word (full word and lemma)
        errorSyll <- paste(passage, "_syll", mispronTrim[m], sep="", collapse=NULL)
        errorSyllPairFreq <- passageErrors$pairLogFreqAvg[mispronTrim[m]]
        errorSyllPairCode <- passageErrors$pairCode[mispronTrim[m]]
        errorWordIdent <- passageErrors$wordTrimmed[mispronTrim[m]]
        errorWordLemma <- passageErrors$wordLemma[mispronTrim[m]]
        
        #create array of syllables from which correct syllables can be drawn
        matchDat <- passageErrors
        matchDat <- matchDat[matchDat$pairCode==errorSyllPairCode,] #has the same errorSyllPairCode as the error syllable to be matched
        matchDat <- matchDat[matchDat$errorSpace==TRUE,] #at least seven syllables away from any error syllable
        
        #created a sorted vector of indices, based on log frequency average for possible matches (syllable plus following syllable)
        matchDat$distToErrorFreq <- abs(matchDat$pairLogFreqAvg - errorSyllPairFreq)
        freqMatches <- sort(matchDat$distToErrorFreq, index.return=TRUE)
        freqMatches <- freqMatches$ix
        
        #select best match: first try to match on exact word or word lemma
        for(q in 1:length(freqMatches)){
          possMatch <- matchDat$wordTrimmed[freqMatches[q]] #word to which possible match syllable belongs
          possLemma <- matchDat$wordLemma[freqMatches[q]] #lemma to which possible match syllable belongs
          if((possMatch==errorWordIdent) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #best match is syllable from same word in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          }else if(((possLemma==errorWordLemma) && !(is.element(matchDat$syllable_id[freqMatches[q]], matchedSyllables)))){
            matchSyll <- matchDat$syllable_id[freqMatches[q]] #second best match is syllable from same lemma in different location that isn't already in the list of correct syllables
            matchedSyllables <- c(matchedSyllables, matchSyll)
            break
          }
        }
        
        #if no luck with exact word or word lemma, select best match from remaining options, which is first option from sorted frequency list that hasn't been put into our list of correct syllables
        if(is.null(matchSyll)){
          for(p in 1:length(freqMatches)){
            possMatchId <- matchDat$syllable_id[freqMatches[p]]
            if(is.element(possMatchId, matchedSyllables)){
              next
            } else {
              matchSyll <- matchDat$syllable_id[freqMatches[p]]
              matchedSyllables <- c(matchedSyllables, matchSyll)
              break
            }
          }
        }
        
        #store output data in syllDat summary matrix (one row per error syllable)
        if(!is.null(matchSyll)){
          syllDat[nrow(syllDat) + 1,] <-c(id,
                                          passage,
                                          errorType,
                                          errorSyll,
                                          matchSyll)
          syllDatTimestamps[nrow(syllDatTimestamps) + 1,] <-c(id,
                                                              passage,
                                                              errorMarker,
                                                              errorSyll)
          syllDatTimestamps[nrow(syllDatTimestamps) + 1,] <-c(id,
                                                              passage,
                                                              correctMarker,
                                                              matchSyll)
        }
      }
    
    syllDatTotals[nrow(syllDatTotals) + 1,] <-c(id,
                                                passage,
                                                length(mispron))
    }
  }
}


### SECTION 7: OUTPUT DATA
#re-order by participant>passage>syllable id
extract_number <- function(x){
  syllID <- strsplit(x, "_")[[1]][2]
  number <- as.numeric(gsub("syll", "", syllID))
  return(number)
}
syllDatTimestamps$number <- unlist(lapply(syllDatTimestamps$syllable, extract_number))
syllDatTimestamps <- syllDatTimestamps[order(syllDatTimestamps[,1], syllDatTimestamps[,2], syllDatTimestamps[,5]), ]
syllDatTimestamps <- syllDatTimestamps[,-5]

#output to csv
write.csv(syllDat,paste(out_path, syll_out, sep = "", collapse = NULL), row.names=FALSE)
write.csv(syllDatTimestamps,paste(out_path, sylltime_out, sep = "", collapse = NULL), row.names=FALSE)
write.csv(syllDatTotals,paste(out_path, sylltotal_out, sep = "", collapse = NULL), row.names=FALSE)


### SECTION 8: UPDATE CENTRAL TRACKER FOR STUDY
#load central tracker
track_path <- '/home/data/NDClab/datasets/rwe-eeg-dataset/data-monitoring/central-tracker_rwe-eeg.csv'
trackerDat <- read.csv(track_path, header=TRUE, check.names=FALSE)

subs_matched <- unique(syllDat$id)

for (x in 1:length(subs_matched)) {
  id <- subs_matched[x]
  trackerDat[trackerDat$id == id, ]$syllMatch_s1_r1_e1 = "1"
}

print("Updated syllMatch_s1_r1_e1!")