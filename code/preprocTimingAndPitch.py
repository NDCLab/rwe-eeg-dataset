# rwe-eeg-dataset Timing and Pitch Preprocessing
# Author: Jessica M. Alexander
# Last Updated: 2022-08-17

### SECTION 1: SETTING UP
import numpy as np
import os
import pandas as pd
import parselmouth
import re
from datetime import date
#import gnupg

today = date.today()
today_formatted = today.strftime("%Y-%m-%d")

#function to create working df for each subject and prefill with coded timestamps
def create_newdf(npdf):
  newdf = np.full((20,26), fill_value=np.NaN)
  
  for row in range(npdf.shape[0]):
    newdf[row, 0:8] = npdf[row, 1:9]
  
  return newdf

#function to extract data from praat using parselmouth
def extract_parselmouth(sub_id, pitch, df, newdf):
  for row in range(newdf.shape[0]):
    readStart = newdf[row,0]
    prePREswitchStart = newdf[row,1]
    preswitchStart = newdf[row,2]
    switchStart = newdf[row,3]
    switchWord = newdf[row,4]
    postswitchStart = newdf[row,5]
    postswitchEnd = newdf[row,6]
    readEnd = newdf[row,7]
    
    if np.isnan(switchWord):
      continue #cannot calculate any passage data if missing timestamp for switchWord
    elif np.isnan(readStart) and np.isnan(readEnd):
      continue #cannot calculate any passage data if missing both readStart and readEnd timestamps
    elif np.isnan(readEnd) and not np.isnan(readStart):
      newdf[row, 8] = switchWord - readStart #first half reading time
      newdf[row, 10] = parselmouth.praat.call(pitch, "Get mean", readStart, switchWord, "Hertz") #first half mean pitch
      newdf[row, 11] = parselmouth.praat.call(pitch, "Get standard deviation", readStart, switchWord, "Hertz") #first half pitch sd
    elif np.isnan(readStart) and not np.isnan(readEnd):  
      newdf[row, 9] = readEnd - switchWord #second half reading time
      newdf[row, 12] = parselmouth.praat.call(pitch, "Get mean", switchWord, readEnd, "Hertz") #second half mean pitch
      newdf[row, 13] = parselmouth.praat.call(pitch, "Get standard deviation", switchWord, readEnd, "Hertz") #second half pitch sd
    elif np.isnan(prePREswitchStart): #coding incomplete
      newdf[row, 8] = switchWord - readStart #first half reading time
      newdf[row, 10] = parselmouth.praat.call(pitch, "Get mean", readStart, switchWord, "Hertz") #first half mean pitch
      newdf[row, 11] = parselmouth.praat.call(pitch, "Get standard deviation", readStart, switchWord, "Hertz") #first half pitch sd
      newdf[row, 9] = readEnd - switchWord #second half reading time
      newdf[row, 12] = parselmouth.praat.call(pitch, "Get mean", switchWord, readEnd, "Hertz") #second half mean pitch
      newdf[row, 13] = parselmouth.praat.call(pitch, "Get standard deviation", switchWord, readEnd, "Hertz") #second half pitch sd
    else:
      newdf[row, 8] = switchWord - readStart #first half reading time
      newdf[row, 10] = parselmouth.praat.call(pitch, "Get mean", readStart, switchWord, "Hertz") #first half mean pitch
      newdf[row, 11] = parselmouth.praat.call(pitch, "Get standard deviation", readStart, switchWord, "Hertz") #first half pitch sd
      newdf[row, 9] = readEnd - switchWord #second half reading time
      newdf[row, 12] = parselmouth.praat.call(pitch, "Get mean", switchWord, readEnd, "Hertz") #second half mean pitch
      newdf[row, 13] = parselmouth.praat.call(pitch, "Get standard deviation", switchWord, readEnd, "Hertz") #second half pitch sd
      newdf[row, 14] = preswitchStart - prePREswitchStart #prePREswitch group reading time
      newdf[row, 18] = parselmouth.praat.call(pitch, "Get mean", prePREswitchStart, preswitchStart, "Hertz") #prePREswitch group mean pitch
      newdf[row, 19] = parselmouth.praat.call(pitch, "Get standard deviation", prePREswitchStart, preswitchStart, "Hertz") #prePREswitch group pitch sd
      newdf[row, 15] = switchStart - preswitchStart #preswitch group reading time
      newdf[row, 20] = parselmouth.praat.call(pitch, "Get mean", preswitchStart, switchStart, "Hertz") #preswitch group mean pitch
      newdf[row, 21] = parselmouth.praat.call(pitch, "Get standard deviation", preswitchStart, switchStart, "Hertz") #preswitch group pitch sd
      newdf[row, 16] = postswitchStart - switchStart #switch group reading time
      newdf[row, 22] = parselmouth.praat.call(pitch, "Get mean", switchStart, postswitchStart, "Hertz") #switch group mean pitch
      newdf[row, 23] = parselmouth.praat.call(pitch, "Get standard deviation", switchStart, postswitchStart, "Hertz") #switch group pitch sd
      newdf[row, 17] = postswitchEnd - postswitchStart #prePREswitch group reading time
      newdf[row, 24] = parselmouth.praat.call(pitch, "Get mean", postswitchStart, postswitchEnd, "Hertz") #postswitch group mean pitch
      newdf[row, 25] = parselmouth.praat.call(pitch, "Get standard deviation", postswitchStart, postswitchEnd, "Hertz") #postswitch group pitch sd

  sublist = [sub_id] * newdf.shape[0]
  newdfplus = np.vstack((sublist, np.transpose(newdf)))
  dfout = np.transpose(np.vstack((newdfplus, df.iloc[:,0])))
  
  return dfout

#function to decrypt the wav file
def decrypt_wav(audiopath):
  stream = open(audiopath, 'rb')
  decrypted_wav = gpg.decrypt_file(stream)
  return decrypted_wav

#function to run each participant
def run_sub(sub, filelist, audacitypath, timestamps, dfmain):
  filename = filelist[sub]
  coded_filepath = timestamps + filename
  subject = filename.split("_")[0]
  sub_id = int(subject.split("-")[1])
  allsubaudio = os.listdir(audacitypath) #list of all files in the audio folder
  r = re.compile(".*(%s).*valence.*"%sub_id)
  subwav = list(filter(r.match, allsubaudio))[0] #find the subject's Valence .wav file
  audiopath = audacitypath + subwav
  #decrypted_wav = decrypt_wav(audiopath) #decrypt the .wav file
  audio = parselmouth.Sound(audiopath) #feed the .wav file to Parselmouth
  df = pd.read_excel(coded_filepath, header=0)
  npdf = df.to_numpy()
  pitch = audio.to_pitch_ac() #extract Parselmouth pitch object
  
  newdf = create_newdf(npdf) #create subject scaffold df
  
  dfout = extract_parselmouth(sub_id, pitch, df, newdf) #extract all sub data points from audio file with Parselmouth

  dfmain = np.vstack((dfmain, dfout)) #output array and append to dfmain
  
#  x = re.compile(".*wav") #find the decrypted .wav file
#  unencrypted_wave = list(filter(x.match, subaudio))[0]
#  os.remove(unencrypted_wave) #delete the decrypted .wav file
  
  return dfmain

### SECTION 2: FILE PATHS AND INITIALIZATION
#capture list of coded files
#hpc
#timestamps = "/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/valence-timing/"
#audacitypath = "/home/data/NDClab/datasets/rwe-eeg-dataset/sourcedata/raw/audio/"
#outpath = "/home/data/NDClab/datasets/rwe-eeg-dataset/derivatives/preprocessed/valence-timing/"

#local
timestamps = "/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/valence-timing/"
audacitypath = "/Users/jalexand/github/rwe-eeg-dataset/sourcedata/raw/audio/"
outpath = "/Users/jalexand/github/rwe-eeg-dataset/derivatives/preprocessed/valence-timing/"

#retrieve list of coded files
filelist = []
fulllist = os.listdir(timestamps)
for file in fulllist:
  if file.endswith(".xlsx"):
    filelist.append(file)

#initialize empty array
dfmain = np.empty((0,28))

### SECTION 3: START PARTICIPANT LOOP
#loop over subjects to read in coded timestamps and extract reading times and pitch information
for sub in range(len(filelist)):
  dfmain = run_sub(sub, filelist, audacitypath, timestamps, dfmain)

### SECTION 4: OUTPUT DATA
columns = ['id', 'readStart', 'prePREswitchStart', 'preswitchStart', 'switchStart',' switchWord', 'postswitchStart', \
          'postswitchEnd', 'readEnd', 'timeFirst', 'timeSecond', 'pitchMeanFirst', 'pitchSdFirst', 'pitchMeanSecond', \
          'pitchSdSecond', 'timePrePREswitch', 'timePreswitch', 'timeSwitch', 'timePostswitch', 'pitchMeanPrePREswitch', \
          'pitchSdPrePREswitch', 'pitchMeanPreswitch', 'pitchSdPreswitch',  'pitchMeanSwitch', 'pitchSdSwitch', 'pitchMeanPostswitch', 'pitchSdPostswitch', 'passage']
combodf = pd.DataFrame(dfmain, columns=columns)
combodf['id'] = combodf['id'].astype(int)
combodf.to_csv(outpath + "timingpitch_subject-by-passage" + today_formatted + ".csv", index=False)

### SECTION 5: UPDATE CENTRAL TRACKER FOR STUDY
#load central tracker
#track_path = '/home/data/NDClab/datasets/rwe-eeg/data-monitoring/central-tracker_rwe-eeg.csv'
trackerDat = pd.read_csv(track_path, header=0)

#valenceTiming_s1_r1_e1
for i in range(len(trackerDat['id'])):
  subid = trackerDat.iloc[i, 0]
  rowno = int(np.where(trackerDat["id"] == subid)[0])
  colno = trackerDat.columns.get_loc('valenceTiming_s1_r1_e1')
  if subid in np.unique(combodf['id']):
    trackerDat.iloc[rowno, colno] = 1
  else:
    trackerDat.iloc[rowno, colno] = 0
print("Updated valenceTiming_s1_r1_e1!")

#valencePitch_s1_r1_e1
for i in range(len(trackerDat['id'])):
  subid = trackerDat.iloc[i, 0]
  rowno = int(np.where(trackerDat["id"] == subid)[0])
  colno = trackerDat.columns.get_loc('valencePitch_s1_r1_e1')
  if subid in np.unique(combodf['id']):
    trackerDat.iloc[rowno, colno] = 1
  else:
    trackerDat.iloc[rowno, colno] = 0
print("Updated valencePitch_s1_r1_e1!")

#write back to central tracker
trackerDat.to_csv(track_path, index=False)
