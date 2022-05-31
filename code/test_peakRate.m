% Extracting peakRate Data from Audio
% Authors: Jessica M. Alexander, George A. Buzzell
% Last Updated: 2022-05-26

today = char(datetime("today", 'Format', 'yyyy-MM-dd'));

% set up directories
main_dir = '/Users/jalexand/github/rwe-eeg-dataset/sourcedata/raw/audacity-split';
code_dir = '/Users/jalexand/github/rwe-eeg-dataset/code';
cd(code_dir)

% read in audio
input_audio = 'sub-210001_rwe_darwin_s1_r1_e1_3-in-a-broad_MONO.wav';
filename_parts = strsplit(input_audio, "_");
temp = strsplit(char(filename_parts(7)), ".");
id = char(filename_parts(1));
passage = char(temp(1));
[y,Fs] = audioread([main_dir filesep input_audio]);

% downsample to 16000 Hz
% 44100 * (160/441) -- handy guide for finding resampling ratios
% downsampleDat = resample(y,160,441);
% Fs_downsample = 16000;

% run find_peakRate from Oganian
[env, peakRate, peakEnv] = find_peakRate(y, Fs);

% Calculate time points (in seconds)
t = (1/100):(1/100):(length(peakRate)/100);

% create output array
syllPeaks = zeros(length(peakRate), 2);
syllPeaks(:,1) = peakRate'; %insert peakRate data as column 1
syllPeaks(:,2) = t; %insert time points as column 2
syllPeaks_output = syllPeaks(syllPeaks(:,1)>0, :);

% output to csv
writematrix(syllPeaks_output, ['syllable-onsets_', id, '_', passage, '_', datestr(now,'yyyymmdd'), '.csv']);


