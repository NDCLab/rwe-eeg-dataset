# Code

This folder contains the scripts used to create preprocessed derivatives:

| script | input | output |
|:-- | :-- | :-- |
| preprocDarwin.R | participant-specific records directly from PsychoPy | two files for the Darwin passages, one of which is subject-level results and one of which is passage-level results for all subjects |
| preprocValence.R | participant-specific records directly from PsychoPy | two files for the Valence passages, one of which is subject-level results and one of which is passage-level results for all subjects |
| syllMatch.R | participant-specific records coded from the task audio recording, based upon the error-coding protocols | three files: one with the total number of errors, per type, coded for each participant within each passage; one with each error syllable that was selected and matched to a correct syllable; and all error/correct syllables with their associated marker for insertion into the EEG data once the corresponding timestamp has been manually recorded |
| raterReliability_by-passage.R | participant-specific records coded from the task audio recording, based upon the error-coding protocols | one file with kappa values for each error type on a per subject, per passage basis |

In addition, this folder contains a "scaffolds" file that provides, for each passage, an architecture to the relation between words and syllables, as well as a copy of the SUBTLEXus corpus downloaded 06/13/2022.

The items in the peakRate folder are currently in-progress.