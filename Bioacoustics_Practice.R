#Getting started ----

# Install devtools from CRAN if not already done
install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

#Southeast Asian monkey recordings ----

#We begin by dowloading the sound files provided by the behaviouR package
#To keep things organised, we will store them in a sub-folder called Focalrecordings 
#The phrase “Focal Recording” refers to all the audio events 
#produced by an animal in a specified time-period

# Now we will create folder in your RStudio Project called 'FocalRecordings'
dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

# Now we will load the sound files that were in the behaviouR package
githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

# Now we will save the recordings to the new folder you created as standard
# audio .wav format files; You do not need to understand the next set of code
# in detail
for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = ""))
}


#Importing and displaying an individual .wav file ----

#We can now begin importing the .wav files

GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")

GibbonWaveFile

duration(GibbonWaveFile) * GibbonWaveFile@samp.rate

GibbonWaveFile@samp.rate / 1000 #Because we work in kilohertz 

#Plotting the amplitude
oscillo(GibbonWaveFile)

oscillo(GibbonWaveFile, from = 0.1, to = 0.2) #Zooming into the recording to see more detail

oscillo(GibbonWaveFile, from = 0.15, to = 0.2)

#Creating a spectrogram ----

#A spectrogram shows how the spectrum of frequencies varies over time
#shows the time on the x-axis, and the frequencies on the y-axis. 
#It also shows the amplitude, but rather than doing so on a z-axis (3D model)

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500) #Zooming in again to focus on Gibbon

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500, Colors = "Colors") #Changing the colour for easier viewing 

#Can use ggspectro if we want as more customisable, but harder to create as dataframe needed

# It is sometimes a case of trial-and error to get the limits and spectro.colors
# at a suitable scale to see the information displayed nicely
v <- ggspectro(GibbonWaveFile, flim=c(0,2.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

v

#Displaying multiple spectrograms ----

#It is sometimes useful to compare multiple spectrograms side-by-side for ease of comparison.

# We can tell R to print the spectrograms 2x2 using the code below
par(mfrow = c(2, 2))

# This is the function to create the spectrograms
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, max.freq = 2500,
                    Colors = "Colors")

par(mfrow = c(1,1))

#Simplifying the audio data to allow multivariate analysis ----

#Mel-frequency cepstral coefficients (MFCC) 

#Feature extraction is a way of simplifying very complex data to a much simplifer form

#For audio data MFCC has proved to be a particularly robust technique
#It involves the following main steps: 

#Take the Fourier transform of the audio signal. Imagine you played the note C-natural on a piano: you would hear a single note. Then imagine you played A-natural.
#Again a single note. But if you play a two-note chord C and A, the waveforms interact with each other, which is why you hear a chord. Fourier transform helps disentangle patterns in audio based on the underlying sounds.
#The mathematics are complex, but there is a nice graphical explanation at https://youtu.be/spUNpyF58BY
#Convert the powers of the key frequencies detected by the Fourier transform using a “Mel scale”.
#A mel scale, named after melody, is a perceptual scale of pitches, judged to be equal frequency apart from each other.
#It uses triangular overlapping windows, laid on top of the Fourier output, to simplify the information.
#Take logarithms of the values in each overlapping window
#Take the cosine of the logarithms
#The coefficients are the amplitudes of the resulting spectra

#Fortunately we can process all huge .Wav files in one go by using
FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")

dim(FeatureDataframe) #Remember the original file had >500,000 events

View(FeatureDataframe)

#We can now push this through PCA analysis, like we did for NES8010

library(vegan) # This should already be installed from NES8010
source("nes8010.R") #Remember to go and copy paste this from NES8010 folder

# Use [, -1] to keep all rows but omit first column
acoustics_pca <- ordi_pca(FeatureDataframe[, -1], scale=TRUE)
summary(acoustics_pca)

ordi_plot(acoustics_pca, display="sites") #Visualising the PCA results, don't need species or columns, however.

#Plot shows three groups for three different recordings. 

acoustics_sco <- ordi_scores(acoustics_pca, display="sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_chull(alpha=0.5) +
  scale_color_discrete(name = "Monkey Type",
                      labels = c("Female Gibbon", "Great Arugus", "Male Solo")) +
  geom_point() #Can plot this as ggplot if wan't more freedom with edits. Plus conformation from before!

#Edit this further with ggrepl package
library(ggrepel) 

#Songbird analysis ---- 
#The xeno-canto dataset provides a huge international dataset of 
#Citizen Science acoustic data
#Quality is variable though
#warbleR package has options to help reduce the bulk of the data from xeno-canto 

install.packages("warbleR")
library(warbleR)

#Searching for blackbird data first
#restriced to 5-25 seconds in length 

#False is there to stop us downloading it, just checking they are there

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)

blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)

#Good thing with warbleR is that we can create interactive maps with leaflet
#w popups at each location 

map_xc(blackbird_songs, leaflet.map = TRUE)

#Now we can download them for further analysis, making sure to create a new subfolder
# + convernt from mp3 to .Wav

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")

#If we look in the finder section we notice that some are named different to how we want them named

#The following code looks a bit fierce, but conceptually it is quite simple. It:
  
#Obtains a list of all the file names in the blackbird_songs subfolder
#Creates an empty R object called new_files which will contain the new filenames
#Takes the name of each file, and uses str_split to sub-divide it into 3 pieces, based on where the dash "-" symbol is (look at the original filename and there are 2 dashes).
#Goes through a loop (the for function) to repeat a set of commands for each file
#Concatenate the bits of the filename together using str_c, keeping the first two components together to create Turdusmerula, adding in text saying -song_ which ends with an underscore, and adding in the last component, the xxxxxx.mp3 from the file.
#Add this name to the new_files object, which gradually gets longer as you loop through each filename.
#Rename all the files.

library(stringr) # part of tidyverse

old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Do the same for alarm
old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Now that you have renamed all your original .MP3 files,
#it is simply a case of copying them over to a new subfolder in your RStudio Project, 
#called blackbird_audio

dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

#Changing from mp3 to .Wav

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

#Visualise and analyse the song and alarm calls ----
#Single blackbird, oscillogram and spectrogram

blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")

blackbird_wav

oscillo(blackbird_wav) #May need to zoom in here to see more detail

oscillo(blackbird_wav, from = 0.59, to = 0.60) #Could split the alarms up if wanted...

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors") #Creating a spectogram of the waves

#MFCC of blackbird song and alarm calls ----
#we are going to simplify the blackbird data through feature extraction using 
#the Mel-frequency cepstral coefficient technique (MFCC)
#We do need to change the default hertz though as blackbird exceeds the default limits
#(2000 Hz)

blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)
dim(blackbird_mfcc)

#Now do PCA of it

blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca) #Note the lower scores compared to monkey

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_chull(alpha=0.5) +
  scale_color_discrete(name = "Call Type",
                       labels = c("Alarm", "Song")) +
                      geom_point() 



