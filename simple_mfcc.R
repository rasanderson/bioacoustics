# Come up with a simple way of counting songs based on MFCC, SVM etc.

# devtools::install_github("https://github.com/DenaJGibbon/behaviouR")
# 
library(behaviouR)
# library(tuneR)
# library(seewave)
library(ggplot2)
library(dplyr)
# 
library(warbleR)
library(Sim.DiffProc)

#blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)
blackbird_songs <- query_xc(qword = 'Turdus merula type:song len:5-25', download = FALSE)
chiffchaff_songs <- query_xc(qword = 'Phylloscopus collybita cnt:"united kingdom" len:5-25', download = FALSE)

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("datasets/xeno_canto/blackbird_songs"), recursive = TRUE)
dir.create(file.path("datasets/xeno_canto/chiffchaff_songs"), recursive = TRUE)

# Download the .MP3 files into two separate sub-folders
query_xc(X = blackbird_songs, path="datasets/xeno_canto/blackbird_songs")
query_xc(X = chiffchaff_songs, path="datasets/xeno_canto/chiffchaff_songs")

library(stringr) # part of tidyverse

old_files <- list.files("datasets/xeno_canto/blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

old_files <- list.files("datasets/xeno_canto/chiffchaff_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-songs_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

dir.create(file.path("datasets/xeno_canto/birds"), recursive = TRUE)
file.copy(from=paste0("datasets/xeno_canto/blackbird_songs/",
                      list.files("datasets/xeno_canto/blackbird_songs")),
          to="datasets/xeno_canto/birds")
file.copy(from=paste0("datasets/xeno_canto/chiffchaff_songs/",
                      list.files("datasets/xeno_canto/chiffchaff_songs")),
          to="datasets/xeno_canto/birds")

mp32wav(path="datasets/xeno_canto/birds", dest.path="datasets/xeno_canto/birds")
unwanted_mp3 <- dir(path="datasets/xeno_canto/birds", pattern="*.mp3")
file.remove(paste0("datasets/xeno_canto/birds/", unwanted_mp3))

blackbird_wav <- readWave("datasets/xeno_canto/birds/Turdusmerula-song_243908.wav")
blackbird_wav
oscillo(blackbird_wav)

SpectrogramSingle(sound.file = "datasets/xeno_canto/birds/Turdusmerula-song_243908.wav",
                  Colors = "Colors")

birds_mfcc <- MFCCFunction(input.dir = "datasets/xeno_canto/birds",
                               max.freq=7000)
birds_pca <- rda(birds_mfcc[, -1], scale = TRUE)
summary(birds_pca)
bird_sco <- data.frame(scores(birds_pca, display="sites"))
bird_sco <- mutate(bird_sco, group_code = birds_mfcc$Class)

ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

dim(blackbird_mfcc)

#  global parameters
warbleR_options(wav.path = "datasets/xeno_canto/birds")


sound.files <- list.files("datasets/xeno_canto/birds")
sound.files <- sound.files[1:(length(sound.files))]
selec       <- rep(1, length(sound.files))
start       <- rep(1, length(sound.files))
end         <- rep(5, length(sound.files)) # Stop after 5 s for speed

xc_files <- data.frame(sound.files = sound.files,
                             selec = selec,
                             start = start,
                             end   = end)

ext_st <- selection_table(X = xc_files, extended = TRUE)
wv1 <- read_wave(X = ext_st, index = 3, from = 0, to = 0.37)
wv1
spectro(wv1, wl = 150, grid = FALSE, scale = FALSE, ovlp = 90)
spectro(wv1)

spec_anal <- spectro_analysis(ext_st, bp = c(0,5))
mel_st <- mfcc_stats(ext_st, bp = c(0,5))
mean_mel <- mel_st[,78:102]
library(vegan)
mean_mel_pca <- rda(mean_mel, scale = TRUE)
summary(mean_mel_pca)
par(mfrow=c(1,1))
plot(mean_mel_pca, display="sites")
mean_mel_sco <- data.frame(scores(mean_mel_pca, display="sites"),
                           species = c(rep("Chiffchaff", 260), rep("Blackbird", 252)))
ggplot(mean_mel_sco, aes(x = PC1, y = PC2, colour = species)) +
  geom_point()
