# Come up with a simple way of counting songs based on MFCC, SVM etc.

# devtools::install_github("https://github.com/DenaJGibbon/behaviouR")
# 
library(behaviouR)
# library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(stringi)
library(vegan)

SONG_DIR <- "datasets/xeno_canto/"

blackbird <- query_xc(qword = 'Turdus merula type:song len:5-25', download = FALSE)
chiffchaff <- query_xc(qword = 'Phylloscopus collybita cnt:"united kingdom" len:5-25', download = FALSE)
wren      <- query_xc(qword = 'Troglodytes troglodytes type:song len:5-25', download = FALSE)

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path(paste0(SONG_DIR, "blackbird")), recursive = TRUE)
dir.create(file.path(paste0(SONG_DIR, "chiffchaff")), recursive = TRUE)
dir.create(file.path(paste0(SONG_DIR, "wren")), recursive = TRUE)

# Download the .MP3 files into two separate sub-folders
query_xc(X = blackbird, path=paste0(SONG_DIR, "blackbird"))
query_xc(X = chiffchaff, path=paste0(SONG_DIR, "chiffchaff"))
query_xc(X = wren, path=paste0(SONG_DIR, "wren"))

# If you want to convert to .wav
mp32wav(path=paste0(SONG_DIR, "blackbird"), dest.path=paste0(SONG_DIR, "blackbird"))
mp32wav(path=paste0(SONG_DIR, "chiffchaff"), dest.path=paste0(SONG_DIR, "chiffchaff"))
mp32wav(path=paste0(SONG_DIR, "wren"), dest.path=paste0(SONG_DIR, "wren"))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, "blackbird"), pattern="*.mp3")
file.remove(paste0(SONG_DIR, "blackbird/", unwanted_mp3))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, "chiffchaff"), pattern="*.mp3")
file.remove(paste0(SONG_DIR, "chiffchaff/", unwanted_mp3))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, "wren"), pattern="*.mp3")
file.remove(paste0(SONG_DIR, "wren/", unwanted_mp3))


blackbird_wav <- readWave(paste0(SONG_DIR, "blackbird/Turdus-merula-243908.wav"))
blackbird_wav
oscillo(blackbird_wav)

SpectrogramSingle(sound.file = paste0(SONG_DIR, "/blackbird/Turdus-merula-243908.wav"),
                  Colors = "Colors")


# Copy files into a single folder
dir.create(file.path(paste0(SONG_DIR, "for_analysis")), recursive = TRUE)
file.copy(from=paste0(paste0(SONG_DIR, "blackbird/"),
                      list.files(paste0(SONG_DIR, "blackbird"))),
          to=paste0(SONG_DIR, "for_analysis"))
file.copy(from=paste0(paste0(SONG_DIR, "chiffchaff/"),
                      list.files(paste0(SONG_DIR, "chiffchaff"))),
          to=paste0(SONG_DIR, "for_analysis"))
file.copy(from=paste0(paste0(SONG_DIR, "wren/"),
                      list.files(paste0(SONG_DIR, "wren"))),
          to=paste0(SONG_DIR, "for_analysis"))


# MFCC analysis
birds_mfcc <- MFCCFunction(input.dir = paste0(SONG_DIR, "for_analysis"),
                               max.freq=8000)
birds_pca <- rda(birds_mfcc[, -1], scale = TRUE)
head(summary(birds_pca))
bird_sco <- data.frame(scores(birds_pca, display="sites"))
bird_sco <- mutate(bird_sco, group_code = str_sub(birds_mfcc[, 1],
                                                  1, as.vector(regexpr("\\-[^\\-]*$",
                                                                       birds_mfcc[, 1]))-1))       
ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

