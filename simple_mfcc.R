# Come up with a simple way of counting songs based on MFCC, SVM etc.

# devtools::install_github("https://github.com/DenaJGibbon/behaviouR")
# 
#library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(stringi)
library(stringr)
library(vegan)
source("audio_fn.R")

SONG_DIR <- "datasets/xeno_canto/"

sppA_name <- "Chloris chloris" #"Turdus merula"
sppB_name <- "Phylloscopus collybita"
sppC_name <- "Troglodytes troglodytes"
sppA <- query_xc(qword = paste(sppA_name, ' type:song len:5-25'), download = FALSE)
sppB <- query_xc(qword = paste(sppB_name, ' cnt:"united kingdom" len:5-25'), download = FALSE)
sppC <- query_xc(qword = paste(sppC_name, ' type:song len:5-25'), download = FALSE)

# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path(paste0(SONG_DIR, sppA_name)), recursive = TRUE)
dir.create(file.path(paste0(SONG_DIR, sppB_name)), recursive = TRUE)
dir.create(file.path(paste0(SONG_DIR, sppC_name)), recursive = TRUE)

# Download the .MP3 files into two separate sub-folders
query_xc(X = sppA, path=paste0(SONG_DIR, sppA_name))
query_xc(X = sppB, path=paste0(SONG_DIR, sppB_name))
query_xc(X = sppC, path=paste0(SONG_DIR, sppC_name))

# If you want to convert to .wav
mp32wav(path=paste0(SONG_DIR, sppA_name), dest.path=paste0(SONG_DIR, sppA_name))
mp32wav(path=paste0(SONG_DIR, sppB_name), dest.path=paste0(SONG_DIR, sppB_name))
mp32wav(path=paste0(SONG_DIR, sppC_name), dest.path=paste0(SONG_DIR, sppC_name))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, sppA_name), pattern="*.mp3")
file.remove(paste0(SONG_DIR, sppA_name, "/", unwanted_mp3))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, sppB_name), pattern="*.mp3")
file.remove(paste0(SONG_DIR, sppB_name, "/", unwanted_mp3))
unwanted_mp3 <- dir(path=paste0(SONG_DIR, sppC_name), pattern="*.mp3")
file.remove(paste0(SONG_DIR, sppC_name, "/", unwanted_mp3))


# sppA_wav <- readWave(paste0(SONG_DIR, sppA_name, "/Turdus-merula-243908.wav"))
# sppA_wav
# oscillo(sppA_wav)
# my_spec(sound.wav = sppA_wav, Colors = "Colors", main = sppA_name)

sppA_wav <- readWave(paste0(SONG_DIR, sppA_name, "/Chloris-chloris-25834.wav"))
sppA_wav
oscillo(sppA_wav, fastdisp = TRUE)
my_spec(sound.wav = sppA_wav, Colors = "Colors", main = sppA_name)


sppC_wav <- readWave(paste0(SONG_DIR, sppC_name, "/Troglodytes-troglodytes-642879.wav"))
sppC_wav
oscillo(sppC_wav)
my_spec(sound.wav = sppC_wav, Colors = "Colors", main = sppC_name, max.freq=10000)


# Copy files into a single folder
dir.create(file.path(paste0(SONG_DIR, "for_analysis")), recursive = TRUE)
unwanted_wav <- dir(path=paste0(SONG_DIR, "for_analysis", "/"), pattern="*.wav")
file.remove(paste0(SONG_DIR, "for_analysis", "/", unwanted_wav))
file.copy(from=paste0(paste0(SONG_DIR, sppA_name, "/"),
                      list.files(paste0(SONG_DIR, sppA_name))),
          to=paste0(SONG_DIR, "for_analysis"))
file.copy(from=paste0(paste0(SONG_DIR, sppB_name, "/"),
                      list.files(paste0(SONG_DIR, sppB_name))),
          to=paste0(SONG_DIR, "for_analysis"))
file.copy(from=paste0(paste0(SONG_DIR, sppC_name, "/"),
                      list.files(paste0(SONG_DIR, sppC_name))),
          to=paste0(SONG_DIR, "for_analysis"))


# MFCC analysis
# birds_mfcc <- MFCCFunction(input.dir = paste0(SONG_DIR, "for_analysis"),
#                                max.freq=8000)
# Remove any NaNs (usually silence in a broken recording)
# birds_mfcc <- birds_mfcc[!is.nan(rowSums(birds_mfcc[,-1])),]
# 
# birds_pca <- rda(birds_mfcc[, -1], scale = TRUE)
# head(summary(birds_pca))
# bird_sco <- data.frame(scores(birds_pca, display="sites"))
# bird_sco <- mutate(bird_sco, group_code = str_sub(birds_mfcc[, 1],
#                                                   1, as.vector(regexpr("\\-[^\\-]*$",
#                                                                        birds_mfcc[, 1]))-1))       
# ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
#   geom_point()


# Compare with d and dd
birds_mfcc <- my_mfcc(input.dir = paste0(SONG_DIR, "for_analysis"),
                           max.freq=8000)
# Remove any NaNs (usually silence in a broken recording)
birds_mfcc <- birds_mfcc[!is.nan(rowSums(birds_mfcc[,-1])),]

birds_pca <- vegan::rda(birds_mfcc[, -1], scale = TRUE)
head(summary(birds_pca))
bird_sco <- data.frame(vegan::scores(birds_pca, display="sites"))
bird_sco <- dplyr::mutate(bird_sco, group_code = stringr::str_sub(birds_mfcc[, 1],
                                                  1, as.vector(regexpr("\\-[^\\-]*$",
                                                                       birds_mfcc[, 1]))-1))       
ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

# knn classification
library(randomForest)
library(splitTools)
set.seed(123)
inds <- partition(1:nrow(birds_mfcc), p=c(train = 0.7, valid = 0.3)) # test optional
group_code <- str_sub(birds_mfcc[, 1],1, as.vector(regexpr("\\-[^\\-]*$", birds_mfcc[, 1]))-1)
birds_mfcc_train <- birds_mfcc[inds$train, -1]
birds_grps_train <- group_code[inds$train]
birds_mfcc_valid <- birds_mfcc[inds$valid, -1]
birds_grps_valid <- group_code[inds$valid]

# May want to compare with normalised
birds_mfcc_train <- scale(birds_mfcc_train)
birds_mfcc_valid <- scale(birds_mfcc_valid)

birds_rf <- randomForest(y=as.factor(birds_grps_train), x = birds_mfcc_train,
                         ntree = 100,
                         ytest=as.factor(birds_grps_valid), xtest=birds_mfcc_valid)
print(birds_rf)


# Create .wav files with one, two or three calls
input.dir1 <- paste0(SONG_DIR, sppA_name)
input.dir2 <- paste0(SONG_DIR, sppB_name)
input.dir3 <- paste0(SONG_DIR, sppC_name)
seed=123
nrand.in=100
output.dir1 <- paste0(SONG_DIR, "one")
output.dir2 <- paste0(SONG_DIR, "two")
output.dir3 <- paste0(SONG_DIR, "three")
n.sec <- 5
samp.rate <- 44100
unwanted_wav <- dir(path=paste0(output.dir1, "/"), pattern="*.wav")
file.remove(paste0(output.dir1, "/", unwanted_wav))
unwanted_wav <- dir(path=paste0(output.dir2, "/"), pattern="*.wav")
file.remove(paste0(output.dir2, "/", unwanted_wav))
unwanted_wav <- dir(path=paste0(output.dir3, "/"), pattern="*.wav")
file.remove(paste0(output.dir3, "/", unwanted_wav))

rnd_mix(input.dir1=input.dir1, input.dir2=input.dir2, input.dir3=input.dir3,
        output.dir1=output.dir1, output.dir2=output.dir2, output.dir3=output.dir3,
        n.sec=5, nrand.in=nrand.in)

# Copy mixture files into single folder for mfcc
dir.create(file.path(paste0(SONG_DIR, "mixture")), recursive = TRUE)
unwanted_wav <- dir(path=paste0(SONG_DIR, "mixture", "/"), pattern="*.wav")
file.remove(paste0(SONG_DIR, "mixture", "/", unwanted_wav))

file.copy(from=paste0(paste0(SONG_DIR, "one/"),
                      list.files(paste0(SONG_DIR, "one"))),
          to=paste0(SONG_DIR, "mixture"))
file.copy(from=paste0(paste0(SONG_DIR, "two/"),
                      list.files(paste0(SONG_DIR, "two"))),
          to=paste0(SONG_DIR, "mixture"))
file.copy(from=paste0(paste0(SONG_DIR, "three/"),
                      list.files(paste0(SONG_DIR, "three"))),
          to=paste0(SONG_DIR, "mixture"))

# MFCC on mixture
mix_mfcc <- my_mfcc(input.dir = paste0(SONG_DIR, "mixture"),
                      max.freq=8000)
# Remove any NaNs (usually silence in a broken recording)
mix_mfcc <- mix_mfcc[!is.nan(rowSums(mix_mfcc[,-1])),]

# Setup MFCC mixture for Random Forest
set.seed(123)
inds <- partition(1:nrow(mix_mfcc), p=c(train = 0.7, valid = 0.3)) # test optional
n_char <- nchar(mix_mfcc[, 1])
group_code <- NULL
for(i in 1:length(n_char)){
  if(n_char[i] == 1)
    group_code <- c(group_code, "one")
  if(n_char[i] == 2)
    group_code <- c(group_code, "two")
  if(n_char[i] == 3)
    group_code <- c(group_code, "three")
}
mix_mfcc_train <- mix_mfcc[inds$train, -1]
mix_grps_train <- factor(group_code[inds$train], levels = c("one", "two", "three"))
mix_mfcc_valid <- mix_mfcc[inds$valid, -1]
mix_grps_valid <- factor(group_code[inds$valid], levels = c("one", "two", "three"))

# Run the random forest
mix_mfcc_train <- scale(mix_mfcc_train) # do we need to rescale??
mix_mfcc_valid <- scale(mix_mfcc_valid)

mix_rf <- randomForest(y=as.factor(mix_grps_train), x = mix_mfcc_train,
                         ntree = 100,
                         ytest=as.factor(mix_grps_valid), xtest=mix_mfcc_valid)
print(mix_rf)


# Explore how well sound indices from warbleR spectro_analysis work
# Create a data.frame with columns suitable for selection table input
sel_df <- data.frame(sound.files = list.files(paste0(SONG_DIR, "mixture"),
                                              full.names = TRUE),
                     channel     = rep(1, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))),
                     selec       = rep(1, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))),
                     start       = rep(0, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))),
                     end         = rep(5, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))),
                     bottom.freq = rep(0, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))),
                     top.freq    = rep(10, length(list.files(paste0(SONG_DIR,
                                                                   "mixture")))))
mix_sel <- selection_table(X = sel_df, extended = TRUE)
mix_stats <- spectro_analysis(mix_sel, parallel = 4)

# Set up for training and validation
set.seed(123)
inds <- splitTools::partition(1:nrow(mix_stats), p=c(train = 0.7, valid = 0.3)) # test optional
u_char <- stringr::str_locate(mix_stats[, 1], "_")[,1]
group_code <- NULL
for(i in 1:length(u_char)){
  if(u_char[i] == 2)
    group_code <- c(group_code, "one")
  if(u_char[i] == 3)
    group_code <- c(group_code, "two")
  if(u_char[i] == 4)
    group_code <- c(group_code, "three")
}

mix_stats_train <- mix_stats[inds$train, -(1:3)]
mix_grps_train <- factor(group_code[inds$train], levels = c("one", "two", "three"))
mix_stats_valid <- mix_stats[inds$valid, -(1:3)]
mix_grps_valid <- factor(group_code[inds$valid], levels = c("one", "two", "three"))

# Random forest on sound statistics
mix_stats_train <- scale(mix_stats_train) # do we need to rescale??
mix_stats_valid <- scale(mix_stats_valid)

mix_stats_rf <- randomForest::randomForest(y=as.factor(mix_grps_train), x = mix_stats_train,
                       ntree = 100,
                       ytest=as.factor(mix_grps_valid), xtest=mix_stats_valid)
print(mix_stats_rf)

