my_mfcc <- function (input.dir, min.freq = 200, max.freq = 2000, n.windows = 9, 
          num.cep = 12) 
{
  call.timing.list <- list.files(input.dir, full.names = T, pattern = ".wav")
  call.timing.list.short <- list.files(input.dir, full.names = F, pattern = ".wav")
  subsamps <- lapply(1:length(call.timing.list), function(i) readWave(call.timing.list[[i]]))
  mfcc.vector.list <- list()
  Class <- stringr::str_split_fixed(call.timing.list.short, pattern = "_", n = 2)[, 1]
  for (x in 1:length(subsamps)) {
    print(paste("processing sound event", x, "out of", length(subsamps)))
    short.wav <- subsamps[[x]]
    wav.dur <- duration(short.wav)
    win.time <- wav.dur/n.windows
    melfcc.output <- tuneR::melfcc(short.wav,
                                   minfreq = min.freq, 
                                   hoptime = win.time,
                                   maxfreq = max.freq,
                                   numcep = num.cep, 
                                   wintime = win.time)
    deltas.output  <- tuneR::deltas(melfcc.output)
    deltas2.output <- tuneR::deltas(deltas.output)
    mfcc.vector <- c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])),
                     as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])),
                     as.vector(t(deltas2.output[1:(n.windows - 1), 2:num.cep])))
                     #wav.dur)
    mfcc.vector.list[[x]] <- mfcc.vector
  }
  mfcc.output <- mfcc.vector.list
  Class <- stringr::str_split_fixed(call.timing.list.short, 
                                    pattern = "_", n = 2)[, 1]
  mfcc.output.df <- do.call(rbind.data.frame, mfcc.output)
  colnames(mfcc.output.df) <- seq(from = 1, to = ncol(mfcc.output.df), by = 1)
  mfcc.output.df <- cbind.data.frame(Class, mfcc.output.df)
  return(mfcc.output.df)
}

my_spec <- function (sound.wav, min.freq = 500, max.freq = 8000, Colors = "Colors", 
          downsample = TRUE, downsample.new = 16000, main = NULL) 
{
  short.wav <- sound.wav # readWave(sound.file)
  if (downsample == TRUE) {
    if (short.wav@samp.rate > 20000) {
      short.wav <- downsample(short.wav, downsample.new)
    }
  }
  #Name <- stringr::str_split_fixed(sound.file, pattern = ".wav", 
  #                                 n = 2)[, 1]
  #Name <- stringr::str_split_fixed(Name, pattern = "/", 
  #                                 n = 2)[, 2]
  short.wav <- tuneR::normalize(short.wav)
  temp.spec <- signal::specgram(short.wav@left, Fs = short.wav@samp.rate, 
                                n = 1024, overlap = 95)
  if (Colors == "BW") {
    print(plot(temp.spec, xlab = "Time (s)", ylab = "Frequency (Hz)", 
               ylim = c(min.freq, max.freq), rev(gray(0:255/255)), 
               axes = T, useRaster = TRUE, main = main))
  }
  if (Colors == "Colors") {
    print(plot(temp.spec, xlab = "Time (s)", ylab = "Frequency (Hz)", 
               ylim = c(min.freq, max.freq), (matlab::jet.colors(256)), 
               axes = T, useRaster = TRUE, main = main))
  }
}

# for debugging rnd_mix function
input.dir1 <- paste0(SONG_DIR, "blackbird")
input.dir2 <- paste0(SONG_DIR, "chiffchaff")
input.dir3 <- paste0(SONG_DIR, "wren")
seed=123
nrand.in=5
nrand.out=3
output.dir1 <- "one"
output.dir2 <- "two"
output.dir3 <- "three"
n.sec <- 5
samp.rate <- 44100

# Need to adapt resamp to avoid error being thrown if new samp freq unchanged
my_resamp <- function (wave, f, g, channel = 1, output = "matrix") 
{
  input <- inputw(wave = wave, f = f, channel = channel)
  wave <- input$w
  f <- input$f
  bit <- input$bit
  rm(input)$w
  n <- nrow(wave)
  if (g == f) 
    wave1 <- wave
  if (g < f) {
    r <- f/g
    wave1 <- wave[seq(1, n, by = r), 1]
  }
  if (g > f) {
    s <- (n * g)/f
    wave1 <- approx(wave, n = s)$y
  }
  wave1 <- outputw(wave = wave1, f = g, format = output)
  return(wave1)
}

rnd_mix <- function(input.dir1, input.dir2, input.dir3,
                    output.dir1, output.dir2, output.dir3, n.sec=5,
                    nrand.in = 25, nrand.out = 3, seed=123, samp.rate=44100){
  set.seed(seed)
  wav_list1 <- list.files(input.dir1, full.names = T, pattern = ".wav")
  wav_list2 <- list.files(input.dir2, full.names = T, pattern = ".wav")
  wav_list3 <- list.files(input.dir3, full.names = T, pattern = ".wav")
  rnd_list1 <- wav_list1[sample(length(wav_list1), nrand.in)]
  rnd_list2 <- wav_list2[sample(length(wav_list2), nrand.in)]
  rnd_list3 <- wav_list3[sample(length(wav_list3), nrand.in)]
  
  # Randomly get single samples at samp.rate, and cut out a n.sec chunk
  subsamp1 <- lapply(1:length(rnd_list1), function(i) readWave(rnd_list1[[i]]))
  subsamp1 <- lapply(1:length(rnd_list1), function(i) my_resamp(subsamp1[[i]],
                                                              g = samp.rate,
                                                              output = "Wave"))
  subsamp1_dur <- unlist(lapply(1:length(rnd_list1), function(i) duration(subsamp1[[i]])))
  subsamp_rnd_start <- runif(5, min=0, max=unlist(subsamp1_dur)-n.sec)
  for(j in 1:length(rnd_list1)){
    if(subsamp1_dur[[j]] <= n.sec)
      stop("Duration length to long for cutting")
    
  }
  subsamp1 <- lapply(1:length(rnd_list1), function(i) cutw(subsamp1[[i]],
                                                           from=subsamp_rnd_start[i],
                                                           to=subsamp_rnd_start[i]+n.sec,
                                                           output="Wave"))
  
}