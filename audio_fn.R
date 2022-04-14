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

rand_wav <- function(wav_list, samp.rate, n.sec, nrand.in) {
  # Randomly get single samples at samp.rate, and cut out a n.sec chunk
  rnd_list <- wav_list[sample(length(wav_list), nrand.in)]
  subsamp <- lapply(1:nrand.in, function(i) readWave(rnd_list[[i]]))
  subsamp <- lapply(1:nrand.in, function(i) my_resamp(subsamp[[i]],
                                                      g = samp.rate,
                                                      output = "Wave"))
  subsamp_dur <- unlist(lapply(1:nrand.in, function(i) duration(subsamp[[i]])))
  subsamp_rnd_start <- runif(nrand.in, min=0, max=subsamp_dur-n.sec)
  for(j in 1:nrand.in){
    if(subsamp_dur[[j]] <= n.sec)
      stop("Duration length to long for cutting")
  }
  subsamp <- lapply(1:nrand.in, function(i) cutw(subsamp[[i]],
                                                 from = subsamp_rnd_start[i],
                                                 to = subsamp_rnd_start[i] + n.sec,
                                                 output = "Wave"))
}


rnd_mix <- function(input.dir1, input.dir2, input.dir3,
                    output.dir1, output.dir2, output.dir3, n.sec=5,
                    nrand.in = 25, seed=123, samp.rate=44100){
  set.seed(seed)
  wav_list1 <- list.files(input.dir1, full.names = T, pattern = ".wav")
  wav_list2 <- list.files(input.dir2, full.names = T, pattern = ".wav")
  wav_list3 <- list.files(input.dir3, full.names = T, pattern = ".wav")
  
  # Random samples n.sec duration of single spp call
  wav_samp_spp_a <- rand_wav(wav_list1, samp.rate, n.sec, nrand.in)
  wav_samp_spp_b <- rand_wav(wav_list2, samp.rate, n.sec, nrand.in)
  wav_samp_spp_c <- rand_wav(wav_list3, samp.rate, n.sec, nrand.in)
  wav_samp_spp_a <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_a[[i]], unit = "16"))
  wav_samp_spp_b <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_b[[i]], unit = "16"))
  wav_samp_spp_c <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_c[[i]], unit = "16"))
  dir.create(file.path(output.dir1), recursive = TRUE)
  for(i in 1:nrand.in){
    writeWave(wav_samp_spp_a[[i]], paste0(output.dir1, "/a_", i, ".wav"))
    writeWave(wav_samp_spp_b[[i]], paste0(output.dir1, "/b_", i, ".wav"))
    writeWave(wav_samp_spp_c[[i]], paste0(output.dir1, "/c_", i, ".wav"))
  }
  
  # Random samples n.sec duration for mixing into two spp calls
  wav_samp_spp_a <- rand_wav(wav_list1, samp.rate, n.sec, nrand.in)
  wav_samp_spp_b <- rand_wav(wav_list2, samp.rate, n.sec, nrand.in)
  wav_samp_spp_c <- rand_wav(wav_list3, samp.rate, n.sec, nrand.in)
  rnd_spp_a_id <- sample(nrand.in, size = nrand.in)
  rnd_spp_b_id <- sample(nrand.in, size = nrand.in)
  rnd_spp_c_id <- sample(nrand.in, size = nrand.in)
  wav_samp_spp_a <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_a[[i]], unit = "16"))
  wav_samp_spp_b <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_b[[i]], unit = "16"))
  wav_samp_spp_c <- lapply(1:nrand.in, function(i) normalize(wav_samp_spp_c[[i]], unit = "16"))
  mix_ab <- lapply(1:nrand.in, function(i) normalize(Wave((wav_samp_spp_a[[i]]@left +
                                                   wav_samp_spp_b[[i]]@left),
                                                 samp.rate = samp.rate,
                                                 bit = 16),
                                                 unit = "16"))
  mix_ac <- lapply(1:nrand.in, function(i) normalize(Wave((wav_samp_spp_a[[i]]@left +
                                                   wav_samp_spp_c[[i]]@left),
                                                samp.rate = samp.rate,
                                                bit = 16),
                                                unit = "16"))
  mix_bc <- lapply(1:nrand.in, function(i) normalize(Wave((wav_samp_spp_b[[i]]@left +
                                                   wav_samp_spp_c[[i]]@left),
                                                samp.rate = samp.rate,
                                                bit = 16),
                                                unit = "16"))
  dir.create(file.path(output.dir2), recursive = TRUE)
  for(i in 1:nrand.in){
    writeWave(mix_ab[[i]], paste0(output.dir2, "/ab_", i, ".wav"))
    writeWave(mix_ac[[i]], paste0(output.dir2, "/ac_", i, ".wav"))
    writeWave(mix_bc[[i]], paste0(output.dir2, "/bc_", i, ".wav"))
  }
  
  # Random samples n.sec duration for mixing into three spp calls
  wav_samp_spp_a <- rand_wav(wav_list1, samp.rate, n.sec, nrand.in * 3)
  wav_samp_spp_b <- rand_wav(wav_list2, samp.rate, n.sec, nrand.in * 3)
  wav_samp_spp_c <- rand_wav(wav_list3, samp.rate, n.sec, nrand.in * 3)
  wav_samp_spp_a <- lapply(1:(nrand.in * 3), function(i) normalize(wav_samp_spp_a[[i]], unit = "16"))
  wav_samp_spp_b <- lapply(1:(nrand.in * 3), function(i) normalize(wav_samp_spp_b[[i]], unit = "16"))
  wav_samp_spp_c <- lapply(1:(nrand.in * 3), function(i) normalize(wav_samp_spp_c[[i]], unit = "16"))
  rnd_spp_a_id <- sample(nrand.in * 3, size = nrand.in * 3)
  rnd_spp_b_id <- sample(nrand.in * 3, size = nrand.in * 3)
  rnd_spp_c_id <- sample(nrand.in * 3, size = nrand.in * 3)
  mix_abc <- lapply(1:(nrand.in * 3), function(i) normalize(Wave((wav_samp_spp_a[[i]]@left +
                                                   wav_samp_spp_b[[i]]@left) +
                                                   wav_samp_spp_c[[i]]@left,
                                                samp.rate = samp.rate,
                                                bit = 16),
                                                unit = "16"
                                                ))
  dir.create(file.path(output.dir3), recursive = TRUE)
  for(i in 1:(nrand.in * 3)){
    writeWave(mix_abc[[i]], paste0(output.dir3, "/abc_", i, ".wav"))
  }
  
}
