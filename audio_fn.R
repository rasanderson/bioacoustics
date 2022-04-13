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
