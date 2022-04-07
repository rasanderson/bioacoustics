# Audio I/O and pre-processing with torchaudio
# https://cran.rstudio.com/web/packages/torchaudio/vignettes/audio_preprocessing_tutorial.html

library(torchaudio)
library(viridis)

# Opening a file
# torchaudio also supports loading sound files in the wav and mp3 format.
# We call waveform the resulting raw audio signal.
url = "https://pytorch.org/tutorials/_static/img/steam-train-whistle-daniel_simon-converted-from-mp3.wav"
filename = tempfile(fileext = ".wav")
r = httr::GET(url, httr::write_disk(filename, overwrite = TRUE))

waveform_and_sample_rate = transform_to_tensor(tuneR_loader(filename))
waveform = waveform_and_sample_rate[[1]]
sample_rate = waveform_and_sample_rate[[2]]

paste("Shape of waveform: ", paste(dim(waveform), collapse = " "))
paste("Sample rate of waveform: ", sample_rate)

plot(waveform[1], col = "royalblue", type = "l")
lines(waveform[2], col = "orange")

# Transformations
# torchaudio supports a growing list of transformations.
# 
# Resample: Resample waveform to a different sample rate.
# Spectrogram: Create a spectrogram from a waveform.
# GriffinLim: Compute waveform from a linear scale magnitude spectrogram using
#  the Griffin-Lim transformation.
# ComputeDeltas: Compute delta coefficients of a tensor, usually a spectrogram.
# ComplexNorm: Compute the norm of a complex tensor.
# MelScale: This turns a normal STFT into a Mel-frequency STFT, using a conversion matrix.
# AmplitudeToDB: This turns a spectrogram from the power/amplitude scale to the decibel scale.
# MFCC: Create the Mel-frequency cepstrum coefficients from a waveform.
# MelSpectrogram: Create MEL Spectrograms from a waveform using the STFT function in Torch.
# MuLawEncoding: Encode waveform based on mu-law companding.
# MuLawDecoding: Decode mu-law encoded waveform.
# TimeStretch: Stretch a spectrogram in time without modifying pitch for a given rate.
# FrequencyMasking: Apply masking to a spectrogram in the frequency domain.
# TimeMasking: Apply masking to a spectrogram in the time domain.
# Each transform supports batching: you can perform a transform on a single raw
#  audio signal or spectrogram, or many of the same shape.
# 
# Since all transforms are torch::nn_modules, they can be used as part of a neural network at any point.
# 
# To start, we can look at the log of the spectrogram on a log scale.

specgram <- transform_spectrogram()(waveform)

paste("Shape of spectrogram: ", paste(dim(specgram), collapse = " "))

specgram_as_array <- as.array(specgram$log2()[1]$t())
image(specgram_as_array[,ncol(specgram_as_array):1], col = viridis(n = 257,  option = "magma"))

# Or we can look at the Mel Spectrogram on a log scale.
specgram <- transform_mel_spectrogram()(waveform)

paste("Shape of spectrogram: ", paste(dim(specgram), collapse = " "))

specgram_as_array <- as.array(specgram$log2()[1]$t())
image(specgram_as_array[,ncol(specgram_as_array):1], col = viridis(n = 257,  option = "magma"))

# We can resample the waveform, one channel at a time.
new_sample_rate <- sample_rate/10

# Since Resample applies to a single channel, we resample first channel here
channel <- 1
transformed <- transform_resample(sample_rate, new_sample_rate)(waveform[channel, ]$view(c(1,-1)))

paste("Shape of transformed waveform: ", paste(dim(transformed), collapse = " "))

plot(transformed[1], col = "royalblue", type = "l")

# As another example of transformations, we can encode the signal based on Mu-Law enconding.
# But to do so, we need the signal to be between -1 and 1. Since the tensor is just a
# regular PyTorch tensor, we can apply standard operators on it.
# Let's check if the tensor is in the interval [-1,1]
cat(sprintf("Min of waveform: %f \nMax of waveform: %f \nMean of waveform: %f", as.numeric(waveform$min()), as.numeric(waveform$max()), as.numeric(waveform$mean())))

#Since the waveform is already between -1 and 1, we do not need to normalize it.

normalize <- function(tensor) {
  # Subtract the mean, and scale to the interval [-1,1]
  tensor_minusmean <- tensor - tensor.mean()
  return(tensor_minusmean/tensor_minusmean$abs()$max())
}

# Let's normalize to the full interval [-1,1]
# waveform = normalize(waveform)

# Let’s apply encode the waveform.

transformed <- transform_mu_law_encoding()(waveform)

paste("Shape of transformed waveform: ", paste(dim(transformed), collapse = " "))

plot(transformed[1], col = "royalblue", type = "l")

# And now decode.

reconstructed <- transform_mu_law_decoding()(transformed)

paste("Shape of recovered waveform: ", paste(dim(reconstructed), collapse = " "))

plot(reconstructed[1], col = "royalblue", type = "l")

# We can finally compare the original waveform with its reconstructed version.
# Compute median relative difference
err <- as.numeric(((waveform - reconstructed)$abs() / waveform$abs())$median())
paste("Median relative difference between original and MuLaw reconstucted signals:", scales::percent(err, accuracy = 0.01))
