
library(tuneR)
setWavPlayer('afplay')	

stft = function(y,H,N) {
  v = seq(from=0,by=2*pi/N,length=N)     
  win = (1 + cos(v-pi))/2                            					#Hann window
  cols = floor((length(y)-N)/H) + 1                  					#no. of frames of stft
  stft = matrix(0,N,cols)                            					#empty matrix
  for (t in 1:cols) {
    range = (1+(t-1)*H): ((t-1)*H + N)  #t-th frame
    chunk  = y[range]
    stft[,t] = fft(chunk*win)
  } 
  stft
}

#recovering audio from the stft Y
istft = function(Y,H,N) {
  v = seq(from=0,by=2*pi/N,length=N)     
  win = (1 + cos(v-pi))/2
  y = rep(0,N + H*ncol(Y))
  for (t in 1:ncol(Y)) {
    chunk  = fft(Y[,t],inverse=T)/N
    range = (1+(t-1)*H): ((t-1)*H + N)                					#the audio indices for the t-th frame
    y[range]  = y[range]  + win*Re(chunk)             					#add the windowed ifft
  }
  y
}
		
N = 1024                                                				#FFT len
H = N/8                                                 				#hop size

bits = 16
w = readWave("glunker_stew.wav")                       				 	#i/p
sr = w@samp.rate                                        				#extract sampling rate from i/p
y = w@left                                              				#work with left channel
Y = stft(y,H,N)

Y = matrix(complex(modulus = Mod(Y), argument = rep(0,length(Y))),nrow(Y),ncol(Y))	#phase values are set to 0 to robotize
ybar = istft(Y,H,N)
ybar = ybar/max(ybar)                                   				#normalize
u = Wave(round((2^14)*ybar), samp.rate = sr, bit=bits)  				#make wave struct
play(u)                 

