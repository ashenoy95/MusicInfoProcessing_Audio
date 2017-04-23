
library(tuneR)	
setWavPlayer('afplay')	

stft = function(y,H,N) {
  v = seq(from=0,by=2*pi/N,length=N)     
  win = (1 + cos(v-pi))/2
  cols = floor((length(y)-N)/H) + 1
  stft = matrix(0,N,cols)
  for (t in 1:cols) {
    range = (1+(t-1)*H): ((t-1)*H + N)
    chunk  = y[range]
    stft[,t] = fft(chunk*win)
  } 
  ph = Arg(stft)
  for (k in 1:nrow(ph)) {
    ph[k,] = c(ph[k,1],diff(ph[k,]))
  }
  stft = matrix(complex(modulus = Mod(stft), argument = ph),nrow(stft),ncol(stft)) 
  stft
}

istft = function(Y,H,N) {
  ph = Arg(Y)
  for (k in 1:nrow(Y)) {
    ph[k,] = cumsum(ph[k,])
  }
  Y = matrix(complex(modulus = Mod(Y), argument = ph),nrow(Y),ncol(Y)) 
  v = seq(from=0,by=2*pi/N,length=N)     
  win = (1 + cos(v-pi))/2
  y = rep(0,N + H*ncol(Y))
  for (t in 1:ncol(Y)) {
    chunk  = fft(Y[,t],inverse=T)/N
    range = (1+(t-1)*H): ((t-1)*H + N)
    y[range]  = y[range]  + win*Re(chunk)
  }
  y
}
	
N = 1024                                        #fft len
H = N/4                                         #hop size
bits = 16 
w = readWave("glunker_stew.wav")                #i/p
sr = w@samp.rate                                #extract sampling rate from i/p
y = w@left                                      #working with left channel
Y = stft(y,H,N)
      
print("Using the top 1%")
percent = 1
mod = Mod(Y);
phase = Arg(Y);
x = sort(mod,decreasing=T);
cutoff = x[(percent/100) * length(x)]
mod[mod<cutoff] = 0;                            #using STFT coefficients with highest 1% moduli
phase[mod<cutoff] = 0;
Y = matrix(complex(modulus = mod, argument = phase),nrow(Y),ncol(Y)) 
ybar = istft(Y,H,N) 
ybar = (2^14)*ybar/max(ybar)  
u = Wave(round(ybar), samp.rate = sr, bit=bits) #create wav struct
play(u)
