
#The echo comes D = .125 secs after the original sound, but decreases in amplitude by a factor of F = .8. 
#This echo is now part of the sound in the room, so D seconds later we will hear another version of the original echo, 
#decreased in amplitude by another factor of F. This process will continue indefinitely, 
#though the echos eventually become inaudible as their amplitudes decrease.

library(tuneR)
setWavPlayer('afplay')

w = readWave("bass_oboe.wav")                   #i/p
x = w@left                                      #working with left channel
start = 40000     
x = x[start:length(w)]
N = 1024
a = rep(0,N)                                    #AR filter
a[1] = 1
a[1001] = -.8
a = c(a,rep(0,length(x)-length(a)))             #0 padding
A = fft(a)
X = fft(x)
Y = X/A         
y = Re(fft(Y,inverse=TRUE))                     #echo
y = y/max(y)                                    #normalize
u = Wave(round(2^14 * y),samp.rate=8000,bit=16) #make wav struct
play(u)
