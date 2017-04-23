
library(tuneR)
setWavPlayer('afplay')

sr = 8000                       #sampling rate
bd = 16                         #bit depth
f = 440                         #frequency
vr = 1/5                        #vibrato rate
t = seq(0,5,by=1/8000)          #time points at which sound is sampled
a = sin(2*pi*vr*t)    
s = a*sin(2*pi*f*t) 
v = round((2^(bd-2))*s)
w = Wave(v,samp.rate=sr,bit=bd) #creates wav struct
play(w)
