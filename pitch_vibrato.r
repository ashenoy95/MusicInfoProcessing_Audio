
library(tuneR)
setWavPlayer('afplay')

f = 440                                   #frequency
bd = 16                                   #bit depth
sr = 8000                                 #sampling rate
t = seq(0,3,1/sr)                         #points at which the sound is sampled
vr = 5                                    #vibrato rate
dv = 5                                    #vibrato width
s = sin(2*pi*f*t - dv/vr*cos(2*pi*vr*t))  #sin wave
v = round((2^(bd-2))*s)
w = Wave(v,samp.rate=sr,bit=bd)           #wav struct
play(w)
