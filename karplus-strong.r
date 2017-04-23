
library(tuneR)
setWavPlayer('afplay')

sr = 8000                                           #sampling rate
f = 440                                             #freq
L = round(sr/f)
L = 70
n = 8192
x = runif(n)
y = rep(0,n)
x = c(x,y)

for (i in (n+1):(n+n)) {
	x[i] = (x[i-L] + x[i-L-1])/2
}

y = x[(n+1):(n+n)]
y = rep(y,5)                                       #play the note 5 times
u = Wave(round(2^(11)*y), samp.rate = sr, bit=16)  #make wav struct
play(u)
