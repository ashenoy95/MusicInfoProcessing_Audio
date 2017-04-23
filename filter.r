
library(tuneR)
setWavPlayer('afplay')

w = readWave("bass_oboe.wav")                   #i/p 
bd = w@bit                                      #extract bit depth from i/p
sr = w@samp.rate                                #extract sample rate from i/p
y = w@left                                      #working only on left channel
y = y[1:160000]/max(y)                          #20 secs

N = 8192                                        #FFT len
range = 1000:3000                               #a filter that “passes” only frequencies between 1000 and 3000 Hz
retain = 40
A = rep(0,N)                                    #filter response
A[range] = 1
A[N-range+2] = 1                                #for symmetry
a = Re(fft(A,inverse=TRUE))                     #ideal filter

AA = fft(a)
#plot(Mod(AA[1:(N/2)]))

a = c(a[(N-retain+2):N] , a[1:retain])/N        #take the center 2*retain coeffs of filter
print(length(a))
AA = fft(c(a,rep(0,N-length(a))))	              #0 padding
#plot(Mod(AA[1:(N/2)]),type='l')	

z = filter(y,a,method="conv",circular=T)
z = as.vector(z)
u = Wave(round(2^(bd-2)*z),samp.rate=sr,bit=bd) #make wave struct
play(u)
