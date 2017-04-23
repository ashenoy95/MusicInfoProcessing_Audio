library(tuneR)
setWavPlayer('afplay')

sr = 8000							#sampling rate
bd = 16								#bit depth
notes = 10
harm = 10							#no. of harmonics to add
f = 440								#freq
secs = 0
z = rep(0,0)
z2 = rep(0,0)
et = c("A","Bb","B","C","C#","D","Eb","E","F","F#","G","G#")	#equal temperament
et_len = length(et)
amazing_grace = c("G","C","E","D","C","E","D","C","A","G")	#notes in Amazing Grace
len = c(1,2,1/2,1/6,1/3,2,1,2,1,2)				#duration of each note in secs

for (i in 1:notes) {
	for (j in 1:et_len) {
		if(amazing_grace[i]==et[j])
			half_step = j-1
	}
	fdash = f*2^(half_step/12)
	if (amazing_grace[i]=="G")
		fdash = fdash/2
		g = fdash
	if (amazing_grace[i]=="C")
		c = fdash
	t = seq(0, len[i], 1/sr)
	s = rep(0, length(t))
	for (k in 1:harm) {					#add 10 harmonics as grace notes
		a = runif(1)					#randomly generated amplitude
		phi = runif(1)					#randomly generate phase
		s = s + a*sin(2*pi*k*fdash*t + phi)		#create sine wave having different timbre to simulate the feel of bagpipes
	}
	z = c(z,s)
	secs = secs + len[i]
}

t = seq(0,secs,1/sr)

#long held notes of C & G to simulate bagpipes
x = sin(2*pi*c/2*t)								
y = sin(2*pi*g/2*t)

w = Wave(round(2^(bd-5)*(x+y+z)), samp.rate=sr, bit=bd)		#create wav struct
play(w)
