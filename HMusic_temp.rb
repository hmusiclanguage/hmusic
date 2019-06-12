live_loop :hmusic do
	sample :kick
	sample :hihat, attack: 0.1, sustain: 0.3, release: 0.1
	sample :guitar
	sleep 0.6
	sample :hihat, attack: 0.1, sustain: 0.3, release: 0.1
	sleep 0.6
	sample :snare, reverb: 0.5
	sample :hihat, attack: 0.1, sustain: 0.3, release: 0.1
	sleep 0.6
	sample :hihat, attack: 0.1, sustain: 0.3, release: 0.1
	sleep 0.6
end