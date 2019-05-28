live_loop :hmusic do
	sample :drum_bass_hard
	sample :drum_cymbal_closed
	sleep 0.53571427
	sample :drum_cymbal_closed
	sleep 0.53571427
	sample :drum_snare_hard, reverb: 1.0, amp: 1.0, echo
	sample :drum_cymbal_closed
	sleep 0.53571427
	sample :drum_cymbal_closed
	sleep 0.53571427
end