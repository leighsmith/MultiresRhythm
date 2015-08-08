load ~/AnalysedRhythms/greensleeves_performed_preserve.text
size(signal)
locs = find(signal);
times = locs ./ 200
velocities = signal(locs)
save_cm("greensleeves_performed_recovered.cm", times, velocities, "rhythm-onset")
