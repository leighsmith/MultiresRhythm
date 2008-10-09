### -*- Octave -*-
### Bulk analysis of many data files

normalised_pc = zeros(1, 34);
for file = 0:34
    filename = sprintf("~/Research/Data/NewAnalysedRhythms/shmulevitch-pattern-%d\.mat", file);
    [mag, phase, pc, npc] = \
	complexityFile(sprintf("Shmulevitch %d", file), filename);
    normalised_pc(file + 1) = npc;
end

plot(normalised_pc);
