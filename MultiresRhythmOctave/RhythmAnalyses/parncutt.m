## -*- Octave -*-
## Analyse all of Parncutt's rhythms
examples = ["swing"; "skip"; "waltz"; "cross"; "march"];
for i = 1:rows(examples)
  filename = ["~/AnalysedRhythms/parncutt-", examples(i,:)];
  [mag, phase, pc] = multiresFile([examples(i,:) "\n"], filename);
endfor

