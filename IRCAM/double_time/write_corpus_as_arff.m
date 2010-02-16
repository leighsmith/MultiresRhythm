function write_corpus_as_arff( title, corpus_patterns, filename, double_time_probs, ground_truth )
%write_corpus_as_arff Writes out the corpus patterns as an ARFF file for Weka.
% $Id: write_corpus_as_arff.m 5598 2010-01-29 19:24:33Z leighsmi $
    arff = fopen(filename, 'w');

    if (arff ~= -1)
        fprintf(arff, ['%% Classification of double time, given rhythm patterns.\n' ...
                       '@RELATION "%s"\n' ...
                       '@ATTRIBUTE filename string\n'], title);
                
        % TODO These shouldn't be hardwired lengths.
        write_vector_description(arff, 'PeriodicityPatterns', 1:18);
        write_vector_description(arff, 'MetricalProfileBass', 1:16);
        write_vector_description(arff, 'MetricalProfileTreble', 1:16);
        fprintf(arff, '@ATTRIBUTE DoubledTimeQuaverAlternation numeric\n');
        fprintf(arff, '@ATTRIBUTE HalfTimeQuaverAlternation numeric\n');
        fprintf(arff, '@ATTRIBUTE CounterPhaseQuaverAlternation numeric\n');
        fprintf(arff, '@ATTRIBUTE DoubleTime {0,1}\n\n');

        fprintf(arff, '@DATA\n');
        for patternIndex = 1 : length(corpus_patterns)
            fprintf(arff, '"%s",', corpus_patterns{patternIndex}.name);
            write_vector(arff, corpus_patterns{patternIndex}.periodicities);
            write_vector(arff, reducedMetricalProfile(corpus_patterns{patternIndex}));
            write_vector(arff, double_time_probs(patternIndex,:));
            fprintf(arff, '%d\n', ground_truth(patternIndex));
        end

        fclose(arff);
    end
end


function write_vector_description(filehandle, name, vectorElements)
    for elementIndex = 1 : length(vectorElements)
        fprintf(filehandle, '@ATTRIBUTE %s_%02d numeric\n', name, vectorElements(elementIndex));
    end
end

function write_vector(filehandle, vector)
    if(~isempty(vector))
        fprintf(filehandle, '%5.3f,', vector);
    end
end
    