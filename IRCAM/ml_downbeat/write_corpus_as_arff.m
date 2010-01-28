function write_corpus_as_arff( corpus_patterns, filename )
%write_corpus_as_arff Writes out the corpus patterns as an ARFF file for Weka.
% $Id$
    arff = fopen(filename, 'w');

    if (arff ~= -1)
        fprintf(arff, ['%% Classification of anacrusis, given rhythm patterns.\n' ...
                       '@RELATION "Downbeat from rhythm pattern"\n' ...
                       '@ATTRIBUTE filename string\n']);
                
        % TODO These shouldn't be hardwired lengths.
        write_vector_description(arff, 'PeriodicityPatterns', 1:18);
        write_vector_description(arff, 'MetricalProfileBass', 1:16);
        write_vector_description(arff, 'MetricalProfileTreble', 1:16);
        write_vector_description(arff, 'HyperMetricalProfile', 1:64);
        fprintf(arff, '@ATTRIBUTE Tempo numeric\n');
        fprintf(arff, '@ATTRIBUTE Anacrusis {0,1,2,3}\n\n');

        fprintf(arff, '@DATA\n');
        for patternIndex = 1 : length(corpus_patterns)
            fprintf(arff, '"%s",', corpus_patterns{patternIndex}.name);
            write_vector(arff, corpus_patterns{patternIndex}.periodicities);
            write_vector(arff, reducedMetricalProfile(corpus_patterns{patternIndex}));
            write_vector(arff, hypermetricalProfile(corpus_patterns{patternIndex}));
            fprintf(arff, '%5.3f,', corpus_patterns{patternIndex}.tempo);
            fprintf(arff, '%d\n', corpus_patterns{patternIndex}.anacrusis);
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
    fprintf(filehandle, '%5.3f,', vector);
end
    