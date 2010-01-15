function write_corpus_patterns_as_arff( corpus_patterns, filename )
%write_corpus_as_arff Writes out the corpus patterns as an ARFF file for Weka.
% $Id$
    arff = fopen(filename, 'w');

    if (arff ~= -1)
        fprintf(arff, ['%% Classification of ballroom dance styles given rhythm patterns.\n' ...
                       '@RELATION "Ballroom Dance Styles"\n' ...
                       '@ATTRIBUTE filename string\n']);

        syncopated_tatum_indices = syncopatedTatums(corpus_patterns{1});
        write_vector_description(arff, 'SyncopationProfileBass', syncopated_tatum_indices);
        write_vector_description(arff, 'SyncopationProfileTreble', syncopated_tatum_indices);
        
        % TODO These shouldn't be hardwired lengths.
        write_vector_description(arff, 'PeriodicityPatterns', 1:18);
        write_vector_description(arff, 'MetricalProfileBass', 1:16);
        write_vector_description(arff, 'MetricalProfileTreble', 1:16);
        write_vector_description(arff, 'HyperMetricalProfile', 1:64);
        fprintf(arff, '@ATTRIBUTE Tempo numeric\n');
        fprintf(arff, '@ATTRIBUTE dance_style {ChaChaCha,Jive,Quickstep,Rumba,Samba,Tango,VienneseWaltz,Waltz}\n\n');

        fprintf(arff, '@DATA\n');
        for patternIndex = 1 : length(corpus_patterns)
            fprintf(arff, '"%s",', corpus_patterns{patternIndex}.name);
            write_vector(arff, reducedFeatureVector(corpus_patterns{patternIndex}));
            fprintf(arff, '%s', corpus_patterns{patternIndex}.style);
            fprintf(arff, '\n');
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
    