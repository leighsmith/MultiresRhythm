% -*- Octave -*-

classdef Evaluation
    %Evaluation Determines the method of beat evalution
    % The Evaluation class contains the precision window and that relative
    % filtering is to be performed, and the annotation filter.
    %
    % $Id: Evaluation.m 993 2009-07-10 15:42:43Z lsmith $
    %
    % Copyright (c) 2009 IRCAM, All Rights Reserved.
    % Permission is only granted to use this code for Quaero evaluation
    % purposes.
    
    properties
        precisionWindow = 0.0;
        relativePrecision = 1; % Boolean.
        annotationFilter = '';
    end
    
    methods
        function evaluation = Evaluation()
            evaluation.precisionWindow = 0.0;
            evaluation.relativePrecision = 0;
            evaluation.annotationFilter = '';
        end
            
        function yesOrNo = isAnnotationFiltered(evaluation)
            yesOrNo = isa(evaluation.annotationFilter, 'function_handle');
        end
        
        function yesOrNo = isRelativePrecision(evaluation)
            yesOrNo = evaluation.relativePrecision;
        end
        
        function filterName = annotationFilterName(evaluation)
            if (evaluation.isAnnotationFiltered())
                filterName = func2str(evaluation.annotationFilter);
            else
                filterName = '';
            end
        end
    end
    
end

