% -*- Octave -*-
% This mimics the xmlread() in Matlab V7.7.0. It returns a DOM document.
% For Java operations:
% xmlread('/Users/leigh/Research/Data/IRCAM-Beat/RWC/Analysis/RM-C001.wav.markers.xml')
function document = xmlread(filename)
% TODO should check if we have previously initialised java to save reinitialising each time.
    java_init();
    java_classpaths = strsplit(getenv('CLASSPATH'), ':');

    for i = 1 : length(java_classpaths)
        javaaddpath(java_classpaths(i));
    end
    % current_class_path = javaclasspath()

    domParser = java_new('org.apache.xerces.parsers.DOMParser');

    parse_success = domParser.parse(filename);
    document = domParser.getDocument();
end
