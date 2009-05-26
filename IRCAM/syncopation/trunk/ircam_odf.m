function [ odf ] = ircam_odf( filepath )
% IRCAMbeat ODF has 100mS silence appended onto the audio file before calculating the
% ODF, which is then compensated for when calculating the markers. We
% remove it.
sample_rate = 172.27; % (Hz) Hardwired since there is no data stored.
leading_silence_seconds = 0.1;

raw_odf = load(filepath);

odf = raw_odf(round(leading_silence_seconds * sample_rate) + 1 : end);

end