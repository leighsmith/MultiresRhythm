function save_rhythm_mix (filename_to_write, original_rhythm_file, clap_times_seconds, clap_sample, clap_amplitudes)
%save_rhythm_mix Mix the sound file original_rhythm_file with the claps marked by clap_sample_file at times clap_times_seconds.

    if(nargin < 5)
        clap_amplitudes = 1;
    end
    % load original file
    [original_rhythm_sound, original_sr, dynamic_resolution] = wavread(tilde_expand(original_rhythm_file));

    if (nargin < 4)
        clap_sample_file = tilde_expand('~/Research/Data/Handclap Examples/hihat_closed.wav');
        [clap_sample, clap_sr] = wavread(clap_sample_file);
    else
        % If we do specify a clap sample, assume it is at the same sample rate as the sound file.
        clap_sr = original_sr;
    end

    % Check the sample rate of the two sounds and resample the clap to
    % match.
    if(clap_sr ~= original_sr)
        fprintf('Resampling the clap sound to %f Hz\n', original_sr);
        clap_sample = resample(clap_sample, clap_sr, original_sr);
    end
    
    % create a sound vector with our clap_sample
    clapping_sound = sample_at_times(length(original_rhythm_sound), clap_sample, original_sr, clap_times_seconds, clap_amplitudes);

    if (size(clapping_sound, 2) < size(original_rhythm_sound, 2))
        multiple_channels = zeros(size(clapping_sound, 1), size(original_rhythm_sound, 2));
        for channelIndex = 1 : size(original_rhythm_sound, 2)
            multiple_channels(:,channelIndex) = clapping_sound;
        end
        clapping_sound = multiple_channels;
    elseif (size(clapping_sound, 2) > size(original_rhythm_sound, 2))
        fprintf('Need to reduce the clapping_sound channels to match the original_rhythm_sound\n');
    end
    clapping_mix = original_rhythm_sound + clapping_sound;
    clear('original_rhythm_sound', 'clapping_sound', 'multiple_channels');
    wavwrite(bipolar_normalise(clapping_mix), original_sr, dynamic_resolution, tilde_expand(filename_to_write));
end

function [full_duration] = sample_at_times(length_of_sound, sample_sound, sample_rate, times_in_seconds, amplitudes)
%sample_at_times Returns a sound with sample_sound placed beginning at each time specified in seconds. Uses the sample rate of the sample_sound.

    sample_length = length(sample_sound);
    attack_times_in_frames = floor(times_in_seconds .* sample_rate);
    full_duration = zeros(length_of_sound, 1);
    if(isscalar(amplitudes))
        amplitudes = repmat(amplitudes, [length(times_in_seconds), 1]);
    end

    for attackIndex = 1 : length(attack_times_in_frames)
        attack_time = attack_times_in_frames(attackIndex) + 1; % +1 for base 1 indexing.
        amplitude_scaler = amplitudes(attackIndex);
        region_to_copy = attack_time : min(length_of_sound - 1, attack_time + sample_length - 1);

        % fprintf('region to copy %d\n' region_to_copy)
	full_duration(region_to_copy) = sample_sound .* amplitude_scaler;
    end
end

function [normalised_vector] = bipolar_normalise (sound_vector)
% Normalise over the entire sound to a bipolar range (between -1.0 and 1.0)
    maximum_displacement = max(max(max(sound_vector)), abs(min(min(sound_vector))));
    % (maximum_displacement +  (1 / (2^(16-1)))
    normalised_vector = sound_vector ./ (maximum_displacement + eps); % to stop wavwrite clipping
end
