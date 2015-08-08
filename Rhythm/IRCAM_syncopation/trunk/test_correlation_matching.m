function test_correlation_matching( choice )
%test_correlation_matching Summary of this function goes here
%   Detailed explanation goes here

if(nargin < 1)
    choice = 1;
end

plot_q_t = false;

target_onsets = [100 500 700 1100 1300 1500 1900];
target_impulses = zeros(1, max(target_onsets) + 100);
target_impulses(target_onsets) = 1;
target = gaussianRhythm(target_impulses, 100);

if(plot_q_t)
    figure();
    plot(target);
    title('Target');
end
    
q1_onsets = [100 300 700];
q1_impulses = zeros(1, max(q1_onsets) + 100);
q1_impulses(q1_onsets) = 1;
q1 = gaussianRhythm(q1_impulses, 100);

if(plot_q_t)
    figure();
    plot(q1)
    title('Query 1');
end

q2_onsets = [100 500 700];
q2_impulses = zeros(1, max(q2_onsets) + 100);
q2_impulses(q2_onsets) = 1;
q2 = gaussianRhythm(q2_impulses, 100);

if(plot_q_t)
    figure();
    plot(q2)
    title('Query 2');
end

[bivalent_aligns, values] = correlation_alignments(q1, target)

[locs, values] = cross_correlation_match(q1, target)

% Produces values of [400, 1200, 1000, 800, 600]
figure();
plot_correlation_match(q1, target, locs(choice))
title(sprintf('Matching test 1 choice %d, shift by %d samples', choice, locs(choice)));

[bivalent_aligns, values] = correlation_alignments(q2, target)

[locs, values] = cross_correlation_match(q2, target)

figure();
plot_correlation_match(q2, target, locs(choice))
title(sprintf('Matching test 2 choice %d, shift by %d samples', choice, locs(choice)));


end

