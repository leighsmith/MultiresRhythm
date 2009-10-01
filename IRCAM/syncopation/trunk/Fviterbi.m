% function [path_v, loglikelihood] = Fviterbi(probinit_v, probobs_m, probtrans_m)
%
% DESCRIPTION:
% ============
% Viterbi decoding algorithm based on 
% observation probability (probobs_m), transition probability (probtrans_m) and initial probability (probinit_v)
%
% return the path of states along time and the corresponding likelihood
%
% INPUTS:
% =======
% - probinit_v  (nb_state, 1)		: initial probability for all the 'nb_state' states
% - probobs_m	(nb_state, nb_time)	: observation probability of each of the 'nb_state' states at each of the 'nb_time' times
% - probtrans_m	(nb_state, nb_state, nb_time)	: transition probability of each of the 'nb_state' state to each of the 'nb_state' state
%
% OUTPUTS:
% ========
% - path_v	(nb_time)		: best path through the states, gives the state number for each time
% - loglikelihood			: log-likelihood of the best path
%
% peeters@ircam.fr 2005/11
%
% Modified by Leigh.Smith@ircam.fr for transition probabilities that vary
% across time.

function [path_v, loglikelihood] = Fviterbi(probinit_v, probobs_m, probtrans_m)

S = size(probobs_m, 1);
T = size(probobs_m, 2);

if length(probinit_v)~=S, error('dimensions do not match');, end
if size(probtrans_m,1)~=S, error('dimensions do not match');, end
if size(probtrans_m,2)~=S, error('dimensions do not match');, end


% ===============================
% === the probability computation is done in the log domain
logprobtrans_m		= log(probtrans_m+eps);
logprobobs_m		= log(probobs_m+eps);

% ===================================
% NOTATIONS:
% z_m (nb_state, nb_time)	: z_m(s,t) gives the best previous to state s at time t 
% B_m (nb_state, nb_time)	: B_m(s,t) gives the likelihood of state s at time t
% ===================================
z_m	= zeros(S,T);
B_m	= zeros(S,T);


% ===================================
% === 1) INITIALISATION
t = 1;
for s=1:S
    B_m(s, t)   = log(probinit_v(s)) + log(probobs_m(s, t));
    z_m(s, t)   = 0;
end

    

% === 2) FORWARD
if T>1
    for t=2:T % === across time
        
        for s=1:S % === across states

            % === transition probabilities to the current state
            logprobtrans_v 	= logprobtrans_m(:,s,t-1);
            % === maximum transition probabilities to the current state
            [max_value, max_pos]= max( B_m(:, t-1) + logprobtrans_v );
            z_m(s,t) 		= max_pos;
            % === cumulative probability
            B_m(s,t) 		= B_m(max_pos,t-1) + logprobtrans_v(max_pos) + logprobobs_m(s, t);
            
        end, % === for s
        
    end, % === for t
else
    t = 1;
end

% ===================================
% === 3) MAXIMUM ENDING PROBABILITY
[value_v, pos_v] 	= sort(B_m(:,T));
% === value_v	likelihood of the most likely probabilities ('ties' -> if one wants to backward several tracks)
% === pos_v	positions of the most likely probabilities ('ties' -> if one wants to backward several tracks)
pos_v 			= pos_v(end:-1:1);
value_v 		= value_v(end:-1:1);


% === B_m (S,T)
% === z_m (S,T)

% ===================================
% === 4) BACKWARD
for p=1:1,	% === if one wants to backward several tracks
    loglikelihood 	= value_v(p);

    stock		= pos_v(p);
    path_v(T)		= stock;
    
    for t = T:-1:2, % === backward in time
        stock		= z_m(stock, t);
        path_v(t-1)	= stock;
    end
end


