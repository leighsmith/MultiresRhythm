all_ridges = load("~/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.ridges");
# all_ridges.oneTrueRidge
# all_tactus = load("~/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.tactus");
all_tactus = load("~/Research/Data/NewAnalysedRhythms/greensleeves-example-ridge.tactus");
startTime = 83
plotRidgesAndTactus("tactus", all_ridges.oneTrueRidge, all_tactus.tactus.' .+ 1, startTime + 1);

