% cd to correct directory
addpath('../Est_Files')
clear all

nbn =40;

bootflag = 1;
save bootflag bootflag nbn

for bb = 1:nbn

    bb

uhflag = 1;
save uhflag uhflag
% Read in the data
datareadstm
% Prep data for combined household location and mobility decision estimation
S12prep
% Estimate combined household location and mobility decisions
S12est
% Prep data for transition probability estimation
S3prep
% Estimate transition probabilities
S3est
% Prep data for utility decomposition 
S4prep
% Estimate utility decomposition
S4est
%S4est_liqcon %uncomment to run  final regressions with liquidity constraints

% get evidence of forward-looking behavior
dynamicevidence_shares

uhflag = 0;
save uhflag uhflag
S12est
S3prep
S3est
S4prep
S4est


end
