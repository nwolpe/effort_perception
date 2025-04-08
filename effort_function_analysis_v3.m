%% quality checks and exclusion
clear
addpath ~/Documents/MatlabScipts/
data = readtable('~/Documents/effort_function/features_data_29.01.25_2.csv');

data_mvc = readtable('~/Documents/effort_function/mvc_ratios.csv');

% Create a logical index for values below the 75th percentile
q75 = prctile(data_mvc.mvc_ratio_recorded, 75);
people2keep = data_mvc.Participant(data_mvc.mvc_ratio_recorded < q75);
% people2keep = data_mvc.Participant(data_mvc.mvc_ratio_recorded<=1.1 & data_mvc.mvc_ratio_recorded>= 0.9);

data = data(ismember(data.Participant, people2keep),:);

subID = unique(data.Participant);

% exclude people with low ranked correlation between force and rating
corr_coeff = nan(length(subID),1);
success_rate = nan(length(subID),1);
mvc_ratio= nan(length(subID),1);

for iSub = 1:length(subID)
    % Get the data for the current participant
    ind2include = strncmp(data.Participant, subID(iSub), 4);
    
    corr_coeff(iSub) = corr(data.Target_Force(ind2include), data.Subjective_Effort(ind2include), 'type', 'Spearman');
    success_rate(iSub) = sum(strncmp(data.Success(ind2include),'Yes',3)) / length(data.Success(ind2include));
    mvc_ratio(iSub) = unique(data.mvc_ratio(ind2include));
end

sub2exclude = isoutlier(corr_coeff, "mean", "ThresholdFactor", 2.5) | isoutlier(success_rate,"mean","ThresholdFactor", 2.5) | success_rate==1;

ind2exclude = ismember(data.Participant, subID(sub2exclude));
data(ind2exclude,:) = [];

% save as csv
% writetable(data, '~/Documents/effort_function/effort_function_data_filtered.csv')
clearvars subID

[subID, ~, numeric_ids] = unique(data.Participant);
data.Participant_num = numeric_ids;



%% compare different functions

% Prepare data
X = data.Target_Force; % Predictor (n-by-1)
y = data.Subjective_Effort; % Response (n-by-1)
group = data.Participant; % Grouping variable (n-by-1)
handUsed = double(strncmp(data.Hand, 'Dominant', 8)); % Binary hand variable
participants = unique(group);
numParticipants = numel(participants);

% Combine predictors into a matrix (force and hand)
% This is needed because nlmefit expects multi-column predictors as a matrix
X_with_hand = [X, handUsed];

% Define the model functions with hand as a fixed effect:
% PHI(1) = fixed intercept, PHI(2) = fixed slope for force, PHI(3) = fixed effect for hand
% VFUN(1) = random intercept, VFUN(2) = random slope for force

modelFun_lin = @(PHI, XFUN, VFUN) (PHI(1) + VFUN(1)) + (PHI(2) + VFUN(2)).*XFUN(:,1) + PHI(3).*XFUN(:,2);

modelFun_quad = @(PHI, XFUN, VFUN) (PHI(1) + VFUN(1)) + (PHI(2) + VFUN(2)).*XFUN(:,1) + PHI(3).*XFUN(:,1).^2 + PHI(4).*XFUN(:,2);

modelFun_hyperbolic = @(PHI, XFUN, VFUN) ((PHI(1) + VFUN(1)) ./ (1 + (PHI(2) + VFUN(2)).*XFUN(:,1))) + PHI(3).*XFUN(:,2);

modelFun_exp = @(PHI, XFUN, VFUN) (PHI(1) + VFUN(1)) .* exp((PHI(2) + VFUN(2)).*XFUN(:,1)) + PHI(3).*XFUN(:,2);

% Initial guesses 
beta0_lin = [1, 1, 0]; % fixed effects [intercept, slope_force, hand_effect]
beta0_quad = [1, 1, 0.1, 0]; % [intercept, slope_force, quadratic_term, hand_effect]
beta0_hyperbolic = [1, 0.1, 0]; % [intercept, rate, hand_effect]
beta0_exp = [1, 0.1, 0]; % [amplitude, rate, hand_effect]

% V matrix stays the same since random effects are still just for intercept and force slope
V = zeros(numParticipants, 2);

% Fit the models with hand as a fixed effect
[~, ~, stats_lin, ~] = nlmefit(X_with_hand, y, group, V, modelFun_lin, beta0_lin, 'REParamsSelect', [1, 3]);
[~, ~, stats_lin_full, ~] = nlmefit(X_with_hand, y, group, V, modelFun_lin, beta0_lin, 'REParamsSelect', 1:3);
stats_lin.bic
stats_lin_full.bic

[~, ~, stats_quad, ~] = nlmefit(X_with_hand, y, group, V, modelFun_quad, beta0_quad, 'REParamsSelect', [1, 3, 4]);
[~, ~, stats_quad_full, ~] = nlmefit(X_with_hand, y, group, V, modelFun_quad, beta0_quad, 'REParamsSelect', 1:4);
stats_quad.bic
stats_quad_full.bic

[~, ~, stats_hyperbolic, ~] = nlmefit(X_with_hand, y, group, V, modelFun_hyperbolic, beta0_hyperbolic, 'REParamsSelect', [1, 3]);
[~, ~, stats_hyperbolic_full, ~] = nlmefit(X_with_hand, y, group, V, modelFun_hyperbolic, beta0_hyperbolic, 'REParamsSelect', 1:3);
stats_hyperbolic.bic
stats_hyperbolic_full.bic

[~, ~, stats_exp, ~] = nlmefit(X_with_hand, y, group, V, modelFun_exp, beta0_exp, 'REParamsSelect', [1, 3]);
[~, ~, stats_exp_full, ~] = nlmefit(X_with_hand, y, group, V, modelFun_exp, beta0_exp, 'REParamsSelect', 1:3);
stats_exp.bic
stats_exp_full.bic


%% compute betas for effort and reward sensitivity
clearvars sub2exclude success_rate
rng(1)

AES = nan(length(subID),1);
PHQ = nan(length(subID),1);

data.feedback = strncmp(data.Success, 'Yes',3);
b = nan(length(subID),4);

for iSub = 1:length(subID)
    % Get the data for the current participant
    ind2include = strncmp(data.Participant, subID(iSub), 4);
    
    AES(iSub) = nanmean(data.AES(ind2include));
    PHQ(iSub) = nanmean(data.PHQ9(ind2include));
    
    effort_rating = data.Subjective_Effort(ind2include);
    
%     x = [zscore(data.Target_Force(ind2include).^2), data.feedback(ind2include)==1];
    
    x = [zscore(data.Target_Force(ind2include).^2), data.feedback(ind2include)==1, ...
        strncmp(data.Hand(ind2include), 'Dominant', 8)];

%     x(effort_rating>0.9,:)=[];
%     effort_rating(effort_rating>0.9) = [];
    y = zscore(effort_rating);
    
    
    b(iSub, :) = custom_robustfit(x, y);
    
    % Predictions
    intercept = ones(length(y),1);
    y_pred = sum([intercept,x] .* b(iSub,:),2);
    
    % Compute residuals
    SS_res = sum((y - y_pred).^2);   % Residual sum of squares
    SS_tot = sum((y - mean(y)).^2);  % Total sum of squares
    
    % Compute R²
    R2_exp(iSub) = 1 - (SS_res / SS_tot);

 
end

% remove participants with 0% success! and 100% success
% sub2exclude = success_rate==1 | success_rate==0;
corrtype = 'Pearson';
[r p1] = corr(AES, b, 'rows', 'Complete', 'type', corrtype)
[r p2] = corr(PHQ(PHQ<15), b(PHQ<15,:), 'rows', 'Complete', 'type', corrtype)


% Combine p-values into a single vector
all_p = [p1(2:3), p2(2:3)];

% FDR correction
adj_p = mafdr(all_p, 'BHFDR', true)

% writetable(table(PHQ, b), '~/Documents/effort_function/forFigure.csv')

