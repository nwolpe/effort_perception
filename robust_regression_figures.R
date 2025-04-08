# script to plot correlation figures

data<-read.csv('~/Documents/effort_function/forFigure.csv')


############# raw plot for rating vs. effort #############

library(dplyr)
library(ggplot2)

# Define the bins
num_bins <- 10
data_binned <- data %>%
  mutate(Force_bin = cut(Target_Force, breaks = num_bins))
participant_means <- data_binned %>%
  group_by(Participant, Force_bin) %>%
  summarise(
    Mean_Force = 100*mean(Target_Force, na.rm = TRUE),
    Mean_Effort = 100*mean(Subjective_Effort, na.rm = TRUE),
    .groups = "drop"
  )
bin_summary <- participant_means %>%
  group_by(Force_bin) %>%
  summarise(
    Grand_Mean_Force = mean(Mean_Force, na.rm = TRUE),
    Grand_Mean_Effort = mean(Mean_Effort, na.rm = TRUE),
    SE = sd(Mean_Effort, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

fig1b<-ggplot(bin_summary, aes(x = Grand_Mean_Force, y = Grand_Mean_Effort)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = Grand_Mean_Effort - SE, ymax = Grand_Mean_Effort + SE),
                width = 0.01, linewidth = 1)+
  theme_minimal() +
  xlim(0,130)+ ylim(0,100)+
  labs(
    x = "Target force %MVC (binned)",
    y = "Effort rating")

print(fig1b)
ggsave(filename = "~/Documents/effort_function/Fig1b.jpg",
       plot = fig1b, dpi = 300, width = 6, height = 4)

####### effort and success sensitivity vs. PHQ and AES #########

library(ggplot2)
library(cowplot)

# Panel A: PHQ vs b_2
plot_a <- ggplot(data, aes(x = b_2, y = PHQ)) +
  geom_point(size = 2, color = 'steelblue', alpha = 0.8) +
  geom_smooth(method = 'lm', color = 'black', se = TRUE) +
  labs(x = "Force sensitivity", y = "PHQ9") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Panel B: PHQ vs b_3
plot_b <- ggplot(data, aes(x = b_3, y = PHQ)) +
  geom_point(size = 2, color = 'firebrick', alpha = 0.8) +
  geom_smooth(method = 'lm', color = 'black', se = TRUE) +
  labs(x = "Success sensitivity", y = "PHQ9") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Combine into one figure with cowplot
combined_plot <- plot_grid(plot_a, plot_b, labels = c("A", "B"), align = 'hv')

# Display the combined plot
print(combined_plot)

ggsave(filename = "~/Documents/effort_function/Fig2.jpg",
       plot = combined_plot, dpi = 300, width = 6.5, height = 4.5)



######### Exploring other features ########


data <- read.csv('~/Documents/effort_function/effort_function_data_filtered.csv')

library(dplyr)
library(tidyr)
library(ggplot2)


# Recode Success as binary
data$Success_bin <- ifelse(data$Success == "Yes", 1, 0)

# Create squared force
data$Target_Force_sq <- data$Target_Force^2

# Variables of interest
vars <- c("Target_Force_sq", "Success_bin", "AUC_target", "Peak_target", "CoV", 
          "Sum_AUC", "Target_Force_t_1", "Subjective_Effort_t_1")

# Add squared version to vars list
# vars[vars == "Target_Force"] <- "Target_Force_sq"

# Remove rows with NA in relevant vars
data_clean <- data %>% drop_na(all_of(vars))

# Compute within-subject correlations
participants <- unique(data_clean$Participant)
cor_list <- list()

for (pid in participants) {
  df <- filter(data_clean, Participant == pid)
  if (nrow(df) < 5) next  # skip if too few trials
  
  # Subset relevant variables
  df_sub <- df %>% dplyr::select(all_of(vars))
  
  
  r <- cor(df_sub, use = "pairwise.complete.obs")
  
  cor_list[[pid]] <- r
}

# Average correlation matrices across participants
cor_array <- array(NA, dim = c(length(vars), length(vars), length(cor_list)))
for (i in seq_along(cor_list)) {
  cor_array[, , i] <- cor_list[[i]]
}
mean_cor <- apply(cor_array, c(1, 2), mean, na.rm = TRUE)

#  now plot
library(ggplot2)
library(dplyr)
library(tidyr)

# Recreate variable names (in case they got lost)
vars <- colnames(cor_list[[1]])

# Stack correlation matrices into an array
cor_array <- array(NA, dim = c(length(vars), length(vars), length(cor_list)))

for (i in seq_along(cor_list)) {
  cor_array[,,i] <- cor_list[[i]]
}

# Mean correlation matrix
mean_cor <- apply(cor_array, c(1, 2), mean, na.rm = TRUE)
colnames(mean_cor) <- rownames(mean_cor) <- vars

# Turn matrix into long format with row and column names
cor_df <- as.data.frame(as.table(mean_cor))
colnames(cor_df) <- c("Var1", "Var2", "Mean_r")

# Ensure factors follow correct variable order
cor_df <- cor_df %>%
  mutate(
    Var1 = factor(Var1, levels = vars),
    Var2 = factor(Var2, levels = vars)
  )

# New order and display names
new_labels <- c(
  "Target_Force_sq" = "Target force",
  "Success_bin" = "Success",
  "Subjective_Effort_t_1" = "Previous effort",
  "Target_Force_t_1" = "Previous force",
  "Sum_AUC" = "Cumulative force",
  "CoV" = "Force instability",
  "Peak_target" = "Prediction error",
  "AUC_target" = "Excess force"
)

# Define the desired plotting order
plot_order <- names(new_labels)  # you can rearrange this vector if needed


cor_df <- cor_df %>%
  mutate(
    Var1 = factor(Var1, levels = plot_order, labels = new_labels),
    Var2 = factor(Var2, levels = plot_order, labels = new_labels)
  ) %>%
  filter(as.integer(Var1) > as.integer(Var2))  # maintain lower triangle


fig3 <- ggplot(cor_df, aes(x = Var2, y = Var1, fill = Mean_r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", Mean_r)), size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_minimal() +
  coord_fixed() +
  labs(fill = expression("mean " * italic(r) * " value"))+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "~/Documents/effort_function/Fig3.jpg",
       plot = fig3, dpi = 300, width = 10, height = 6)

########### now run robust regression ########

library(MASS)     # for rlm (robust regression)
library(glmnet)   # for ridge regression
library(dplyr)    # for data wrangling

participants <- unique(data$Participant)
results <- list()

for (pid in participants) {
  # Subset data
  df <- filter(data, Participant == pid)
  
  if (nrow(df) < 10) next  # skip small samples
  
  # Add derived features
  df <- df %>%
    mutate(
      Target_Force_sq = Target_Force^2,
      Hand = factor(Hand),
      Success = factor(Success)
    )
  
  # Remove trials with missing lagged predictors
  df <- df %>% filter(!is.na(Target_Force_t_1), !is.na(Subjective_Effort_t_1))
  if (nrow(df) < 10) next
  
  # Z-score continuous predictors
  continuous_vars <- c("Target_Force_sq", "AUC_target", "Peak_target", 
                       "CoV", "Sum_AUC", "Target_Force_t_1", "Subjective_Effort_t_1")
  df[continuous_vars] <- scale(df[continuous_vars])
  
  # Create design matrix (without intercept)
  X <- model.matrix(~ Target_Force_sq + Hand + Success + AUC_target + Peak_target +
                      CoV + Sum_AUC + Target_Force_t_1 + Subjective_Effort_t_1, data = df)[, -1]
  y <- df$Subjective_Effort
  
  # Step 1: Fit robust regression and extract weights
  m_fit <- rlm(y ~ X, psi = psi.huber, maxit = 100)
  w <- m_fit$w  # robust weights
  
  # Step 2: Ridge regression using robust weights
  # Use CV to find best lambda
  cvfit <- cv.glmnet(X, y, alpha = 0, weights = w, standardize = FALSE)
  best_lambda <- cvfit$lambda.min
  
  # Extract coefficients (excluding intercept)
  coef_best <- as.vector(coef(cvfit, s = best_lambda))[-1]
  names(coef_best) <- colnames(X)
  
  results[[pid]] <- coef_best
}

# Combine into data frame
coef_df <- do.call(rbind, lapply(names(results), function(pid) {
  out <- results[[pid]]
  if (is.null(out)) return(NULL)
  data.frame(Participant = pid, t(out))
}))

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Prepare coef_long to match summary_df
coef_long <- coef_long %>%
  mutate(
    Predictor = ifelse(Predictor == "SuccessYes", "Success_bin", Predictor)
  ) %>%
  filter(Predictor != "HandNon_Dominant") %>%
  mutate(
    Predictor = factor(Predictor, levels = plot_order, labels = new_labels)
  )


summary_df <- coef_long %>%
  group_by(Predictor) %>%
  summarise(
    mean_beta = mean(Beta, na.rm = TRUE),
    se = sd(Beta, na.rm = TRUE) / sqrt(n()),
    lower = mean_beta - 1.96 * se,
    upper = mean_beta + 1.96 * se
  )



fig3b<-ggplot(summary_df, aes(x = Predictor, y = mean_beta)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  labs(y = "Regression coefficient (standardised)", x = "Feature") +
  coord_flip() +  # flip to make vertical labels easier to read
  theme(
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )


# Combine into one figure with cowplot
fig3_combined <- plot_grid(fig3, fig3b, labels = c("A", "B"), align = 'hv')

# Display the combined plot
print(fig3_combined)

ggsave(filename = "~/Documents/effort_function/Fig3.jpg",
       plot = fig3_combined, dpi = 300, width = 13, height = 6)


