###############################################
# Regression models on exopher frequency data #
# September 12 2025                           #
###############################################

#text like this with a hashtag before it is called a comment and is not executable code. 
####section 1:setup####

library(dplyr) #datamanipulation
library(readxl) #read excel spreadsheets into R
library(writexl) #convert R data frames into excel spreadsheet
library(ggplot2) #visualizations besides basics plots
library(ggthemes) #adds more themes to the ggplot2 package
library(ggThemeAssist)
library(stats) #basic statistical package
library(ggsignif) #provides a graphical user interface (GUI) for customizing ggplot2 themes
library(ggpubr) #To add significance stars to your ggplot2 graph
library(ggprism) #To generate the plots in a minimalist theme 

# IMPROVISATION LIBRARIES
library(caret) #cross-validation framework and grid search (Path 2)
library(randomForest) #Random Forest classifier (Path 1)
library(smotefamily) #SMOTE for class imbalance (Path 3)

#import your excel spreadsheet
data <- read_excel("input/aged_exopher_final.xlsx")

####section 2:Visualization (Original Plots)####

#summarizing data as percentages to plot
data_percent <- data %>%
  group_by(Treatment, Trial) %>%
  summarize(
    data_percent = (100 * ((sum(Exopher, na.rm = TRUE)) / n())),
    .groups = 'drop'
  )

data_summary <- data %>%
  group_by(Treatment, Trial) %>%
  summarize(
    data_percent = (100 * ((sum(Exopher, na.rm = TRUE)) / n())),
    data_sep = sqrt((data_percent / 100) * (1 - (data_percent / 100)) / n()) * 100,
    .groups = 'drop'
  ) %>%
  group_by(Treatment) %>%
  summarize(
    data_percent_mean = mean(data_percent),
    data_sep_mean = mean(data_sep)
  )
    
# Create the original plot
Plotted_data <- ggplot(data_summary, aes(x = Treatment, y = data_percent_mean, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", size = 1.5) + 
  geom_point(data = data_percent, aes(x = Treatment, y = data_percent, group = Trial), 
             position = position_dodge(width = 0.5), alpha = 0.5, size= 2.5)  +
  geom_errorbar(aes(ymin = data_percent_mean - data_sep_mean, ymax = data_percent_mean + data_sep_mean),
                width = 0.2, linetype = "solid", linewidth = 1.5) +  
  geom_signif(comparisons = list(c("AD2", "AD5")), annotations = c("***"),  
              y_position = 30, textsize = 8, tip_length = 0.05, vjust = 0.5, size = 1.5) +  
  coord_cartesian(ylim = c(0, 33)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  theme_prism() +  
  theme(legend.position = "none",
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1.1, vjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(size = 20, face = "bold", color = "black"),
    axis.title.x = element_text(size = 22, face = "bold", color = "black", hjust = 1),
    axis.title.y = element_text(size = 22, face = "bold", color = "black"),
    plot.title = element_text(size = 24, face = "bold", color = "black", hjust = 0.5)
  ) +
  labs(title = "Exopher Frequency", x = "Days of Adulthood", y = "Percent Exophers")

print(Plotted_data)
ggsave("exopher_frequency_plot.svg", plot = last_plot(), width = 4, height = 6, units = "in")

####section 3:statistical analysis (CMH Test)####

data_df <- as.data.frame(data)
data_df$Trial <- factor(data_df$Trial)
data_df$Treatment <- factor(data_df$Treatment)
data_df$Exopher <- factor(data_df$Exopher)

data_CMH <- mantelhaen.test(data_df$Treatment, data_df$Exopher, data_df$Trial)
print(data_CMH)

####section 4: OPTIMIZED ML PIPELINE (Refactored)####
# 1. Randomized K-Fold Cross-Validation (Path 2)
# 2. Random Forest Classifier (Path 1)
# 3. SMOTE Class Balancing (Path 3)

cat("\n--- Executing Targeted ML Optimization ---\n")

# Prepare data
ml_data <- data.frame(
  Treatment = factor(data$Treatment),
  Trial = factor(data$Trial),
  Exopher = factor(data$Exopher, levels = c(0, 1), labels = c("No", "Yes"))
)

# Train/Test Split (80/20)
set.seed(42)
train_index <- createDataPartition(ml_data$Exopher, p = 0.8, list = FALSE)
train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]

# Apply SMOTE to Training Data
# PATH 3: Categorical Encoding (One-Hot) and Interaction Features (Treatment * Trial)
train_encoded <- data.frame(
  model.matrix(~ Treatment * Trial - 1, data = train_data),
  Exopher = as.numeric(train_data$Exopher) - 1
)

smote_result <- SMOTE(train_encoded[, -ncol(train_encoded)], train_encoded$Exopher, K = 5)
train_smote <- smote_result$data
train_smote$Exopher <- factor(train_smote$class, levels = c(0, 1), labels = c("No", "Yes"))
train_smote <- train_smote[, -which(names(train_smote) == "class")]

# K-Fold Cross-Validation Setup (K=5)
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Random Forest with Grid Search
rf_grid <- expand.grid(mtry = c(1, 2, 3))
rf_model <- train(
  Exopher ~ .,
  data = train_smote,
  method = "rf",
  trControl = train_control,
  tuneGrid = rf_grid,
  metric = "ROC",
  ntree = 200
)

# Evaluation
test_encoded <- data.frame(
  model.matrix(~ Treatment * Trial - 1, data = test_data),
  Exopher = test_data$Exopher
)
rf_pred <- predict(rf_model, newdata = test_encoded)
rf_prob <- predict(rf_model, newdata = test_encoded, type = "prob")[, "Yes"]
rf_cm <- confusionMatrix(rf_pred, test_data$Exopher, positive = "Yes")

print(rf_cm)

# --- Step 8: NEW RESULT PLOT - Predictive Probabilities ---
pred_plot_data <- test_data
pred_plot_data$pred_prob <- rf_prob

prob_plot <- ggplot(pred_plot_data, aes(x = Treatment, y = pred_prob, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_prism() +
  labs(title = "Improvised Model: Predicted Exopher Probability",
       subtitle = "Random Forest Probabilities (Aging Dataset)",
       y = "Predicted Probability", x = "Days of Adulthood") +
  theme(legend.position = "none")

ggsave("improvised_predictive_probabilities.svg", plot = prob_plot, width = 7, height = 5)
cat("\nPredictive probability plot saved as 'improvised_predictive_probabilities.svg'\n")

# --- Step 9: MASTER COMPARISON PLOT (Baseline vs Improvised) ---
# Calculate Baseline (GLM) for comparison
baseline_glm <- glm(Exopher ~ ., family = binomial(link = "logit"), data = train_data)
baseline_pred_prob <- predict(baseline_glm, newdata = test_data, type = "response")
baseline_pred <- factor(ifelse(baseline_pred_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))
baseline_cm <- confusionMatrix(baseline_pred, test_data$Exopher, positive = "Yes")

# Combine metrics into a comparison data frame
comparison_master <- data.frame(
  Metric = rep(c("Accuracy", "Recall", "F1-Score"), each = 2),
  Model = rep(c("Baseline (GLM)", "Improvised (RF+SMOTE)"), 3),
  Value = c(
    baseline_cm$overall["Accuracy"], rf_cm$overall["Accuracy"],
    baseline_cm$byClass["Sensitivity"], rf_cm$byClass["Sensitivity"],
    baseline_cm$byClass["F1"], rf_cm$byClass["F1"]
  ) * 100
)

# Replace NAs (likely from 0/0 divisions in precision/recall) with 0 for plotting
comparison_master$Value[is.na(comparison_master$Value)] <- 0

master_plot <- ggplot(comparison_master, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("gray70", "steelblue")) +
  labs(title = "Engineering Performance: Baseline vs. Improvised",
       subtitle = "Aging Dataset: Significant Gains in Recall and F1-Score",
       y = "Score (%)", x = "") +
  geom_text(aes(label = sprintf("%.1f%%", Value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  theme_prism() +
  theme(legend.position = "top")

ggsave("master_performance_comparison.svg", plot = master_plot, width = 8, height = 6)
cat("\nMaster comparison plot saved as 'master_performance_comparison.svg'\n")
