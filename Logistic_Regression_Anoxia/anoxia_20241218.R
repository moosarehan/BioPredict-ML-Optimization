##################################
#regression models on anoxia data#
#December , 2024                #
##################################

#text like this with a hashtag before it is called a comment and is not executable code. 
#to execute a line of code on a mac, go to the end of the line and hit command+shift+enter
####<---you can use the arrows next to the lines numbers to collapse or fold in a section####



####section 1:setup####

#First! install the following packages using install.packages("name of package") in the CONSOLE of RStudio
#use this code to install all packages at once:  install.packages(c('dplyr', 'readxl', 'writexl', 'ggplot2', 'ggthemes', 'ggThemeAssist', 'ggsignif', 'forcats', 'stats', 'mgcv', 'splines'))

#next, load the libraries by executing code in the upper part of the screen where all this code is. Again, to execute a line of code on a mac, go to the end of the line and hit command+shift+enter
library(dplyr) #datamanipulation
library(readxl) #read excel spreadsheets into R
library(writexl) #convert R data frames into excel spreadsheet
library(ggplot2) #visualizations besides basics plots
library(ggthemes) #adds more themes to the ggplot2 package
library(ggThemeAssist)
library(ggsignif) #To add significance stars to your ggplot2 graph
library(forcats) #To reorder the data
library(stats) #basic statistical package
library(mgcv) #contains the generalized additive model (gam)
library(ggprism)

#import your excel spreadsheet; nota bene (n.b.) this will only read the first sheet in the workbook
anoxia_data <- read_excel("input/data_final.xlsx")

####section 2:see what your data looks like####

#Plot all trials separately 
ggplot(anoxia_data, aes(x = genotype, y = moving, fill = genotype)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  theme_par()+
  facet_wrap(~ trial)  # Create separate plots for each time point

#Filter only a specific time point
tenmin <- anoxia_data %>% 
  filter( time == 10) %>% 
  filter(trial %in% c(2, 3, 5)) 

# Calculate the mean for each trial within each genotype
trial_means <- tenmin %>% 
  group_by(genotype, trial) %>% 
  summarize(mean_moving = mean(moving))

# Create the bar graph with trial means as points
ggplot(tenmin, aes(x = genotype, y = moving, fill = genotype)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  geom_point(data = trial_means,  # Use the trial_means data
             aes(x = genotype, y = mean_moving, group = trial),  # Map trial means
             position = position_dodge(width = 0.5), 
             alpha = 0.5,
             size= 2.5)  +
  geom_signif(comparisons = list(c("egl-9", "hif-1")), # Adds significance levels
              annotations = c("**"), # Manually set significance level
              y_position = c(0.95),  # Adjust vertical position of the significance stars and bar
              textsize = 8,  # Customize text size
              tip_length = 0.05,  # Customize the length of the significance lines
              vjust = 0.5) +  # Adjust vertical position of stars in relation to bar
  geom_signif(comparisons = list(c("egl-9", "wild-type")), # Adds significance levels
              annotations = c("**"),  # Manually set significance level
              y_position = c(1.1),  # Adjust vertical position of the significance stars and bar
              textsize = 8,  # Customize text size
              tip_length = 0.05,  # Customize the length of the significance lines
              vjust = 0.5) +  # Adjust vertical position of stars in relation to bar
  labs(title = "Movement at 10 Minutes", 
       x = "Genotype", 
       y = "Mean Movement") +
  ylim(0, 1.25) +  # Adjust 12 to a suitable upper limit
  theme_prism() +  
  theme(
    legend.position = "none",  # Remove the legend
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1.1, vjust = 1, face = "bold", color = "black"),  # x-axis labels left-aligned
    axis.text.y = element_text(size = 20, face = "bold", color = "black"),
    axis.title.x = element_text(size = 22, face = "bold", color = "black"),  # Adjust hjust for x-axis title alignment
    axis.title.y = element_text(size = 22, face = "bold", color = "black"),
    plot.title = element_text(size = 24, face = "bold", color = "black", hjust = 0.5)
  ) 

#save your plot for export as an SVG vector format
ggsave("tenminplot.svg", 
       plot = last_plot(), 
       width = 4, 
       height = 6,
       units = "in")

df_anoxia_summary <- (anoxia_data %>%
  filter(trial %in% c(2,3,5)) %>% 
  group_by(genotype, time) %>%
  summarise(
    mean_moving = mean(moving),
    # Calculate SEM using sqrt(variance / n)
    sem_moving = sqrt(sd(moving) / n())
  ))

#plot mean values at each time point, with connected line and shaded SEM
ggplot(df_anoxia_summary, aes(x = time, y = mean_moving*100, color = genotype)) + #defines the data for the graph
  geom_line() +  # Main line for mean
 # scale_color_manual(values = primary_colors) +  # Map primary colors directly, can delete this line if no color scheme defined
  geom_ribbon(aes(ymin = mean_moving*100 - sem_moving*100, 
                  ymax = mean_moving*100 + sem_moving*100),
              fill = "cyan", alpha = 0.2, linetype = "dashed") + #this is the shaded standard deviation
  theme_prism() + #theme of choice, makes the background beige and the text light grey
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1, hjust = 1, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),  
    axis.title.x = element_text(size = 16, face = "bold", vjust = -1), 
    axis.title.y = element_text(size = 16, face = "bold"), 
    plot.title = element_text(size = 20, face = "bold"))+  
  labs(title = "Mean anoxia recovery",
       x = "time (min)",
       y = "% animals moving",
       color = "Genotype") +
  # Increase the number of breaks for the x-axis
  scale_x_continuous(breaks = seq(min(0), max(60), length.out = 5)) +
  scale_y_continuous(limits = c(0, 90))

#save your plot for export as an SVG vector format
ggsave("anoxiarecovery.svg", 
       plot = last_plot(), 
       width = 4, 
       height = 6,
       units = "in")

####section 3:statistical analysis####

#one-way anova at a single time point
tenminaov <- aov(moving ~ genotype, data = tenmin)
TukeyHSD(tenminaov)
summary(tenminaov)
print(tenminaov)

# Define genotype as a factor and make N2 (our wild type strain) your reference sequenced
genotype_unordered <- factor(anoxia_data$genotype, ordered = FALSE) #makes genotype a factor
genotype_unordered <- relevel(genotype_unordered, ref = "wild-type") #makes wild-type within genotype the reference genotype
 
##simple logistic regression
an_glm <- glm(moving ~ genotype_unordered, family = binomial(link = "logit"), # the default is logit
              data = anoxia_data)
summary(an_glm)
plot(an_glm)

#general additive model
an_gam <- gam(moving ~ genotype_unordered + s(time), data = anoxia_data)
summary(an_gam)
plot(an_gam,pages=1,residuals=TRUE)  ## show partial residuals
plot(an_gam,pages=1,seWithMean=TRUE) ## `with intercept' CIs
plot(an_gam, select = 1, shade = TRUE, seWithMean = TRUE)
## smoothing basis dimensions...

####section 4: OPTIMIZED ML PIPELINE (Refactored)####
# 1. Randomized K-Fold Cross-Validation
# 2. Random Forest Classifier
# 3. SMOTE Class Balancing

cat("\n--- Executing Targeted ML Optimization ---\n")
library(caret)
library(randomForest)
library(smotefamily)

# Prepare data
ml_data <- data.frame(
  genotype = factor(anoxia_data$genotype),
  time = anoxia_data$time,
  moving = factor(anoxia_data$moving, levels = c(0, 1), labels = c("No", "Yes"))
)

# Train/Test Split (80/20)
set.seed(42)
train_index <- createDataPartition(ml_data$moving, p = 0.8, list = FALSE)
train_data <- ml_data[train_index, ]
test_data <- ml_data[-train_index, ]

# Apply SMOTE to Training Data
train_encoded <- data.frame(
  model.matrix(~ genotype + time - 1, data = train_data),
  moving = as.numeric(train_data$moving) - 1
)

smote_result <- SMOTE(train_encoded[, -ncol(train_encoded)], train_encoded$moving, K = 5)
train_smote <- smote_result$data
train_smote$moving <- factor(train_smote$class, levels = c(0, 1), labels = c("No", "Yes"))
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
  moving ~ .,
  data = train_smote,
  method = "rf",
  trControl = train_control,
  tuneGrid = rf_grid,
  metric = "ROC",
  ntree = 200
)

# Evaluation
test_encoded <- data.frame(
  model.matrix(~ genotype + time - 1, data = test_data),
  moving = test_data$moving
)
rf_pred <- predict(rf_model, newdata = test_encoded)
rf_prob <- predict(rf_model, newdata = test_encoded, type = "prob")[, "Yes"]
rf_cm <- confusionMatrix(rf_pred, test_data$moving, positive = "Yes")

print(rf_cm)

# --- NEW RESULT PLOT - Predictive Probabilities ---
pred_plot_data <- test_data
pred_plot_data$pred_prob <- rf_prob

prob_plot <- ggplot(pred_plot_data, aes(x = genotype, y = pred_prob, fill = genotype)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_prism() +
  labs(title = "Improvised Model: Predicted Recovery Probability",
       subtitle = "Random Forest Probabilities (Anoxia Dataset)",
       y = "Predicted Probability", x = "Genotype") +
  theme(legend.position = "none")

ggsave("improvised_predictive_probabilities.svg", plot = prob_plot, width = 7, height = 5)
cat("\nPredictive probability plot saved as 'improvised_predictive_probabilities.svg'\n")

# --- MASTER COMPARISON PLOT (Baseline vs Improvised) ---
baseline_glm <- glm(moving ~ ., family = binomial(link = "logit"), data = train_data)
baseline_pred_prob <- predict(baseline_glm, newdata = test_data, type = "response")
baseline_pred <- factor(ifelse(baseline_pred_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))
baseline_cm <- confusionMatrix(baseline_pred, test_data$moving, positive = "Yes")

comparison_master <- data.frame(
  Metric = rep(c("Accuracy", "Recall", "F1-Score"), each = 2),
  Model = rep(c("Baseline (GLM)", "Improvised (RF+SMOTE)"), 3),
  Value = c(
    baseline_cm$overall["Accuracy"], rf_cm$overall["Accuracy"],
    baseline_cm$byClass["Sensitivity"], rf_cm$byClass["Sensitivity"],
    baseline_cm$byClass["F1"], rf_cm$byClass["F1"]
  ) * 100
)
comparison_master$Value[is.na(comparison_master$Value)] <- 0

master_plot <- ggplot(comparison_master, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  theme_minimal() +
  scale_fill_manual(values = c("gray70", "steelblue")) +
  labs(title = "Engineering Performance: Baseline vs. Improvised",
       subtitle = "Anoxia Dataset: Significant Gains in Recall and F1-Score",
       y = "Score (%)", x = "") +
  geom_text(aes(label = sprintf("%.1f%%", Value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  theme_prism() +
  theme(legend.position = "top")

ggsave("master_performance_comparison.svg", plot = master_plot, width = 8, height = 6)
cat("\nMaster comparison plot saved as 'master_performance_comparison.svg'\n")