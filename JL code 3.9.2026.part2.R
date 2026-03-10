

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)


# Load dataset
data <- read.csv("/Users/lexlove/Desktop/Breast_Cancer.csv")

# Preview dataset
head(data)
str(data)

###cleaning data
cleaned_data <- data %>%
  select(
    Age, Race, Marital.Status, X6th.Stage, differentiate,
    Tumor.Size, Estrogen.Status, Progesterone.Status,
    Regional.Node.Examined, Reginol.Node.Positive, Survival.Months,
    Status
  )

cleaned_data <- cleaned_data %>%
  rename(
    `AJCC_Stage` = X6th.Stage,
    `Regional.Node.Positive` = Reginol.Node.Positive
  )

cleaned_data <- cleaned_data %>%
  mutate(
    Status = case_when(
      Status == "Alive" ~ 1,
      Status == "Dead"  ~ 0
    ),
    Race = case_when(
      Race == "White" ~ 1,
      Race == "Black" ~ 2,
      Race == "Other" ~ 3
    ),
    differentiate = case_when(
      differentiate == "Well differentiated" ~ 1, ##grade 1
      differentiate == "Moderately differentiated" ~ 2, ## grade 2
      differentiate == "Poorly differentiated" ~ 3, ## grade 3
      differentiate == "Undifferentiated" ~ 4
    ),
    Estrogen.Status = case_when(
      Estrogen.Status == "Positive" ~ 1,
      Estrogen.Status == "Negative" ~ 0
    ),
    Progesterone.Status = case_when(
      Progesterone.Status == "Positive" ~ 1,
      Progesterone.Status == "Negative" ~ 0
    ),
      Marital.Status = case_when(
        Marital.Status == "Married" ~ 1,
        Marital.Status == "Single " ~ 2,
        Marital.Status == "Divorced" ~ 3,
        Marital.Status == "Widowed" ~ 4,
        Marital.Status == "Separated" ~ 5
      )
  )

#check for missingness
colSums(is.na(cleaned_data))

summary(cleaned_data)

#### 3.2 Distributions/ Counts
 
  #age
ggplot(cleaned_data, aes(Age)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

  #tumor
ggplot(cleaned_data, aes(Tumor.Size)) +
  geom_histogram(binwidth = 5, fill = "tomato", color = "white") +
  labs(title = "Tumor Size Distribution", x = "Tumor Size (mm)", y = "Count")

  #hormone
ggplot(cleaned_data, aes(factor(Estrogen.Status))) +
  geom_bar(fill = "purple") +
  labs(title = "Estrogen Receptor Status", x = "Status (1=Positive, 0=Negative)", y = "Count")

  #stage
ggplot(cleaned_data, aes(AJCC_Stage)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Patients by AJCC Stage", x = "AJCC Stage", y = "Count")


## 3.3 Kaplan–Meier Survival Analysis
###Compare survival across AJCC stages
##Present Kaplan–Meier curves
##Report log-rank test results
#```{r}
# -----------------------------
# Create survival object
# -----------------------------
surv_object <- Surv(cleaned_data$Survival.Months, cleaned_data$Status)

# Fit Kaplan–Meier model by AJCC stage
# -----------------------------
km_fit <- survfit(surv_object ~ AJCC_Stage, data = cleaned_data)

# -----------------------------
# Kaplan–Meier survival curves by stage
# -----------------------------
ggsurvplot(
  km_fit,
  data = cleaned_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  legend.title = "AJCC Stage",
  xlab = "Months",
  ylab = "Survival Probability",
  palette = "Dark2"
)

## Kaplan–Meier survival curves by estrogen
km_fit_er <- survfit(surv_object ~ Estrogen.Status, data = cleaned_data)

ggsurvplot(
  km_fit_er,
  data = cleaned_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  legend.title = "ER Status",
  legend.labs = c("Negative", "Positive")
)


## 3.4 Cox Proportional Hazards Model
##Fit Cox model adjusting for covariates
##Report hazard ratios and confidence intervals
##Evaluate proportional hazards assumption
##```{r}
# -----------------------------
# Fit Cox proportional hazards model
# -----------------------------
cox_model <- coxph(
  Surv(Survival.Months, Status) ~ AJCC_Stage + Age + Race + differentiate + Tumor.Size,
  data = cleaned_data
)

# -----------------------------
# View Cox model summary
# -----------------------------
summary(cox_model)

# -----------------------------
# Forest plot of hazard ratios
# -----------------------------
ggforest(cox_model, data = cleaned_data, main = "Hazard Ratios for Breast Cancer Survival")

# -----------------------------
# Test proportional hazards assumption
# -----------------------------
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test)


