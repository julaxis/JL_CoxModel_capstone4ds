

library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(gt)
library(tidyr)
library(tidyverse)

data <- read.csv("/Users/lexlove/Desktop/Breast_Cancer.csv")

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

#------------------
variable_table <- data.frame(
  Variable = c(
    "Age", "Race", "6th Stage", "Differentiate", "Tumor Size",
    "Survival Months", "Estrogen Status", "Progesterone Status",
    "Regional Nodes Examined", "Regional Nodes Positive", "Status"
  ),
  Definition = c(
    "This variable is the patient’s age at diagnosis.",
    "This variable is the patient’s self-identified racial category.",
    "This variable is the cancer stage based on the AJCC 6th Edition.",
    "This variable is the tumor grade based on how abnormal the cells appear.",
    "This variable is the measured size of the primary tumor.",
    "This variable is the number of months from diagnosis to last follow-up or death.",
    "This variable is an indicator of estrogen receptor expression.",
    "This variable is an indicator of progesterone receptor expression.",
    "This variable is the number of lymph nodes examined.",
    "This variable is the number of lymph nodes found positive for cancer.",
    "This variable is the patient’s vital status at last follow-up."
  ),
  stringsAsFactors = FALSE
)

variable_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Variable Description"
  ) %>%
  tab_footnote(
    footnote = "Each variable in the dataset, accompanied by a qualitative description."
  )

##Coding scheme
codebook <- tibble(
  Variable = c(
    "Status",
    "Race",
    "Differentiate(Tumor grade))",
    "Estrogen Status",
    "Progesterone Status",
    "Marital Status"
  ),
  
  Coding = c(
    "1 = Alive; 0 = Dead",
    "1 = White; 2 = Black; 3 = Other",
    "1 = Well differentiated; 2 = Moderately differentiated; 3 = Poorly differentiated; 4 = Undifferentiated",
    "1 = Positive; 0 = Negative",
    "1 = Positive; 0 = Negative",
    "1 = Married; 2 = Single; 3 = Divorced; 4 = Widowed; 5 = Separated"
  )
)

codebook %>%
  gt() %>%
  tab_header(
    title = "Table 2. Variable Coding Details"
  ) %>%
  cols_label(
    Variable = "Variable",
    Coding = "Coding Scheme"
  )

#Clean Data summary
make_cat_table <- function(data, var, label) {
  tbl <- table(data[[var]])
  pct <- prop.table(tbl) * 100
  
  tibble(
    Variable = label,
    Category = names(tbl),
    Summary = paste0(
      as.numeric(tbl), " (", sprintf("%.1f", pct), "%)"
    )
  )
}

# Build table sections
table_age <- tibble(
  Variable = "Age (mean ± SD)",
  Category = "",
  Summary = sprintf("%.1f ± %.1f",
                    mean(cleaned_data$Age, na.rm = TRUE),
                    sd(cleaned_data$Age, na.rm = TRUE))
)

table_race <- make_cat_table(cleaned_data, "Race", "Race")
table_marital <- make_cat_table(cleaned_data, "Marital.Status", "Marital Status")
table_stage <- make_cat_table(cleaned_data, "AJCC_Stage", "AJCC Stage")
table_grade <- make_cat_table(cleaned_data, "differentiate", "Tumor Grade")
table_er <- make_cat_table(cleaned_data, "Estrogen.Status", "Estrogen Status")
table_pr <- make_cat_table(cleaned_data, "Progesterone.Status", "Progesterone Status")
table_status <- make_cat_table(cleaned_data, "Status", "Vital Status")

table_tumor_size <- tibble(
  Variable = "Tumor Size (mean ± SD)",
  Category = "",
  Summary = sprintf("%.1f ± %.1f",
                    mean(cleaned_data$Tumor.Size, na.rm = TRUE),
                    sd(cleaned_data$Tumor.Size, na.rm = TRUE))
)

table_nodes_examined <- tibble(
  Variable = "Regional Nodes Examined (mean ± SD)",
  Category = "",
  Summary = sprintf("%.1f ± %.1f",
                    mean(cleaned_data$Regional.Node.Examined, na.rm = TRUE),
                    sd(cleaned_data$Regional.Node.Examined, na.rm = TRUE))
)

table_nodes_positive <- tibble(
  Variable = "Regional Nodes Positive (mean ± SD)",
  Category = "",
  Summary = sprintf("%.1f ± %.1f",
                    mean(cleaned_data$Regional.Node.Positive, na.rm = TRUE),
                    sd(cleaned_data$Regional.Node.Positive, na.rm = TRUE))
)

table_survival <- tibble(
  Variable = "Survival Months (mean ± SD)",
  Category = "",
  Summary = sprintf("%.1f ± %.1f",
                    mean(cleaned_data$Survival.Months, na.rm = TRUE),
                    sd(cleaned_data$Survival.Months, na.rm = TRUE))
)

# Combine all sections
table1 <- bind_rows(
  table_age,
  table_race,
  table_marital,
  table_stage,
  table_grade,
  table_tumor_size,
  table_er,
  table_pr,
  table_nodes_examined,
  table_nodes_positive,
  table_survival,
  table_status
)

# Create GT table
table1 %>%
  gt() %>%
  tab_header(
    title = "Table 3. Baseline Characteristics"
  ) %>%
  cols_label(
    Variable = "Variable",
    Category = "Category",
    Summary = "n (%) or Mean ± SD"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Variable
    )
  )

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
  labs(title = "AJCC Stage Distribution", x = "AJCC Stage", y = "Count")

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
  risk.table.height = 0.3,
  risk.table.fontsize = 5,
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



