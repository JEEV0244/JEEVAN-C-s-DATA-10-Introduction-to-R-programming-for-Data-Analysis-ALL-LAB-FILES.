############################################################
# DATA 104 : Introduction to R Programming for Data Analysis
# Mid-Semester Assessment
#
# Name       : JEEVAN C
# Student ID : 0626576
# Date       : 21-10-2026
#
# Question 1 : Student Performance Analysis Using R

# DATA SOURCE
#   Cortez, P. and Silva, A. (2008). Using Data Mining to
#       Predict Secondary School Student Performance. UCI Machine
#       Learning Repository, "Student Performance" dataset.
#       https://archive.ics.uci.edu/dataset/320/student+performance
############################################################


############################################################
# 0. SETUP : packages, folders, theme, helper functions
############################################################
rm(list = ls())

required_pkgs <- c("dplyr", "tidyr", "ggplot2")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Folder where every figure is saved
out_dir <- file.path(getwd(), "R_Q1_Outputs")
if (!dir.exists(out_dir)) dir.create(out_dir)

# Colour palette used in all charts
navy   <- "#1F3A5F"
teal   <- "#2A9D8F"
orange <- "#E76F51"
gold   <- "#E9A23B"
grey   <- "#6B7280"

# Common clean theme for every plot
theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, colour = navy),
    plot.subtitle    = element_text(colour = grey),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# Helper: nice section banner in the console
section <- function(title) {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
}

# Helper: show a plot and save it as a PNG
save_plot <- function(p, filename, w = 7.5, h = 4.8) {
  print(p)
  ggsave(file.path(out_dir, filename), plot = p, width = w, height = h, dpi = 200,
         bg = "white")
  cat("Saved figure:", filename, "\n")
}


############################################################
# QUESTION 1 : STUDENT PERFORMANCE ANALYSIS USING R
############################################################

############################################################
# Q1 (a) Data Import and Inspection
############################################################
section("Q1 (a) DATA IMPORT AND INSPECTION")

# Choose the file from your computer (student-mat.csv)
mat_path <- Sys.getenv("MAT_CSV", unset = "")
if (!nzchar(mat_path)) {
  cat("Please select student-mat.csv in the window that opens...\n")
  mat_path <- file.choose()
}

# This file uses a semicolon (;) as separator, so sep = ";" is needed
students <- read.csv(mat_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)

cat("\nDimensions (rows, columns):", dim(students), "\n")

cat("\nColumn names:\n")
print(names(students))

cat("\nStructure of the data:\n")
str(students)

cat("\nFirst six observations:\n")
print(head(students, 6))

# Data type of every variable
cat("\nData type of each variable:\n")
type_table <- data.frame(
  Variable  = names(students),
  Data_Type = sapply(students, function(x) class(x)[1]),
  row.names = NULL
)
print(type_table)
cat("\nNumber of variables of each type:\n")
print(table(type_table$Data_Type))


############################################################
# Q1 (b) Data Cleaning
############################################################
section("Q1 (b) DATA CLEANING")

# Step 1: missing values (NA)
na_per_column <- colSums(is.na(students))
cat("Total missing (NA) values:", sum(na_per_column), "\n")

# Step 2: empty strings that hide as "valid" text
blank_per_column <- sapply(students, function(x) sum(x == "", na.rm = TRUE))
cat("Total blank text entries :", sum(blank_per_column), "\n")

# Step 3: handle missing values if any exist
# (numeric -> median, text -> most frequent value). Nothing changes
# for this dataset because it has no missing values, but the code is
# written so it would fix the problem if it were present.
for (col in names(students)) {
  if (anyNA(students[[col]])) {
    if (is.numeric(students[[col]])) {
      students[[col]][is.na(students[[col]])] <- median(students[[col]], na.rm = TRUE)
    } else {
      most_common <- names(sort(table(students[[col]]), decreasing = TRUE))[1]
      students[[col]][is.na(students[[col]])] <- most_common
    }
  }
}

# Step 4: duplicate records
dup_count <- sum(duplicated(students))
cat("Duplicate rows found     :", dup_count, "\n")
students <- students %>% distinct()
cat("Rows after removing duplicates:", nrow(students), "\n")

# Step 5: make sure grades and age are in a valid range
cat("\nRange check:\n")
cat("G1 range:", range(students$G1), "| G2 range:", range(students$G2),
    "| G3 range:", range(students$G3), "\n")
cat("Age range:", range(students$age), "| Absences range:", range(students$absences), "\n")

# Step 6: text columns -> factors so groups are handled properly
text_cols <- names(students)[sapply(students, is.character)]
students[text_cols] <- lapply(students[text_cols], factor)
cat("\nColumns converted to factor:", length(text_cols), "\n")

# Step 7: students with a final grade of 0
zero_final <- sum(students$G3 == 0)
cat("Students with G3 = 0 :", zero_final,
    "(kept in the data, they are real records of students who failed or left)\n")

cat("\nFinal clean dataset has", nrow(students), "rows and", ncol(students), "columns\n")


############################################################
# Q1 (c) Data Manipulation Using dplyr
############################################################
section("Q1 (c) DATA MANIPULATION USING dplyr")

# select() : relevant student attributes and academic scores
selected_students <- students %>%
  select(school, sex, age, studytime, failures, absences, higher, internet, G1, G2, G3)
cat("select(): chosen columns ->", paste(names(selected_students), collapse = ", "), "\n")
cat("\nFirst 6 rows of the selected data:\n")
print(head(selected_students, 6))

# filter() : final grade at least 15 out of 20
high_achievers <- students %>% filter(G3 >= 15)
cat("\nfilter(): students with final grade (G3) >= 15 :", nrow(high_achievers),
    "out of", nrow(students), "\n")
print(head(high_achievers %>% select(school, sex, age, studytime, absences, G1, G2, G3), 8))

# mutate() : Performance_Category using conditional logic
students <- students %>%
  mutate(
    Performance_Category = case_when(
      G3 >= 15 ~ "Excellent",
      G3 >= 10 ~ "Satisfactory",
      TRUE     ~ "At Risk"
    ),
    Performance_Category = factor(Performance_Category,
                                  levels = c("At Risk", "Satisfactory", "Excellent")),
    Grade_Change = G3 - G1,
    Attendance_Group = case_when(
      absences <= 5  ~ "Low absences (0-5)",
      absences <= 15 ~ "Medium absences (6-15)",
      TRUE           ~ "High absences (16+)"
    ),
    Attendance_Group = factor(Attendance_Group,
                              levels = c("Low absences (0-5)", "Medium absences (6-15)",
                                         "High absences (16+)"))
  )
cat("\nmutate(): new variables Performance_Category, Grade_Change and Attendance_Group created\n")
print(table(students$Performance_Category))

# group_by() + summarize() : average final grade and number of students per category
category_summary <- students %>%
  group_by(Performance_Category) %>%
  summarize(
    Students       = n(),
    Percentage     = round(n() / nrow(students) * 100, 1),
    Avg_Final_Grade = round(mean(G3), 2),
    Avg_Absences   = round(mean(absences), 2),
    Avg_Study_Time = round(mean(studytime), 2),
    .groups = "drop"
  )
cat("\nAverage final grade and number of students in each performance category:\n")
print(as.data.frame(category_summary))

# group_by() on the school (department-level outcome)
school_summary <- students %>%
  group_by(school) %>%
  summarize(
    Students        = n(),
    Avg_Final_Grade = round(mean(G3), 2),
    Avg_Absences    = round(mean(absences), 2),
    Pass_Rate_Pct   = round(mean(G3 >= 10) * 100, 1),
    .groups = "drop"
  )
cat("\nOutcome by school:\n")
print(as.data.frame(school_summary))

# Attendance (absences) versus final grade
attendance_summary <- students %>%
  group_by(Attendance_Group) %>%
  summarize(Students = n(), Avg_Final_Grade = round(mean(G3), 2), .groups = "drop")
cat("\nAttendance versus final grade:\n")
print(as.data.frame(attendance_summary))

# arrange() : sort by final grade, highest first
sorted_students <- students %>%
  select(school, sex, age, studytime, absences, G1, G2, G3, Performance_Category) %>%
  arrange(desc(G3), desc(G2), desc(G1))
cat("\narrange(): top 10 students by final grade (descending):\n")
print(head(sorted_students, 10))


############################################################
# Q1 (d) Exploratory Data Analysis (EDA)
############################################################
section("Q1 (d) EXPLORATORY DATA ANALYSIS")

# Descriptive statistics
num_vars <- c("age", "studytime", "failures", "absences", "G1", "G2", "G3")
desc_stats <- data.frame(
  Variable = num_vars,
  Mean   = sapply(students[num_vars], mean),
  Median = sapply(students[num_vars], median),
  SD     = sapply(students[num_vars], sd),
  Min    = sapply(students[num_vars], min),
  Max    = sapply(students[num_vars], max),
  IQR    = sapply(students[num_vars], IQR),
  row.names = NULL
)
desc_stats[, -1] <- round(desc_stats[, -1], 2)
cat("Descriptive statistics:\n")
print(desc_stats)

cat("\nSummary of the final grade (G3):\n")
print(summary(students$G3))

g3_skew <- mean((students$G3 - mean(students$G3))^3) / sd(students$G3)^3
cat("\nSkewness of G3:", round(g3_skew, 3), "\n")

# Average final grade for different student groups
cat("\nAverage final grade by school:\n")
print(as.data.frame(students %>% group_by(school) %>%
                      summarize(Mean_G3 = round(mean(G3), 2), Median_G3 = median(G3), .groups = "drop")))
cat("\nAverage final grade by sex:\n")
print(as.data.frame(students %>% group_by(sex) %>%
                      summarize(Mean_G3 = round(mean(G3), 2), Median_G3 = median(G3), .groups = "drop")))
cat("\nAverage final grade by weekly study time (1 = <2h, 2 = 2-5h, 3 = 5-10h, 4 = >10h):\n")
print(as.data.frame(students %>% group_by(studytime) %>%
                      summarize(Students = n(), Mean_G3 = round(mean(G3), 2), .groups = "drop")))
cat("\nAverage final grade by past class failures:\n")
print(as.data.frame(students %>% group_by(failures) %>%
                      summarize(Students = n(), Mean_G3 = round(mean(G3), 2), .groups = "drop")))
cat("\nAverage final grade by wish to take higher education:\n")
print(as.data.frame(students %>% group_by(higher) %>%
                      summarize(Students = n(), Mean_G3 = round(mean(G3), 2), .groups = "drop")))

# Correlation matrix
cat("\nCorrelation matrix of the main numeric variables:\n")
print(round(cor(students[c("G1", "G2", "G3", "absences", "studytime", "failures", "age")]), 2))

# ---- Figure 1: Histogram of final grades ----
fig1 <- ggplot(students, aes(x = G3)) +
  geom_histogram(breaks = seq(-0.5, 20.5, by = 1), fill = navy, colour = "white") +
  geom_vline(xintercept = mean(students$G3), colour = orange, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(students$G3), colour = teal, linetype = "dotted", linewidth = 1) +
  ggplot2::annotate("text", x = mean(students$G3) - 0.3, y = Inf, vjust = 2, hjust = 1,
                    label = paste("Mean =", round(mean(students$G3), 2)), colour = orange,
                    fontface = "bold") +
  ggplot2::annotate("text", x = median(students$G3) + 0.3, y = Inf, vjust = 4, hjust = 0,
                    label = paste("Median =", median(students$G3)), colour = teal,
                    fontface = "bold") +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Distribution of Final Grades (G3)",
       subtitle = "Mathematics course, 395 students",
       x = "Final grade (out of 20)", y = "Number of students") +
  theme_report
save_plot(fig1, "Q1_Fig1_Histogram_Final_Grades.png")

# ---- Figure 2: Boxplot of final grade by school ----
fig2 <- ggplot(students, aes(x = school, y = G3, fill = school)) +
  geom_boxplot(alpha = 0.85, width = 0.55, outlier.colour = orange) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
  scale_fill_manual(values = c("GP" = navy, "MS" = teal)) +
  labs(title = "Final Grade by School",
       subtitle = "White diamond = group mean (GP = Gabriel Pereira, MS = Mousinho da Silveira)",
       x = "School", y = "Final grade (G3)") +
  theme_report + theme(legend.position = "none")
save_plot(fig2, "Q1_Fig2_Boxplot_By_School.png", h = 4.6)

# ---- Figure 3: Boxplots for several student groups ----
group_long <- students %>%
  select(G3, sex, higher, internet, address) %>%
  mutate(across(-G3, as.character)) %>%
  pivot_longer(cols = -G3, names_to = "Group_Variable", values_to = "Group") %>%
  mutate(Group_Variable = recode(Group_Variable,
                                 sex = "Sex (F / M)",
                                 higher = "Wants higher education",
                                 internet = "Internet at home",
                                 address = "Home address (R / U)"))
fig3 <- ggplot(group_long, aes(x = Group, y = G3, fill = Group_Variable)) +
  geom_boxplot(alpha = 0.85, width = 0.55, outlier.colour = orange) +
  facet_wrap(~ Group_Variable, scales = "free_x") +
  scale_fill_manual(values = c(navy, teal, gold, orange)) +
  labs(title = "Final Grade Across Selected Student Groups",
       x = "Group", y = "Final grade (G3)") +
  theme_report + theme(legend.position = "none")
save_plot(fig3, "Q1_Fig3_Boxplot_Selected_Groups.png", h = 5.4)

# ---- Figure 4: Boxplot by weekly study time ----
study_labels <- c("1" = "<2 h", "2" = "2-5 h", "3" = "5-10 h", "4" = ">10 h")
fig4 <- ggplot(students, aes(x = factor(studytime, labels = study_labels), y = G3,
                             fill = factor(studytime))) +
  geom_boxplot(alpha = 0.85, width = 0.6, outlier.colour = orange) +
  scale_fill_manual(values = c(navy, teal, gold, orange)) +
  labs(title = "Final Grade by Weekly Study Time",
       x = "Weekly study time", y = "Final grade (G3)") +
  theme_report + theme(legend.position = "none")
save_plot(fig4, "Q1_Fig4_Boxplot_Study_Time.png", h = 4.6)

# ---- Figure 5: Performance categories ----
fig5 <- ggplot(category_summary, aes(x = Performance_Category, y = Students,
                                     fill = Performance_Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Students, " (", Percentage, "%)")), vjust = -0.5,
            fontface = "bold") +
  scale_fill_manual(values = c("At Risk" = orange, "Satisfactory" = gold, "Excellent" = teal)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Number of Students in Each Performance Category",
       subtitle = "Excellent: G3 >= 15 | Satisfactory: 10-14 | At Risk: below 10",
       x = "Performance category", y = "Number of students") +
  theme_report + theme(legend.position = "none")
save_plot(fig5, "Q1_Fig5_Performance_Categories.png", h = 4.6)

# ---- Figure 6: G2 versus G3 ----
fig6 <- ggplot(students, aes(x = G2, y = G3, colour = Performance_Category)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.7, size = 2) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, colour = navy, linewidth = 1) +
  scale_colour_manual(values = c("At Risk" = orange, "Satisfactory" = gold, "Excellent" = teal)) +
  labs(title = "Second-Period Grade (G2) versus Final Grade (G3)",
       subtitle = paste("Correlation =", round(cor(students$G2, students$G3), 2)),
       x = "Second-period grade (G2)", y = "Final grade (G3)", colour = "Category") +
  theme_report
save_plot(fig6, "Q1_Fig6_G2_vs_G3.png")


############################################################
# Q1 (e) Control Statements : if...else and for loop
############################################################
section("Q1 (e) CONTROL STATEMENTS")

# Classify every student with a for loop + if...else if...else
students$Result <- NA_character_
for (i in seq_len(nrow(students))) {
  if (students$G3[i] >= 15) {
    students$Result[i] <- "Distinction"
  } else if (students$G3[i] >= 10) {
    students$Result[i] <- "Pass"
  } else {
    students$Result[i] <- "Fail"
  }
}

# Process the first 10 student records one by one
cat("Processing the first 10 student records:\n")
for (i in 1:10) {
  cat("Student", i, "| school:", as.character(students$school[i]),
      "| sex:", as.character(students$sex[i]),
      "| G3 =", students$G3[i], "->", students$Result[i], "\n")
}

# Count the results with a loop and a counter
n_distinction <- 0
n_pass <- 0
n_fail <- 0
for (r in students$Result) {
  if (r == "Distinction") {
    n_distinction <- n_distinction + 1
  } else if (r == "Pass") {
    n_pass <- n_pass + 1
  } else {
    n_fail <- n_fail + 1
  }
}
cat("\nResults of the loop over all", nrow(students), "students:\n")
cat("Distinction:", n_distinction, "| Pass:", n_pass, "| Fail:", n_fail, "\n")

# Check the loop against the dplyr category made earlier
cat("Same counts from Performance_Category:",
    sum(students$Performance_Category == "Excellent"),
    sum(students$Performance_Category == "Satisfactory"),
    sum(students$Performance_Category == "At Risk"), "\n")

# A for loop that processes each school separately and compares it
# with the overall pass rate
overall_rate <- round(mean(students$G3 >= 10) * 100, 1)
cat("\nOverall pass rate:", overall_rate, "%\n")
cat("Pass rate for each school (for loop over schools):\n")
for (s in levels(students$school)) {
  school_data <- students[students$school == s, ]
  rate <- round(mean(school_data$G3 >= 10) * 100, 1)
  if (rate >= overall_rate) {
    comment <- "at or above the overall pass rate"
  } else {
    comment <- "below the overall pass rate"
  }
  cat("School", s, ":", nrow(school_data), "students, pass rate", rate, "% ->", comment, "\n")
}

cat("\n", strrep("=", 70), "\nQUESTION 1 COMPLETED. Figures saved in:", out_dir, "\n", strrep("=", 70), "\n", sep = "")

