############################################################
# DATA 104 : Introduction to R Programming for Data Analysis
# Mid-Semester Assessment
#
# Name       : JEEVAN C
# Student ID : 0626576
# Date       : 21-10-2026
#
# Question 2 : Text Preprocessing, Sentiment Analysis and Data Reshaping
#
# HOW TO RUN
#   1. Open this file in RStudio and click "Source" (or run it
#      from top to bottom).
#   2. A file-picker window opens: choose  amazon_cells_labelled.txt
#   3. All graphs are shown in the Plots pane AND saved as PNG
#      files in a folder called "R_Q2_Outputs" inside your
#      current working directory.
#
# DATA SOURCE
#   Kotzias, D. et al. (2015). From Group to Individual Labels
#       using Deep Features. UCI Machine Learning Repository,
#       "Sentiment Labelled Sentences" dataset.
#       https://archive.ics.uci.edu/dataset/331/sentiment+labelled+sentences
############################################################


############################################################
# 0. SETUP : packages, folders, theme, helper functions
############################################################
rm(list = ls())

required_pkgs <- c("dplyr", "tidyr", "ggplot2", "tm")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tm)
})

# Folder where every figure is saved
out_dir <- file.path(getwd(), "R_Q2_Outputs")
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
# QUESTION 2 : TEXT PREPROCESSING, SENTIMENT ANALYSIS
#              AND DATA RESHAPING
############################################################

############################################################
# Q2 (a) Data Import and Inspection
############################################################
section("Q2 (a) DATA IMPORT AND INSPECTION")

amazon_path <- Sys.getenv("AMAZON_TXT", unset = "")
if (!nzchar(amazon_path)) {
  cat("Please select amazon_cells_labelled.txt in the window that opens...\n")
  amazon_path <- file.choose()
}

# Each line is:  sentence <TAB> label   (1 = positive, 0 = negative)
# quote = "" and comment.char = "" stop R from treating ' " and # inside
# sentences as special characters.
reviews <- read.delim(amazon_path, header = FALSE, sep = "\t", quote = "",
                      comment.char = "", stringsAsFactors = FALSE,
                      col.names = c("Review", "Label"), encoding = "UTF-8")

cat("\nFirst six records:\n")
print(head(reviews, 6))
cat("\nDimensions (rows, columns):", dim(reviews), "\n")
cat("\nStructure:\n")
str(reviews)
cat("\nColumn names:", paste(names(reviews), collapse = ", "), "\n")

# Quick quality checks
cat("\nMissing values:", sum(is.na(reviews)), "\n")
cat("Empty reviews :", sum(trimws(reviews$Review) == ""), "\n")
cat("Duplicate review sentences:", sum(duplicated(reviews$Review)), "(kept, because the same short sentence can be written by different customers)\n")
cat("Label values found:", paste(sort(unique(reviews$Label)), collapse = ", "), "\n")


############################################################
# Q2 (b) Text Preprocessing
############################################################
section("Q2 (b) TEXT PREPROCESSING")

# Build a corpus (tm package) so we can use tm_map()
corpus <- VCorpus(VectorSource(reviews$Review))

# 1. Lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# 2. Remove punctuation.
#    Apostrophes are deleted first (don't -> dont) and every other
#    punctuation mark becomes a space, so "great.phone" does not get
#    glued into "greatphone".
remove_apostrophes <- content_transformer(function(x) gsub("['\u2019]", "", x))
punct_to_space    <- content_transformer(function(x) gsub("[[:punct:]]+", " ", x))
corpus <- tm_map(corpus, remove_apostrophes)
corpus <- tm_map(corpus, punct_to_space)

# 3. Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# 4. Remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)

reviews$Clean_Text <- trimws(sapply(corpus, as.character))

cat("Before and after cleaning (first 6 reviews):\n")
for (i in 1:6) {
  cat("\nOriginal :", reviews$Review[i], "\n")
  cat("Cleaned  :", reviews$Clean_Text[i], "\n")
}

# 5. Tokenize: split every sentence into single words
tokens <- strsplit(reviews$Clean_Text, " ")
cat("\nTokens of review 1:", paste(tokens[[1]], collapse = " | "), "\n")
cat("Total tokens before stop-word removal:", length(unlist(tokens)), "\n")

# 6. Remove stop words with the tm stop-word list.
#    The list is also used without apostrophes (don't -> dont) because
#    apostrophes were already removed in step 2.
stop_list <- unique(c(stopwords("english"), gsub("'", "", stopwords("english"))))
tokens_clean <- lapply(tokens, function(w) w[!(w %in% stop_list) & nchar(w) > 1])
cat("Total tokens after stop-word removal :", length(unlist(tokens_clean)), "\n")
cat("Tokens of review 1 after cleaning     :", paste(tokens_clean[[1]], collapse = " | "), "\n")

reviews$Clean_Tokens <- sapply(tokens_clean, paste, collapse = " ")
reviews$Word_Count   <- lengths(tokens_clean)


############################################################
# Q2 (c) Control Statements : for loop and if...else
############################################################
section("Q2 (c) CONTROL STATEMENTS")

reviews$Sentiment <- NA_character_
for (i in seq_len(nrow(reviews))) {
  if (reviews$Label[i] == 1) {
    reviews$Sentiment[i] <- "Positive"
  } else {
    reviews$Sentiment[i] <- "Negative"
  }
}
reviews$Sentiment <- factor(reviews$Sentiment, levels = c("Positive", "Negative"))

cat("Classification of the first 10 reviews:\n")
for (i in 1:10) {
  cat("Review", i, "| label =", reviews$Label[i], "->", as.character(reviews$Sentiment[i]), "|",
      substr(reviews$Review[i], 1, 55), "\n")
}

# The loop result must agree with the supplied label
cat("\nAll", nrow(reviews), "reviews processed by the loop.\n")
cat("Loop result matches the supplied label for every review:",
    all((reviews$Sentiment == "Positive") == (reviews$Label == 1)), "\n")


############################################################
# Q2 (d) EDA and Visualization
############################################################
section("Q2 (d) EDA AND VISUALIZATION")

# Number of positive and negative reviews
sentiment_table <- table(reviews$Sentiment)
cat("Number of reviews by sentiment:\n")
print(sentiment_table)
cat("\nPercentage:\n")
print(round(prop.table(sentiment_table) * 100, 1))

# Review length
cat("\nWords per review after preprocessing:\n")
print(summary(reviews$Word_Count))

# Most frequent words
word_freq <- sort(table(unlist(tokens_clean)), decreasing = TRUE)
top_words <- data.frame(Word = names(word_freq), Frequency = as.integer(word_freq))
cat("\nNumber of unique words:", nrow(top_words), "\n")
cat("\nTop 15 most frequent words:\n")
print(head(top_words, 15))

# ---- Figure 1: Sentiment distribution ----
sent_df <- as.data.frame(sentiment_table)
names(sent_df) <- c("Sentiment", "Reviews")
sent_df$Percent <- round(sent_df$Reviews / sum(sent_df$Reviews) * 100, 1)

fig1 <- ggplot(sent_df, aes(x = Sentiment, y = Reviews, fill = Sentiment)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(Reviews, " (", Percent, "%)")), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Positive" = teal, "Negative" = orange)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Sentiment Distribution of Amazon Reviews",
       subtitle = "UCI Sentiment Labelled Sentences, amazon_cells_labelled.txt",
       x = "Sentiment", y = "Number of reviews") +
  theme_report + theme(legend.position = "none")
save_plot(fig1, "Q2_Fig1_Sentiment_Distribution.png", h = 4.6)

# ---- Figure 2: Top 15 words ----
top15 <- head(top_words, 15)
fig2 <- ggplot(top15, aes(x = reorder(Word, Frequency), y = Frequency)) +
  geom_col(fill = navy, width = 0.7) +
  geom_text(aes(label = Frequency), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "15 Most Frequent Words After Preprocessing",
       x = "Word", y = "Frequency") +
  theme_report
save_plot(fig2, "Q2_Fig2_Top_Words.png", h = 5.2)

# ---- Figure 3: Review length by sentiment ----
fig3 <- ggplot(reviews, aes(x = Sentiment, y = Word_Count, fill = Sentiment)) +
  geom_boxplot(alpha = 0.85, width = 0.5, outlier.colour = grey) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white") +
  scale_fill_manual(values = c("Positive" = teal, "Negative" = orange)) +
  labs(title = "Review Length by Sentiment",
       subtitle = "Words per review after stop-word removal (white diamond = mean)",
       x = "Sentiment", y = "Number of words") +
  theme_report + theme(legend.position = "none")
save_plot(fig3, "Q2_Fig3_Review_Length.png", h = 4.6)


############################################################
# Q2 (e) Data Manipulation and Reshaping (dplyr + tidyr)
############################################################
section("Q2 (e) DATA MANIPULATION AND RESHAPING")

# dplyr : group reviews by sentiment and count them
sentiment_summary <- reviews %>%
  group_by(Sentiment) %>%
  summarize(
    Reviews       = n(),
    Avg_Words     = round(mean(Word_Count), 2),
    Avg_Characters = round(mean(nchar(Review)), 1),
    .groups = "drop"
  )
cat("Reviews grouped by sentiment (dplyr):\n")
print(as.data.frame(sentiment_summary))

# tidyr : WIDE -> LONG
# Currently one row per sentiment and one column per measure (wide).
summary_long <- sentiment_summary %>%
  pivot_longer(cols = -Sentiment, names_to = "Measure", values_to = "Value")
cat("\nSummary table in LONG format (pivot_longer):\n")
print(as.data.frame(summary_long))

# tidyr : LONG -> WIDE
summary_wide <- summary_long %>%
  pivot_wider(names_from = Sentiment, values_from = Value)
cat("\nSame table with sentiments as columns (pivot_wider):\n")
print(as.data.frame(summary_wide))

# Word counts per sentiment : wide table, then long table for plotting
tokens_df <- data.frame(
  Word      = unlist(tokens_clean),
  Sentiment = rep(reviews$Sentiment, lengths(tokens_clean))
)

key_words <- tokens_df %>%
  count(Sentiment, Word) %>%
  group_by(Sentiment) %>%
  slice_max(n, n = 8, with_ties = FALSE) %>%
  pull(Word) %>%
  unique()

words_wide <- tokens_df %>%
  filter(Word %in% key_words) %>%
  count(Word, Sentiment) %>%
  pivot_wider(names_from = Sentiment, values_from = n, values_fill = 0) %>%
  arrange(desc(Positive + Negative))
cat("\nFrequent words per sentiment (WIDE format):\n")
print(as.data.frame(words_wide))

words_long <- words_wide %>%
  pivot_longer(cols = c(Positive, Negative), names_to = "Sentiment", values_to = "Count") %>%
  mutate(Sentiment = factor(Sentiment, levels = c("Positive", "Negative")))
cat("\nSame table in LONG format (ready for ggplot2):\n")
print(head(as.data.frame(words_long), 8))

# ---- Figure 4: Frequent words by sentiment (uses the long table) ----
fig4 <- ggplot(words_long, aes(x = reorder(Word, Count, FUN = sum), y = Count, fill = Sentiment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = teal, "Negative" = orange)) +
  labs(title = "Frequent Words in Positive and Negative Reviews",
       subtitle = "Drawn from the long-format table made with pivot_longer()",
       x = "Word", y = "Frequency") +
  theme_report
save_plot(fig4, "Q2_Fig4_Words_By_Sentiment.png", h = 5.8)

cat("\nPurpose of the transformation:\n")
cat("The wide table is easy for people to read (one row per word, one column per sentiment).\n")
cat("ggplot2 needs the long format (one row per word-sentiment pair) so that 'Sentiment' can\n")
cat("be mapped to fill colour. pivot_longer() and pivot_wider() switch between the two without\n")
cat("losing any information.\n")

cat("\n", strrep("=", 70), "\nQUESTION 2 COMPLETED. Figures saved in:", out_dir, "\n", strrep("=", 70), "\n", sep = "")

