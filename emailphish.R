
install_if_missing <- function(packages) {
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

packages <- c(
  "dplyr",        # Data manipulation
  "tidytext",     # Tokenization
  "tm",           # Text mining
  "textclean",    # Text cleaning
  "stringr",      # String handling
  "ggplot2",      # Visualization
  "caret"         # Train-test split (if needed)
)
install_if_missing(packages)

# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------
train_email <- read.csv("train_email.csv", stringsAsFactors = FALSE)
test_email <- read.csv("test_email.csv", stringsAsFactors = FALSE)

cat("✅ Data loaded successfully!\n")
cat("Train rows:", nrow(train_email), "| Test rows:", nrow(test_email), "\n\n")

# ------------------------------------------------------------
# 3. CHECK FOR MISSING DATA
# ------------------------------------------------------------
cat("Missing values in train:\n")
print(colSums(is.na(train_email)))
cat("\nMissing values in test:\n")
print(colSums(is.na(test_email)))

# ------------------------------------------------------------
# 4. BASIC TEXT CLEANING FUNCTION
# ------------------------------------------------------------
clean_text <- function(text) {
  text <- tolower(text)                     # Lowercase
  text <- replace_contraction(text)         # Replace contractions (e.g., don't → do not)
  text <- replace_number(text)              # Replace numbers with text
  text <- replace_symbol(text)              # Remove symbols
  text <- str_replace_all(text, "http\\S+|www\\S+", "")  # Remove URLs
  text <- removePunctuation(text)           # Remove punctuation
  text <- removeNumbers(text)               # Remove numbers
  text <- stripWhitespace(text)             # Remove extra spaces
  text <- removeWords(text, stopwords("en"))# Remove stopwords
  text <- trimws(text)                      # Trim leading/trailing spaces
  return(text)
}

# ------------------------------------------------------------
# 5. APPLY TEXT CLEANING
# ------------------------------------------------------------
train_email$Clean_Text <- sapply(train_email$Email.Text, clean_text)
test_email$Clean_Text <- sapply(test_email$Email.Text, clean_text)

# ------------------------------------------------------------
# 6. CONVERT TARGET VARIABLE TO FACTOR
# ------------------------------------------------------------
train_email$Email.Type <- factor(train_email$Email.Type, 
                                 levels = c("Safe Email", "Phishing Email"))
test_email$Email.Type <- factor(test_email$Email.Type, 
                                levels = c("Safe Email", "Phishing Email"))

# ------------------------------------------------------------
# 7. TOKENIZATION & WORD FREQUENCY VISUALIZATION
# ------------------------------------------------------------
train_tokens <- train_email %>%
  unnest_tokens(word, Clean_Text) %>%
  count(Email.Type, word, sort = TRUE)

# Show top 10 frequent words for both classes
library(gridExtra)
top_safe <- train_tokens %>% filter(Email.Type == "Safe Email") %>% head(10)
top_phish <- train_tokens %>% filter(Email.Type == "Phishing Email") %>% head(10)

p1 <- ggplot(top_safe, aes(x = reorder(word, n), y = n, fill = Email.Type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + ggtitle("Top Words - Safe Emails") + theme_minimal()

p2 <- ggplot(top_phish, aes(x = reorder(word, n), y = n, fill = Email.Type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + ggtitle("Top Words - Phishing Emails") + theme_minimal()

gridExtra::grid.arrange(p1, p2, ncol = 2)

# ------------------------------------------------------------
# 8. FINAL STRUCTURE & SAVE CLEAN DATA
# ------------------------------------------------------------
cat("\n✅ Cleaned Data Structure:\n")
str(train_email)

write.csv(train_email, "C:/Users/uv488/Downloads/preprocessed_train_email.csv", row.names = FALSE)
write.csv(test_email, "C:/Users/uv488/Downloads/preprocessed_test_email.csv", row.names = FALSE)

cat("\n✅ Email preprocessing completed successfully!")