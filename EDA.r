# INSTALL required packages
install.packages("vroom")
install.packages("gridExtra")
install.packages("tm")
install.packages("wordcloud")

#LOADING PACKAGES
library(dplyr)
library(readr)
library(vroom)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(tm)
library(wordcloud)
library(corrplot)
library(stringr)


col_types <- cols(
  Patient_ID = col_double(),
  Patient_Age = col_integer()
)

# Read the data with specified column types
data_entry <- vroom("F:\\DAPM\\PROJECT", col_types = col_types)
colnames(data_entry) <- gsub(" ", "_", colnames(data_entry))
unique_patient_ids <- unique(data_entry$Patient_ID)  # Get a list of unique patient IDs
shuffled_patient_ids <- sample(unique_patient_ids) # Shuffle the patient IDs randomly
subset_data <- data.frame()  # Initialize an empty data frame for the subset
og_data <- data_entry

# Loop through the shuffled patient IDs to add data points to the subset
for (patient_id in shuffled_patient_ids) {
  subset_data <- bind_rows(subset_data, data_entry %>% filter(Patient_ID == patient_id))
  
  # Break the loop when the subset size reaches 60,000
  if (nrow(subset_data) >= 60000) {
    break
  }
}

nrow(subset_data)

#NUMBER AND NAME OF UNIQUE DISEASES
split_labels <- unlist(strsplit(data_entry$Finding_Labels, "\\|")) #Extract label and tokenize by delimiter 
num_unique_labels <- length(unique(split_labels))    # Find the number of unique labels         

cat("Number of unique labels in subset_data:", num_unique_labels, "\n")
cat("Unique labels in subset_data:", unique(split_labels), "\n")

#DISTRIBUTION OF DISEASES
label_data <- data.frame(Label = split_labels)

# Create a bar chart of the label distribution
label_plot <- ggplot(label_data, aes(x = Label)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels for readability
  labs(x = "Labels", y = "Frequency", title = "Distribution of Labels")

# Display the bar chart
print(label_plot)

#FILTERING DISEASES WITH AT LEAST 5000 DATAPOINTS
label_counts <- data.frame(Label = split_labels) %>%
  group_by(Label) %>%
  summarize(Count = n()) %>%
  filter(Count > 5000) %>%
  arrange(desc(Count))

# Create a bar chart
ggplot(data = label_counts, aes(x = reorder(Label, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Labels", y = "Count", title = "Distribution of Labels (Count > 5000)")

#Create a histogram for the distribution of age
age_histogram <- ggplot(data_entry, aes(x = Patient_Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Distribution of Patient Age") +
  theme_minimal() +
  xlim(0, 100)  # Set the x-axis limit from 0 to 100


# Create a histogram for the distribution of gender
gender_histogram <- ggplot(data_entry, aes(x = Patient_Gender)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Gender", y = "Frequency", title = "Distribution of Patient Gender") +
  theme_minimal()

# Display both histograms side by side
grid.arrange(age_histogram, gender_histogram, ncol = 2)                         








# Create a copy of the data_entry dataset
base_df <- data_entry

# Extract the patient ID and follow-up number from Image_Index
base_df$Patient_ID <- sub("_.*", "", base_df$Image_Index)

# Find the number of non-unique patient IDs
non_unique_patient_ids <- base_df %>% 
  group_by(Patient_ID) %>% 
  filter(n() > 1) %>%
  distinct(Patient_ID) %>%
  nrow()

cat("Number of non-unique patient IDs:", non_unique_patient_ids, "\n")

# Filter for non-unique patient IDs and split the Finding_Labels
non_unique_patients_data <- base_df %>% 
  filter(Patient_ID %in% non_unique_patient_ids) %>%
  select(Patient_ID, Finding_Labels)

split_labels <- unlist(strsplit(data_entry$Finding_Labels, "\\|"))

# Create a data frame with label counts
label_counts <- data.frame(Label = split_labels) %>%
  group_by(Label) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Print the data frame containing unique labels and their counts
print(label_counts)


data_entry$Patient_ID <- sub("_.*", "", data_entry$Image_Index)

# Find non-unique patient IDs
non_unique_patient_ids <- data_entry %>%
  group_by(Patient_ID) %>%
  filter(n() > 2) %>%
  distinct(Patient_ID)

# Filter the dataset for patients with non-unique IDs
non_unique_patient_data <- data_entry %>%
  filter(Patient_ID %in% non_unique_patient_ids$Patient_ID)

# Split the labels by the pipe character (|) and unlist them
split_labels <- unlist(strsplit(non_unique_patient_data$Finding_Labels, "\\|"))

# Create a data frame with label counts
label_counts <- data.frame(Label = split_labels) %>%
  group_by(Label) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Print the data frame containing unique labels and their counts for non-unique patient IDs
print(label_counts)


numeric_columns <- data_entry %>% select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_columns)

# Print the correlation matrix
print(correlation_matrix)
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)



data_entry$Patient_ID <- sub("_.*", "", data_entry$Image_Index)

# Find patients with multiple images
patients_with_multiple_images <- data_entry %>%
  group_by(Patient_ID) %>%
  filter(n() > 1) %>%
  distinct(Patient_ID)

# Filter the dataset for patients with multiple images
patients_data <- data_entry %>%
  filter(Patient_ID %in% patients_with_multiple_images$Patient_ID)

# Split the labels by the pipe character (|) and unlist them
split_labels <- unlist(strsplit(patients_data$Finding_Labels, "\\|"))

# Create a data frame with label counts
label_counts <- data.frame(Label = split_labels) %>%
  group_by(Label) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

# Visualize the disease label distribution for patients with multiple images
ggplot(data = label_counts, aes(x = reorder(Label, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Labels", y = "Count", title = "Disease Label Distribution for Patients with Multiple Images")



data_entry <- data_entry %>%
  mutate(Num_Labels = str_count(Finding_Labels, "\\|") + 1)

# Find rows with multiple labels
rows_with_multiple_labels <- sum(data_entry$Num_Labels > 1)

# Print the number of rows with multiple labels
cat("Number of rows with multiple labels in the Finding_Labels column:", rows_with_multiple_labels, "\n")





