#############################################################
# Skript zur Berechnung von Übergangswahrscheinlichkeiten
#############################################################

# Paket laden
library(tidyverse)

# Read excel file
# TODO: Nimmt an, dass die csv mit Semicolons getrennt ist (;)
# TODO: Hier den richtigen Pfad für die Datei einfügen (hier: sequences.csv)
# TODO: Sicher stellen, dass die Spalte mit den Sequenzen 'sequenzen' heißt
# TODO: Sicher stellen, dass die Spalte mit den probanden 'participant' heißt
data <- read_delim("sequences.csv", delim = ";")

# Group data by participants
grouped_data <- data %>%
  group_by(participant) %>%
  nest()


process_transitions <- function(participant_dataframe) {
  # Computes the sum of all transitions within the data frame
  #
  # Args:
  #   participant_dataframe: The sequences of a particular participant
  # 
  # Returns:
  #   A data frame of the accumulated transitions of that participant
  
  # Init empty list
  sequences <- list()
  
  # Loop over every entry in sequenzen variable
  for (sequence_key in seq_along(participant_dataframe$sequenzen)) {
    # Do not loop over last key
    if (sequence_key != nrow(participant_dataframe)) {
      # Store the string of the current sequence in variable
      current_sequence = paste(participant_dataframe$sequenzen[sequence_key], 
                               participant_dataframe$sequenzen[sequence_key + 1], sep = ' - ')
      
      # Check if list key exists for the current sequence
      if (is.null(sequences[[current_sequence]])) {
        # Add missing key to list
        sequences[[current_sequence]] = 1
      } else {
        # Increment existing list item by 1
        sequences[[current_sequence]] = sequences[[current_sequence]] + 1
      }
    }
  }
  
  # Convert list to data frame
  sequences_processed <- as.data.frame(unlist(sequences))
  
  # Get rownames of transitions
  sequences_processed$sequences <- rownames(sequences_processed)
  
  # Change name of count variable and reorder them
  sequences_processed <- sequences_processed %>%
    rename(count = "unlist(sequences)") %>%
    select(sequences, count)
  
  # Remove row names 
  rownames(sequences_processed) <- c()
  
  return(sequences_processed)
}

# Process data for all participants
processed_data <- grouped_data %>%
  # Create new variable which contains processed data frame
  mutate(
    processed_data = data %>% map(process_transitions)
  ) %>%
  # Remove raw data
  select(-data) %>%
  # Unnest for export
  unnest()
  

# Write data to disc
write.csv(processed_data, "sequences_processed.csv", row.names = FALSE)
