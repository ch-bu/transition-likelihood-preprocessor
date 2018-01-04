#############################################################
# Skript zur Berechnung von Übergangswahrscheinlichkeiten
#############################################################

# Paket laden
library(tidyverse)

# Read excel file
# Nimmt an, dass die csv mit Semicolons getrennt ist (;)
# TODO: Hier den richtigen Pfad für die Datei einfügen
# TODO: Sicher stellen, dass die Spalte mit den Sequenzen 'sequenzen' heißt. 
data <- read_csv2("sequenzen.csv")

# Init empty list
sequences <- list()

# Loop over every entry in sequenzen variable
for (sequence_key in seq_along(data$sequenzen)) {
  # Do not loop over last key
  if (sequence_key != nrow(data)) {
    # Store the string of the current sequence in variable
    current_sequence = paste(data$sequenzen[sequence_key], data$sequenzen[sequence_key + 1], sep = ' - ')
    
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

# Beautify data
sequences_processed$sequences <- rownames(sequences_processed)

# Change name of count variable and reorder them
sequences_processed <- sequences_processed %>%
  rename(count = "unlist(sequences)") %>%
  select(sequences, count)

# Write data to disc
write.csv(sequences_processed, "sequences_processed.csv", row.names = FALSE)
