# Extract clone size values from metadata
clone_sizes <- full_metadata$clone_size_gd

# Remove missing (NA) values
clone_sizes <- clone_sizes[!is.na(clone_sizes)]

# Exclude singleton clones (clone size = 1)
clone_sizes <- clone_sizes[clone_sizes != 1]

# Sort clone sizes in ascending order
clone_sizes <- sort(clone_sizes)

# Calculate deciles (quantiles at every 5% interval)
deciles <- quantile(clone_sizes, probs = seq(0, 1, 0.05), na.rm = TRUE)

deciles


# clone size = 1 : Singletone
# clone size = 2-4: Q1
# clone size = 5-8: Q2
# clone size = 9-33: Q3
# clone size = 34-75: Q4