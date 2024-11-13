#!/bin/bash

# Set the folder to the current directory if not provided as an argument
folder="${1:-.}"

# Check if ffprobe is installed
if ! command -v ffprobe &>/dev/null; then
  echo "ffprobe is required but not installed. Please install ffmpeg."
  exit 1
fi

LC_NUMERIC=C

# Initialize total duration (in seconds)
total_duration=0

# Loop through all video files in the folder
for file in "$folder"/*; do
  if [[ -f "$file" ]]; then
    # Get the duration of the video using ffprobe
    duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$file")

    # Check if the duration was successfully retrieved
    if [[ -z "$duration" ]]; then
      echo "Warning: Could not retrieve duration for $file. Skipping..."
      continue
    fi

    # Add to total duration (ensure duration is rounded and not null)
    total_duration=$(echo "$total_duration + $duration" | bc)
  fi
done

# Round the total duration to remove decimal places
total_duration=$(printf "%.0f" "$total_duration")

# Convert total duration from seconds to hours, minutes, and seconds
hours=$(echo "$total_duration/3600" | bc)
minutes=$(echo "($total_duration%3600)/60" | bc)
seconds=$(echo "$total_duration%60" | bc)

# Display the total runtime
printf "Total runtime: %02d:%02d:%02d\n" "$hours" "$minutes" "$seconds"
