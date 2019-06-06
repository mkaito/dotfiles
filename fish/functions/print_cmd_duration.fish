function print_cmd_duration -a threshold -d "Prints a nicely formatted version of $CMD_DURATION if over $threshold"

  # Set a default threshold of 1s
  if not set -q threshold
    set threshold 1000
  end

  if test \( -n $CMD_DURATION \) -a \( $CMD_DURATION -ge $threshold \)
    set -l dur_seconds 0
    set -l dur_minutes 0
    set -l dur_hours 0
    set -l dur_days 0
    set -l dur_remainder $CMD_DURATION
    set -l dur_string

    set -l threshold_days 86400000
    set -l threshold_hours 3600000
    set -l threshold_minutes 60000
    set -l threshold_seconds 1000

    # Duration is in milliseconds

    # It's probably unnecessary to format durations bigger than days in duration.
    # While a process might possibly run for longer than, say, a week, I think
    # it's fine to display even longer durations such as that in "days", since
    # it's easy to do some head math with "days" as a unit.

    # Calculate days
    if test $dur_remainder -ge $threshold_days
      set dur_days (math "$dur_remainder / $threshold_days")
      set dur_remainder (math "$dur_remainder % $threshold_days")
    end

    # Calculate hours
    if test $dur_remainder -ge $threshold_hours
      set dur_hours (math "$dur_remainder / $threshold_hours")
      set dur_remainder (math "$dur_remainder % $threshold_hours")
    end

    # Calculate minutes
    if test $dur_remainder -ge $threshold_minutes
      set dur_minutes (math "$dur_remainder / $threshold_minutes")
      set dur_remainder (math "$dur_remainder % $threshold_minutes")
    end

    # Calculate seconds
    if test $dur_remainder -ge $threshold_seconds
      set dur_seconds (math "$dur_remainder / $threshold_seconds")
      set dur_remainder (math "$dur_remainder % $threshold_seconds")
    end

    set_color $fish_color_error
    echo -n " "

    # Append days to output
    if test $dur_days -gt 0
      echo -n $dur_days"d"
    end

    # Append hours to output
    if test $dur_hours -gt 0
      echo -n $dur_hours"h"
    end

    # Append minutes to output
    if test $dur_minutes -gt 0
      echo -n $dur_minutes"m"
    end

    # Append seconds to output
    if test $dur_seconds -gt 0
      echo -n $dur_seconds"s"
    end

    set_color normal
  end
end
