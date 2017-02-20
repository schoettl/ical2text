Volunteer hours statistics
==========================

These tools must be used in a pipe, e.g.

```
ical2text < calendar.ics \
  | awk -f one-event-per-description-line.awk \
  | awk -f parse-names.awk \
  | awk -f group-by-name-and-category.awk \
  | awk -f stat2table.awk
```

1. Convert an iCalendar to text.
2. Convert the text format to a long form by duplicating events
   so that each has only one line of the original description field.
3. Parse the lines of the original description field as names; format is:
   `firstName lastName [differingHours] [tagModifier]`
   The `tagModifier` (e.g. "tn" for "Teilnehmer") changes the event tag using
   special rules.
4. Group adjacent events by name and category summing up the hours. Input must be
   sorted accordingly. (SQL `SUM(hours)` with `GROUP BY name, category`.)
5. Convert the long format to a table (names vs. categories). Input must be sorted
   accordingly.

The script `compute-hours-stats.sh` simplifies the pipe construction for common
cases. Check out the `-h` option.
