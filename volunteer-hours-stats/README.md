Volunteer hours statistics
==========================

Overview
--------

The tool `compute-hours-stats.sh` is for computing hours statistics using data
from an iCalendar.

Usage:

```
ical2text < cal.ics | grep ^2017 | ./compute-hours-stats.sh -ttext -s3
```

The output is a table showing names and hours in categories, similar to this:

```
        training   organizational    teaching
name1   5          6                 8
name2   2          0                 1
name3   6          2                 10
name4   7          3                 4
```

Calendar format / syntax
------------------------

To enable the hours statistics computation, there are some rules for the
calendar:

- Create the event in the correct calendar (e.g. not the private one)
- Put the category keyword at the beginning of the event's title, e.g. `orga
  First meeting`
- Put names of the attendees in the event's description field in this format:  
  `Vorname Nachname [tn] [<ggfAbweichendeStunden>]`  
  (`[...]` means that this is optional.)

Details
-------

To see how this script comes to it's results, you can change the output stage
with the `-s` option:

- `-s1` - creates one event for each single name in the event description
- `-s2` - summarizes the events by name and category summing up the hours
- `-s3` - arranges the summary in a table format (see output above) - this is
  the default

The components
--------------

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

Post-edits of the resulting table
---------------------------------

Post-edits in Vim:

```vim
" in decimal numbers:
:%s/\./,/g

" name must start upper-case
" \u -> first letter upper-case, \U ... \e -> all upper-case
" same for \l and \L
:%s/\v^(\S+) (\S+)/\u\1 \u\2/
```
