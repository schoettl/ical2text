ical2text
=========

Convert iCalendar format (.ics) to plain text.
The output can be further processed at the command line, e.g.

```
# Display all meetings in December 2016
ical2text < calendar.ics | grep ^2016-12 | grep -i meeting | sort
```

Article: http://jakob.keramik-schoettl.de/blog/#sec-1-5
