ical2text
=========

Overview
--------

Convert iCalendar format (.ics) to plain text.
The output can be further processed at the command line, e.g.

```
# Display all meetings in December 2016
ical2text < calendar.ics | grep ^2016-12 | grep -i meeting | sort
```

Usage:

```
ical2text

Usage:
  ical2text [options]

Options:
  -f, --field-separator=STRING
    Field separator used to separate title, description, and location [default: @@].
  -l, --line-separator=STRING
    Line separator used to separate lines in description and location [default: ,,].
  -h, --help
    Print this help message.
```


Article: http://jakob.keramik-schoettl.de/blog/#sec-1-5

Build
-----

[Stack](https://www.haskellstack.org), Haskell's build tool, must be installed.

```
stack install docopt
stack install iCalendar
stack ghc ical2text.hs
```
