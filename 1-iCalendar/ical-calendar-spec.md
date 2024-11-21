# iCal calendar language specification

The language of *iCal calendars* is defined by this grammar,
with some additional restrictions:

```
calendar    ::= BEGIN:VCALENDAR crlf
                calprop∗
                event∗
                END:VCALENDAR   crlf
calprop     ::= prodid | version
prodid      ::= PRODID: text  crlf
version     ::= VERSION:2.0   crlf
event       ::= BEGIN:VEVENT  crlf
                eventprop∗
                END:VEVENT    crlf
eventprop   ::= dtstamp | uid | dtstart | dtend | description | summary | location
dtstamp     ::= DTSTAMP:      datetime  crlf
uid         ::= UID:          text      crlf
dtstart     ::= DTSTART:      datetime  crlf
dtend       ::= DTEND:        datetime  crlf
description ::= DESCRIPTION:  text      crlf
summary     ::= SUMMARY:      text      crlf
location    ::= LOCATION:     text      crlf
```

Here `crlf` is a carriage return followed by line feed, represented in Haskell
as `"\r\n"`, and `text` is a string of characters.

In `text`, `crlf` may be present, but always followed by a space.
See `examples/multiline.ics` example, which has a `crlf` in the summary.

No extra whitespace is allowed anywhere in an event.

In the calendar header, both `prodid` and `version` are required and must appear exactly once.

`description`, `summary` and `location` are optional but must not appear more than once.
