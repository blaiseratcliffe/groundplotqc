# Options for groundplotqc

groundplotqc functions read their settings from function arguments, the
rule set and R options. This page lists the R options.

## Option names

Each option is named `groundplotqc.<setting>`, with the setting's name
in lower snake case after the dot.

## Where a setting's value comes from

A function takes each setting from the first of these that gives a
value:

1.  the function's argument;

2.  the rule set;

3.  the option, set with
    [`options()`](https://rdrr.io/r/base/options.html);

4.  the built-in default.

## Options

None yet. Each option is added to this page by the version that brings
the setting it controls.
