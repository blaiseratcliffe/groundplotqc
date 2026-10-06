# Declare missing-value sentinels

Builds the table of sentinel values: the codes a column holds when a
value is missing or doesn't apply. The engine has none built in.

## Usage

``` r
gpq_sentinels(numeric = NULL, character = NULL, date = NULL)
```

## Arguments

- numeric, character, date:

  `NULL`, or a named vector with names `missing` and `not_applicable`,
  for example `c(missing = -1, not_applicable = -9)`.

## Value

A data.table with columns `data_type` (the family), `role`, `value` (as
text), `allowed_in_pk` and `allowed_in_fk`.

## Details

Each row belongs to a family, not to one dictionary type: `numeric` rows
cover every type the type map gives R class integer or double; `date`
rows every type whose type-map row has a `date_format`; `character` rows
every other type of R class character. A type no row covers has no
sentinels, and the rules that need them record `not_run` for its
columns. A primary key never holds a sentinel; a foreign key may hold
the not-applicable one.

## Examples

``` r
gpq_sentinels(
  numeric = c(missing = -99, not_applicable = -88),
  character = c(missing = "?", not_applicable = "~"),
  date = c(missing = "?", not_applicable = "~")
)
#>    data_type           role  value allowed_in_pk allowed_in_fk
#>       <char>         <char> <char>        <lgcl>        <lgcl>
#> 1:   numeric        missing    -99         FALSE         FALSE
#> 2:   numeric not_applicable    -88         FALSE          TRUE
#> 3: character        missing      ?         FALSE         FALSE
#> 4: character not_applicable      ~         FALSE          TRUE
#> 5:      date        missing      ?         FALSE         FALSE
#> 6:      date not_applicable      ~         FALSE          TRUE
```
