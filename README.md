# click-lang

A language for writing rhythms.

## The Language

### Basic Operators

* The aim of the language is to be expressed in the way you would count a beat out loud.
* A bar consists of beats, represented using a dot `.`.
* Between any two beats there can be 4 possible subdivisions. These are represented by the subdivision operators `e`, `+` and `a`. (3 extra operators; 4 notes in total including the beat `.`)
* The subdivision operators must appear in the order above. Any other order will cause a compiler error.
* Skipping a beat can be achieved using the rest operator `,`.
    - Rest operators for smaller subdivisions are not necessary since each subdivision has its own operator. Simply omit a subdivision operator to skip that subdivision.
    - E.g. In `.+a.` the `e` operator is omitted, the first subdivision is skipped.

| Operator name   | Token | Value                                                                |
|-----------------|-------|----------------------------------------------------------------------|
| beat            | `.`   | Used to represent a beat in a bar (**one** ee and ah)                |
| 1st subdivision | `e`   | The note after the first possible subdivision (one **ee** and ah)    |
| 2nd subdivision | `+`   | The note after the first 2 possible subdivisions (one ee **and** ah) |
| 3rd subdivision | `a`   | The note after the first 3 possible subdivisions (one ee and **ah**) |
| rest            | `,`   | A rest lasting one beat                                              |

### Bars

* A bar is represented by surrounding an expression with forward slashes `/{expression}/`.
    - One bar: `/{expression}/`
    - Two bars: `/{expression 1}/{expression 2}/` (Note that only one slash is needed between bars)
* If a program consists of an expression without forward slashes, this will be interpreted as one bar.

### Time Signature

* The number of beats in a bar can be inferred from the expression within it. For instance, the input `.+.+.+.+` would produce a bar of 4/4, because it contains 4 beats.
* The value of a beat is 4 by default. Other values must be stated explicitly by prefixing a bar with the desired value followed by a colon `:`. For example, the input `8:6.` would produce a measure of 6/8.

### Expressions

* An expression is a unit of code which independently makes grammatical sense.
* The beat `.` and rest `,` operators are expressions.
* The subdivision operators are not expressions, because their validity depends on their position relative to a beat/rest and the other subdivision operators.
* A bar or collection of bars is an expression.
* An expression can be surrounded with parentheses `({expression})`. This alone will not affect its behavior.

### Repitition

* An expression can be repeated `n` times by prepending it with `n`.
  - `n.` - `n` beats
  - `n({expression})` - `n` repititions of some surrounded expression
  - `n/{expression}/` - `n` repititions of a bar

### Tempo

* The language does not support specifying the absolute tempo.
* Since entire programs can be scaled to different tempos, compilers should be designed to accept a starting tempo alongside the code.
* Relative changes in tempo can be specified in BPM by placing an integer inside angled brackets. E.g.:
    - `<20>` - increase tempo by 20BPM
    - `<-50>` - decrease tempo by 50BPM
* These tempo changes can be placed anywhere in the code.

## Examples

| Code snippet          | Represents                                                                     |
|-----------------------|--------------------------------------------------------------------------------|
| `....` or `4.`        | A simple 4/4 crotchet beat                                                     |
| `.+.+.+.+` or `4(.+)` | 4/4: "one and two and three and four and"                                      |
| `.e+a..+.`            | 4/4: "one-ee and-a two, three and four"                                        |
| `8:7.`                | A simple bar of 7/8                                                            |
| `/8:4.+../3.+./`      | A bar of 6/8 followed by a bar of 4/4                                          |
| `2/.e+a..+./`         | Same bar twice                                                                 |
| `.a.,+`               | 4/4: One, ah-two, and                                                          |
| `/4./<20>/4./`        | A bar of 4/4, followed by an increase in tempo, followed by another bar of 4/4 |
