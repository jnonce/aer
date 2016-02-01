# AER

Scan JSON for subpieces of data.

## Syntax

`aer [OPTIONS] EXPRESSION [FILE]`

The expression scans the JSON as directed:

* Named part: `demo`
* Array index: `7`
* Named part followed by an array index: `demo/7`
* Any child value: `*`
* Any descendant value: `...`
* Filter out anything other than _terminal_ values (string, number, bool): `@`
* Either of two named parts: `(test,demo)`

## Implementation

This project was built to explore JSON serialization and parser combinators in Haskell.
