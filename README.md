A few function to extend https://github.com/AccelerationNet/data-table.

The fun part of this is exemplified by select-rows, which allows you to write

```common-lisp
(select-rows my-data-table '(< (* 10000 family-size) income))
```

That will generated a function to test the rows against the predicate shown, compile it, save it, 
and then return a new data-table with only the rows that match that predicate.

Compute-column provides a way to change the values in a column, for example:

```common-lisp
(compute-column my-data-table 'bonus (and income (* .1 income)))
```

Other useful functions: remove-duplicate-rows, unique-values-of-column data-table-of-samples).
