# Grab Bag

A simple but somewhat optimized pool (bag) for objects.

You can then filter (rummage) through the bag based on a predicate

### Notes

These pools are only suitable for objects whos equality function is #'eq

When rummaging the predicate provided **must** return a boolean. Returning any other kind of value (whatever it's truthyness) is invalid and the resulting behaviour is undefined.

### Future work

- cache dirty shouldnt mean realloc
