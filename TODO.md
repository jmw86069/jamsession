# jamsession TODO

## temporary package for custom functions (DONE)

This workflow works so much better than the previous method,
it is possible the `source_functions()`, `load_functions()`
workflow will be retired altogether. In some cases, it might
be better to load functions outside the context of a custom package,
for example in rare cases loading into the local workspace.
(This is not a use case I current have, however.)

## New features

* Mechanism to "describe" an R object. So if someone searches
for an R-object, how would they know what's inside?
* Possibility of describing objects saved with a session,
for example `jamba::sdim()` or `jamba::ssdim()` on the
environment being saved.


## todo for functions

Note that functions are currently not versioned by date,
unlike sessions and objects. The reason is the R function
file is not dependent upon the user workspace.

* create `list_functions()`
* create `grep_functions()`

### New function

Return functions defined by a file, then allow searching
for a function name, within a set of one or more `"_functions.R"`
files.

* load function file into new tmp package
* list functions in that package
* unload that package
