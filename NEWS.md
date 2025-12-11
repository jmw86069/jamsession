# version 0.0.7.900

## Bug fixes

* `save_object()` added missing argument 'prefer_local'.

# version 0.0.6.900

* Added dependencies: roxygen2, usethis, withr.

## new functions

* `grep_functions()` to search for available functions files, suitable
to use with `refresh_functions()`.
* `list_functions()` called by `grep_functions()`.
* `list_functions_exports()` will intuit the exported functions from
a specific 'functions' file, useful to see what is provided from a
functions R file. It only works with functions using roxygen2 style
`#' @exports`, in order to avoid having to parse the file as valid R code.

## changes to existing functions

* `list_jamsessions()` now returns sessions sorted by date then name.
* `load_session()`, `save_session()` now check whether functions exist,
`quartz.options()`, `X11.options()`, `windows.options()` before attempting
to set the title.

# version 0.0.5.900

## updates

* DESCRIPTION was updated to use jamba on CRAN and not Github.
Minor author update.
* Added LICENSE, LICENSE.md files, updated License in DESCRIPTION.

# version 0.0.4.900

## new functions

* `fileInfo()` moved from jamba for time being, since jamba removed it
prior to CRAN release.

## bug fixes

* Replaced `jamba::fileInfo()` with local `fileInfo()` due to jamba
change prior to its CRAN release.


# version 0.0.3.900

## changes to existing functions / bug fixes

* `refresh_functions()` was not matching exact start of the filename,
allowing suffix matches to the expected filename, for example `"DM"`
matched `"DM"` and `"JDM"`. It was updated to force match to the full
filename, which is the intended behavior.

# version 0.0.2.900

## changes to existing functions

* `save_jamsession()` new arguments to save a list of objects stored
into an RData file. Future idea is to enable searching for sessions
by object name. It may be useful for working with particularly large
R session files, sometimes several GB size even after compression.

   * `save_objectlist=TRUE` to enable saving a text file
   with a list of R objects saved in the RData session.
   * `objectlist_suffix=".objectlist.txt"` which defines the
   filename suffix to use for the output file.

* `save_jamsession()` now calls `clean_jamsession_name()` to prepare
the session name and convert to a file-friendly format for saving
the session as a file in RData format. This function also changes
the logic somewhat so that it allows underscore `"_"` and no longer
converts to hyphen `"-"`.
* `grep_jamsessions()` also uses `clean_jamsession_name()` in order
to process session names consistently, so the initial session name
can still be used successfully to find the saved session.


## new functions

* `clean_jamsession_name()` - this function takes a session name and
removes some unwanted characters and converts them to underscore `"_"`
by default. It does allow some custom modification of the pattern
matching, and for the replacement character.




# version 0.0.1.900

`refresh_functions()` was updated to include this entry
in the `DECRIPTION` file: `Roxygen=list(markdown=TRUE)`,
which enables markdown syntax in the function help text.
Verify with `print_pkg_description()`.

# version 0.0.0.900

## initial version

* Created initial package functions for sessions, objects, functions.
* Added helper function `show_session_versions()`
* Added helper function `print_pkg_description()`

This initial release already accomplished the major goal
of transitioning away from using `source()` and `sys.source()`
to load R functions, and instead creates a temporary R
package. Benefit is that R packages are handled with more
useful capabilities, such as `roxygen2` help docs.
