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
