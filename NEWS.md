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
