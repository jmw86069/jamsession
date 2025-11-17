# jamsession TODO

## 17nov2025

* COMPLETE. Add `grep_functions()` for consistency, and utility.

   * It should return "Path" in columns, and allow multiple
   matches for each "Name". Need way to specify which to use.

* COMPLETE. `jamsession_paths()`

   * Improve docs showing which `options()` are used, make it
   easier to use with `withr::with_options()` for example.
   * Consider `path.expand()` for shorthand paths.
   * Consider `with_jamsession_paths()` as drop-in?
   * Should most methods return jamsession_paths as attribute?
   So the paths used at the time are available as a "state"
   to be re-used as needed. Consider `"~/Projects/R-objects"`
   by another user, it would evaluate to a new path.
   Currently there is no clear indication what paths were used,
   except by the session calling the jamsession functions.
   No clear pattern to follow to store that information.

* Longer term: Debug occassional error using `refresh_functions()`
inside Rmd that uses caching, sometimes the package throws
an error "package not loaded" or similar, even when it should
be loaded in a previous chunk.

   * Error is resolved by loading `refresh_functions()` manually,
   at least for Rmd which is rendered `rmarkdown::render()`.
   * See `pkgload::is_dev_package()`.

## 14apr2025

* COMPLETE. Add missing dependencies: usethis, roxygen2, farver (?).
Verify if others are needed.

## 03apr2025

* `refresh_functions()`

   * DONE. `verbose=FALSE` should wrap `suppressMessages()` to silence
   `usethis::create_package()` or whatever step is using `message()`.
   * Consider option to use any file without the "_functions.R" suffix.

* Bonus points? `save_functions()`?

   * provide `character` vector of function names
   * all function sources are saved into an R file for editing.
   * If somehow the functions have help text, it can be converted
   to text with: `tools::Rd2txt(utils:::.getHelpFile(help("sd")))`,
   see `Rd2roxygen` package to convert to proper roxygen format.
   
   ```
   sd_rd <- utils:::.getHelpFile(help("sd"))
   # iterate each entry
   rdlinelist <- lapply(seq_along(sd_rd), function(i){
      paste0(paste0(tools:::as.character.Rd(sd_rd[i]), collapse=""), "\n")
   })
   rdlines <- unlist(strsplit(unlist(rdlinelist), "\n"))
   options(roxygen.comment = "#' ")
   roxy1 <- Rd2roxygen::create_roxygen(Rd2roxygen::parse_file(textConnection(rdlines)))
   roxy2 <- Rd2roxygen::create_roxygen(Rd2roxygen::parse_file(textConnection(unlist(rdlinelist))))
   identical(roxy1, roxy2)
   ```

* DEFER. Make paths by default also look in current working directory?

   * Consider option to enable working dir (somehow) so that
   working directory can be checked first when loading,
   and optionally used last when saving?

## 06jun2022

* Consider `grep_functions()` to be consistent with `grep_jamsessions()`
and `grep_objects()`.
* `grep_sessions()` should have argument `save_date` to be consistent
with `load_session()` which allows loading a specific date.


## 28oct2021 - R objects

The `save_object()` mechanism should allow saving multiple objects,
similar to saving an R session with a corresponding `"objectlist.txt"`
file with the objects saved.

New idea: optionally check objects for size before saving session?
If any objects are greater than 500MB it could exit with a warning.

* `max_objectsize=500*1024*1024`


## 22oct2021 - R sessions

General idea is to store the list of R objects in its own file
to help search for R objects within a stored R session.

* `save_jamsession()` - argument `save_objectlist=TRUE` enables saving
the list of Robjects using file extension defined by
`objectlist_suffix=".objectlist.txt"`.



## 05oct2021

* `save_jamsession()`

   - should allow underscore and not convert to hyphen/dash: `"-"`.
   - some method to decide whether to save environment objects,
   default TRUE currently.

* `prune_jamsessions()` - utility to delete old versions of jam session files.
* `prune_objects()` - utility to delete old versions of R object files.
* `grep_functions()` - utility to search functions_path and list available files.
Bonus points for listing functions defined? Probably easier to import.

* `refresh_functions_for_package()` - same as `refresh_functions()` except it
creates a folder with proper package structure that can be used in an R package.

Generally need a better mechanism to store a few R objects in a file.
Currently the R object names are concatenated to produce the output filename.



## usability

* `save_jamsession()` objects to exclude or include

   * exclude object names by pattern and/or vector of object names
   * include only a defined vector of object names
   
      * could be good for saving a minimal R session of "reference data"
      for a given analysis project
   
   * exclude object classes (e.g. environments)
   
      * for example: loaded a couple sessions into their own
      sub-environments. When saving the analysis session,
      the sub-environments probably do not need to be saved,
      preventing writing a bunch of un-necessary data to the
      RData file.
      * Bonus points for detecting and ignoring environments.


## jamsession functions

* check virtual package name for direct conflicts with
names of fully installed R packages

   - warn/stop if virtual package would conflict and thus override
   an installed R package

* New functions to round out the workflow:

   - `list_functions()` - list all stored functions
   - `grep_functions()` - grep list of functions for keywords


## deeper ideas into usefulness

* consider associating notes to an R session?

   * "useful R script(s) associated with this session"

* Is it possible to grep stored functions for the name
of a specific function?

   - `grep_function_name()` - search by function name
   
      * option to subset the files searched by providing grep
      string(s).
   
   - easiest method: Parse the `.R` file for specific pattern
   
      * This step makes assumptions that stored functions would
      follow a certain fixed pattern. However it should be fast
      across numerous files without having to source/load each file.
      * `"^[a-zA-Z0-9_]+[ ]*<-[ ]*function"` This pattern matches
      all function-compatible names starting on a new line that are
      immediately followed by `<- function` on the same line.

   - more involved: load all functions `.R` files as temporary packages,
   this process would be too slow for routine searches across all `.R` files.
   
   - most involved: process to "index" functions `.R` files:
   
      * Store the function details alongside each `.R` file, perhaps
      by running `jamba::jargs()` or storing `formals()` for each function.
      * In order to learn function details, each `.R` file would
      need to be loaded as a virtual R package in order to inspect
      the contents. This alternative runs this process only once
      per `.R` file.
      * Create a new index, or update an existing index when:
      
         - no index entry exists for that `.R` file
         - the index entry is older than the `.R` file
         - index entries can be stored in flat files, or central SQLite db


## jamsession objects

* Document the use of `object_notes_list` argument.

   - Consider returning object notes with `load_object()` as a
   proper R object for review.

* Mechanism to "describe" objects in an objects RData file.
If someone searches for R-objects RData files by name,
how would they know what's inside?

   - currently when multiple objects are stored in one RData
   file, the filename is the set of objects separated by
   hyphen `"-"`.
   - List RData contents, run `jamba::sdim()` to describe
   size and class of all objects
   - This step probably involves loading objects into a temporary
   environment then running `jamba::sdim()` on that environment
   - This step will be slow for very large RData files, but it
   will work.

* Possible to save `jamba::sdim()` in a separate index,
to prevent having to reload the full RData file to read its contents.

   - Is there any R method to read contents of an RData file without
   loading/parsing the entire RData file?
   
      * I have seen this type of function in several custom
      R packages, they all seem to load the full RData file.
      * On StackOverflow help threads, the only solution I recall
      seeing was to load the full RData file into a temporary
      environment then inspect contents there.



