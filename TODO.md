# jamsession TODO

## 06jun2022

Consider `grep_functions()` to be consistent with `grep_jamsessions()`
and `grep_objects()`.


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



