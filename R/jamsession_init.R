
#' jamsession file paths
#'
#' jamsession file paths
#'
#' This function retrieves the file path for each jamsession type:
#'
#' 1. `"sessions"` - the folder where R sessions are saved,
#'    this path is also stored in `getOption("jam.sessions_path")`.
#' 2. `"objects"` - the folder where R objects are saved,
#'    this path is also stored in `getOption("jam.objects_path")`.
#' 3. `"functions"` - the folder where R functions are saved,
#'    this path is also stored in `getOption("jam.functions_path")`.
#'
#' To use custom file paths in each R session, use either `~/.Rprofile`
#' or an appropriate method to define the relevant options:
#'
#' * `options("jam.sessions_path"="/some/specific/path/sessions")`,
#' * `options("jam.objects_path"="/some/specific/path/objects")`,
#' * `options("jam.functions_path"="/some/specific/path/functions")`.
#'
#' When a file path is not defined in `options()`, the corresponding
#' default path is used:
#'
#' * `"sessions"` uses default `"~/Projects/R-sessions"`
#' * `"objects"` uses default `"~/Projects/R-objects"`
#' * `"functions"` uses default `"~/Projects/R-scripts"`
#'
#' (Note `~` refers to the home directory, which may also be defined
#' by `$HOME` depending upon the computer operating system, for
#' example linux, Mac OSX, or Microsoft Windows.)
#'
#' @return `list` is returned, with named elements
#'    `"sessions"`, `"objects"`, and `"functions"`.
#'
#' @family jamsession utilities
#'
#' @param sessions,objects,functions character vector with one or more
#'    file directory path locations, suitable for use by `list.files(path=x)`.
#'    When a value is defined, it is used to override the existing
#'    `options()` value, so the updated value will be used for all
#'    related jamsession functions. Note that any value `""` is removed,
#'    for example `sessions=""` is equivalent to `sessions=NULL`.
#' @param create logical indicating whether to create a file path
#'    that does not already exist. The directory is created using
#'    `dir.create()` using the arguments `mode` and `recursive`.
#' @param mode,recursive arguments passed to `dir.create()` when
#'    necessary, by the argument `create`.
#' @param verbose logical indicating whether to print verbose output.
#' @param ... additional arguments are passed to `dir.create()`.
#'
#' @examples
#' ## Display the current or default paths
#' c(jamsession_paths(functions=NULL))
#'
#' ## Update the functions path
#' jamsession_paths(functions="~/Projects/R-functions")
#'
#' ## confirm the functions path has been updated
#' c(jamsession_paths())
#' getOption("jam.functions_path")
#'
#' ## Revert the functions path
#' c(jamsession_paths(functions="~/Projects/R-scripts"))
#'
#' @export
jamsession_paths <- function
(sessions=NULL,
 objects=NULL,
 functions=NULL,
 create=FALSE,
 mode="0755",
 recursive=TRUE,
 verbose=FALSE,
 ...)
{
   ## sessions
   sessions <- setdiff(sessions, "");
   if (length(sessions) == 0) {
      sessions <- getOption("jam.sessions_path");
   } else {
      options("jam.sessions_path"=sessions);
   }
   if (length(sessions) == 0) {
      sessions <- "~/Projects/R-sessions";
      options("jam.sessions_path"=sessions);
   }

   ## objects
   objects <- setdiff(objects, "");
   if (length(objects) == 0) {
      objects <- getOption("jam.objects_path");
   } else {
      options("jam.objects_path"=objects);
   }
   if (length(objects) == 0) {
      objects <- "~/Projects/R-objects";
      options("jam.objects_path"=objects);
   }

   ## functions
   functions <- setdiff(functions, "");
   if (length(functions) == 0) {
      functions <- getOption("jam.functions_path");
   } else {
      options("jam.functions_path"=functions);
   }
   if (length(functions) == 0) {
      functions <- "~/Projects/R-scripts";
      options("jam.functions_path"=functions);
   }

   if (create) {
      path_set <- c(session, object, `function`);
      path_not_found <- path_set[!dir.exists(path_set)];
      for (path in path_not_found) {
         if (verbose) {
            jamba::printDebug("jamsession_paths(): ",
               "Creating new directory: ",
               paste0("'", path, "'"),
               sep=", ");
         }
         dir.create(path=path,
            recursive=recursive,
            mode=mode,
            ...);
      }
   }

   jamsession_paths <- list(sessions=sessions,
      objects=objects,
      functions=functions);
   return(jamsession_paths);
}
