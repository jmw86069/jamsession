
#' jamsession file paths
#'
#' jamsession file paths
#'
#' This function retrieves the file path for each jamsession type:
#'
#' 1. 'sessions': the folder where R sessions are saved,
#'    stored in `getOption("jam.sessions_path")`,
#'    default `"~/Projects/R-sessions"`.
#' 2. 'objects': the folder where R objects are saved,
#'    stored in `getOption("jam.objects_path")`,
#'    default `"~/Projects/R-objects"`.
#' 3. 'functions': the folder where R functions are saved,
#'    stored in `getOption("jam.functions_path")`,
#'    default `"~/Projects/R-scripts"`.
#'
#' Each may have multiple paths, and may contain `"."` to include
#' the current working directory.
#'
#' To use persistent values for your R environment, define
#' `options()` in the `~/.Rprofile` in your HOME directory,
#' or in the folder of the active project.
#'
#' To use custom file paths in each R session, use either `~/.Rprofile`
#' or an appropriate method to define the relevant options.
#'
#' * `options("jam.sessions_path"="/some/path/sessions")`,
#' * `options("jam.objects_path"="/some/path/objects")`,
#' * `options("jam.functions_path"="/some/path/functions")`.
#'
#'
#' @returns `list` with named elements
#'    'sessions', 'objects', and 'functions'.
#'
#' @family jamsession utilities
#'
#' @param sessions,objects,functions `character` vector with one or more
#'    file directory path locations, suitable for use by `list.files(path=x)`.
#'    When a value is defined, it is used to override the existing
#'    `options()` value, so the updated value will be used for all
#'    related jamsession functions. Note that any value `""` is removed,
#'    for example `sessions=""` is equivalent to `sessions=NULL`.
#' @param create `logical` default FALSE, whether to create a file path
#'    that does not already exist. Only the first path is created
#'    for each of 'sessions', 'objects', 'functions' when none exist.
#'    The directory is created using `dir.create()` using the arguments
#'    `mode` and `recursive`.
#' @param mode,recursive arguments passed to `dir.create()` when
#'    necessary, by the argument `create`.
#' @param verbose `logical` indicating whether to print verbose output.
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
   #
   # sessions
   sessions <- setdiff(sessions, "");
   if (length(sessions) == 0) {
      sessions <- getOption("jam.sessions_path",
         "~/Projects/R-sessions");
   }
   options("jam.sessions_path"=sessions);

   # objects
   objects <- setdiff(objects, "");
   if (length(objects) == 0) {
      objects <- getOption("jam.objects_path",
         "~/Projects/R-objects");
   }
   options("jam.objects_path"=objects);

   # functions
   functions <- setdiff(functions, "");
   if (length(functions) == 0) {
      functions <- getOption("jam.functions_path",
         "~/Projects/R-scripts")
   }
   options("jam.functions_path"=functions);

   # create missing file paths (optional)
   if (isTRUE(create)) {
      paths_set <- list(session, object, functions);
      path_not_found <- NULL;
      # add only one path per type,
      # and only if no path already exists for that type.
      for (ipaths in paths_set) {
         ipaths <- setdiff(ipaths, ".");
         # ipaths <- head(setdiff(ipaths, "."), 1);
         ipaths_found <- ipaths[dir.exists(ipaths)];
         if (length(ipaths_found) == 0) {
            if (length(ipaths) > 0) {
               path_not_found <- c(path_not_found,
                  head(ipaths, 1));
            }
         }
      }

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
