##
## jamsession.R
##
## manage R sessions, R objects, and R functions
##
## Functions included:
## listSessions
## grepSessions

#' jamsession: manage R sessions, R objects, and R functions
#'
#' The jamsession package is based upon the notion of central storage
#' of R sessions, R objects, and R scripts, to facilitate re-use
#' and to minimize the time spent task-switching across analysis
#' projects.
#'
#' The methods subscribe to my personal priority for writing R functions,
#' at least make my own life easier! These functions make my analysis life
#' more efficient, as I know I can retrieve an R session from a project
#' from several years ago. If I want to re-use an R object from an R session,
#' I have a mechanism to do so that scales reasonably well. If I realize that
#' I munged some data from a week ago, I can go back and reload my R session
#' from a week ago.
#'
#' The R session mechanism also changes the R prompt to include the project
#' name, which I have found immensely helpful when multiple active R sessions
#' are open, and I need to remember which is which.
#'
#' The basic units in jamsession:
#' \describe{
#'    \item{session}{An .RData file based upon a project name, saved
#'       alongside the Rhistory, in a central location. One can discover
#'       a past R session with `grep_jamessions("array950")` and
#'       load with `load-jamsession(grep_jamsessions("array950"))`.}
#'    \item{object}{An .RData file which contains only one R object, intended
#'       to be saved and possibly re-used in a separate R session. Objects
#'       can be discovered using `grep_objects("hg19refgene")` and
#'       loaded with `load_object("hg19refGene")`.}
#'    \item{function}{An .R file which contains custom R functions related
#'       to a specific project, but have not yet warranted being rolled into
#'       their own R package yet. Such functions can be loaded or refreshed
#'       with `refresh_functions(project)`. The reason to store
#'       functions separately and load via this mechanism is that the functions
#'       can often encapsulate logic too complex to run in a live R console,
#'       and to keep function code out of the R `.GlobalEnv` to save
#'       space. The mechanism here will load functions into their own R
#'       environment, whose parent environment is .GlobalEnv to allow functions
#'       to be in the usual R function search space.}
#' }
#'
#' @docType package
#' @keywords internal
#' @name jamsession
"_PACKAGE"




#' save R object to a central location
#'
#' save R object to a central location
#'
#' This function saves a single R object to an .RData file,
#' versioned by date using `jamba::getDate()`,
#' so it can be discovered and re-used by other R sessions.
#'
#' When multiple objects should be saved together, the recommended method:
#'
#' * create a list object that includes these objects
#' * save this list object using `save_object()`
#'
#' However, when multiple objects are supplied in `object_list`,
#' the object names are concatenated with `"-"` and this string
#' is used to define the saved R object file. For example
#' `save_object(c("df1", "df2"))` will save an R object file
#' using the string `"df1-df2"`. When this file is loaded,
#' two R objects are loaded into the environment: `"df1"` and `"df2"`.
#' While not always ideal, this mechanism may be more convenient,
#' and it is left for the user to decide which is best.
#'
#' Note that `objects_path` may contain multiple directories, and
#' when this occurs, the directories are attempted in order. The
#' R object is saved to the first directory that allows the file
#' to be saved successfully.
#'
#' @return `character` string with the file path where the
#'    R object data is stored.
#'
#' @family jamsession objects
#'
#' @param object_list `character` vector with one or more R object
#'    names to save. The typical workflow stores only one object per
#'    file. We recommend storing multiple objects into one file only
#'    when those objects are only useful together, and using only one
#'    object would be insufficient or lack enough context for full re-use.
#' @param save_date `character` date string to use when naming the file.
#'    By default, the current date is used via the function
#'    `jamba::getDate()`, however to use a custom date format any
#'    text string can be used here.
#' @param objects_path `character` vector of one or more file paths to
#'    search for saved R objects. When `objects_path=NULL`, it uses
#'    the output from `jamsession_paths()$objects`. The process will
#'    try to save the `.RData` file in each path in `objects_path`
#'    in order, and will use the first successful attempt. For example
#'    if two paths are supplied, and the first path is not accessible
#'    or not user-writeable, the second path will be attempted.
#'    If no paths are user-writeable, this function will `stop()`.
#' @param object_notes_list `character` vector or `list` of `character`
#'    vectors that contain optional notes associated with the R object(s).
#'    This mechanism is intended as a crude way to store text information
#'    associated with the stored R objects. For example, one could store
#'    a short text description of how to R objects were created, or some
#'    other useful information. This process is currently experimental,
#'    and may be refactored in future to have a more consistent workflow.
#' @param do_file_info `logical` indicating whether to print information
#'    about the saved `.RData` file, which includes the stored file
#'    size and path.
#' @param object_suffix `character` string used as the file extension,
#'    by default `".RData"`. This value should probably never change,
#'    but it is possible to change here.
#' @param envir the `environment` from which to obtain the R object.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examplesIf interactive()
#' withr::with_options(list(jam.objects_path=tempdir()), {
#'
#' example_df <- data.frame(name=LETTERS[1:5], values=letters[1:5])
#' save_object("example_df")
#' grep_objects("example")
#'
#' list_objects()
#'
#' })
#' @export
save_object <- function
(object_list,
 save_date=jamba::getDate(),
 objects_path=jamsession_paths()$objects,
 object_notes_list="",
 do_file_info=FALSE,
 object_suffix=".RData",
 envir=globalenv(),
 verbose=TRUE,
 ...)
{
   ## Purpose is to save RData individual objects to files, versioned by date, so they
   ## can be recalled without having to load an entire R session.

   ## To add description, the objectNotesList parameter gets saved to a description file
   ## after subjected to unlist().
   object_list_name <- jamba::cPaste(object_list, sep="-");

   ## Add descriptive attributes -- OMIT FOR NOW SINCE ISN'T SEARCHABLE
   if (length(object_notes_list) > 0 && length(object_list) == 1 &&
         is.atomic(object_notes_list)) {
      object_notes_list <- list(object_notes_list);
   }
   if (is.list(object_notes_list) &&
         length(object_notes_list) == length(object_list) &&
         !(all(object_notes_list %in% c(NA, ""))) ) {
   } else {
      object_notes_list <- NULL;
   }

   if ("." %in% objects_path) {
      if ("first" %in% prefer_local) {
         objects_path <- unique(c(".", objects_path))
      } else if ("last" %in% prefer_local) {
         objects_path <- unique(c(
            setdiff(objects_path, "."),
            "."))
      }
   }

   ## iterate session_path to use the first writeable directory
   for (use_object_path in objects_path) {
      object_file_name <- file.path(
         use_object_path,
         paste0(
            object_list_name,
            "_",
            save_date,
            ".RData"));
      save_success <- tryCatch({
         save(
            list=object_list,
            file=object_file_name,
            envir=envir);
         TRUE;
      }, error=function(e){
         FALSE;
      });
      if (save_success) {
         break;
      }
   }

   if (!save_success) {
      stop("Object could not be saved.");
   }
   if (verbose) {
      plural <- ifelse(length(object_list) > 1, "s", "");
      jamba::printDebug("save_objects(): ",
         c(
            paste0("Object", plural, " saved:"),
            object_file_name),
         sep="");
      if (do_file_info) {
         print(fileInfo(object_file_name));
      }
   }

   ## Save object notes if provided
   if (length(object_notes_list) > 0) {
      object_notes_file_name <- file.path(
         use_object_path,
         paste0(object_list_name,
            "_",
            save_date,
            "_notes.txt"));
      object_notes_df <- data.frame(check.names=FALSE,
         object_attr_type=c("object_list_name",
            rep("object_name", length(unlist(object_list))),
            rep("object_notes", length(unlist(object_notes_list))) ),
         object_attribute=c(object_list_name,
            unlist(object_list),
            unlist(object_notes_list))
      );
      write.table(file=object_notes_file_name,
         row.names=FALSE,
         quote=FALSE,
         na="",
         sep="\t",
         append=FALSE,
         col.names=TRUE,
         x=object_notes_df);
      if (verbose) {
         jamba::printDebug("save_objects(): ",
            c("Object note(s) saved:",
               object_notes_file_name),
            sep="");
      }
   }

   invisible(object_file_name);
}

#' list saved R objects
#'
#' list saved R objects
#'
#' This function lists saved R objects for the given `objects_path`,
#' usually called by `grep_objects()`.
#'
#' When `most_recent=TRUE` it returns only the most recent version
#' of each file.
#'
#' It is possible to supply multiple file paths using the
#' argument `objects_path`, in which case all objects are returned
#' for all directories, then they are sorted by `save_date`.
#' The input `objects_path` is stored as an ordered factor, such
#' that two files with the same `save_date` will still be ordered
#' consistent with the input directories in `objects_path`.
#' In this case, when `most_recent=TRUE` there will be one entry
#' returned per `object` name, and the first entry will be the
#' most recent entry ordered by `save_date` then `objects_path`.
#' In practice, it is recommended to use one `objects_path`
#' location, but this option exists.
#'
#' For example, `load_object(grep_objects("my_favorite_object"))`
#' will load the most recent object by `save_date` where any
#' ties prefer the order in `objects_path`. Similarly,
#' `load_object("my_favorite_object")` does the same thing,
#' without the grep_patten search step.
#'
#' @return list containing
#' \describe{
#'    \item{object_df}{`data.frame` of R objects}
#'    \item{object}{`vector` of matching R object names}
#' }
#'
#' @family jamsession internals
#'
#' @param objects_path character vector of one or more file paths to
#'    search for saved R objects. When `objects_path=NULL`, it uses
#'    the output from `jamsession_paths()$objects`.
#' @param most_recent logical whether to return only the most recent
#'    saved entry for each session name. When `most_recent` is `FALSE`,
#'    this function returns all saved versions for all R session names.
#' @param object_prefix,object_suffix `character` string used as
#'    prefix or suffix when searching each path in `objects_path`
#'    for matching file names. By default, files are expected
#'    to have file extension `.RData`.
#' @param add_stats `logical` indicating whether to include summary
#'    stats for each object.
#' @param ... additional arguments are ignored.
#'
#' @examplesIf interactive()
#' # by default the full list includes all past versions
#' head(list_objects());
#'
#' # you can return only the most recent for each object
#' list_objects(most_recent=TRUE);
#'
#' # add_stats makes sense when most_recent=TRUE
#' list_objects(most_recent=TRUE, add_stats=TRUE);
#'
#' @export
list_objects <- function
(objects_path=jamsession_paths()$objects,
 most_recent=FALSE,
 object_prefix="",
 object_suffix="[.]RData$",
 add_stats=FALSE,
 ...)
{
   if (length(objects_path) == 0) {
      objects_path <- jamsession_paths()$objects;
   }
   object_pattern <- paste0(object_prefix,
      ".+",
      object_suffix);
   object_files <- list.files(path=objects_path,
      pattern=object_pattern,
      full.names=TRUE);
   if (length(object_files) == 0) {
      return(list(
         object_df=data.frame(
            object=character(0),
            save_date=character(0),
            days_old=integer(0),
            file_size=character(0),
            object_path=character(0),
            object_file=character(0)),
         object=character(0)));
   }

   object_file_info <- fileInfo(object_files);
   object_file_size <- object_file_info[,"size"];
   object_file_bytes <- file.info(object_files)$size;

   object_files2 <- gsub(
      "_([^_]+)$",
      ":!!:\\1",
      gsub(object_suffix,
         "",
         gsub(object_prefix,
            "",
            basename(object_files))));
   object_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      jamba::rbindList(strsplit(object_files2, ":!!:")));
   colnames(object_df)[1:2] <- c("object", "save_date");

   object_df$days_old <- jamba::dateToDaysOld(object_df$save_date);
   object_df$file_size <- object_file_size;

   object_df$object_path <- dirname(object_files);
   ## make object_path factor with ordered levels so it
   ## will sort in order of objects_path
   object_df$object_path <- factor(object_df$object_path,
      levels=unique(c(objects_path,
         object_df$object_path)));
   object_df$object_file <- basename(object_files);

   rownames(object_df) <- jamba::makeNames(
      jamba::pasteByRow(object_df[,c("object","save_date"),drop=FALSE]),
      renameFirst=FALSE,
      startN=2);

   ## Add descriptive stats
   if (add_stats) {
      if (!suppressPackageStartupMessages(require(data.table))) {
         object_dt <- data.table::data.table(object_df);
         object_dt$bytes <- object_file_bytes;
         per_object_df <- data.frame(check.names=FALSE,
            object_dt[,
               list(
                  number_saved=length(save_date),
                  total_size=jamba::asSize(sum(bytes)),
                  days_span=diff(range(days_old))+1),
               by=object]);
      } else {
         number_saved <- lengths(split(object_df$object, object_df$object));
         bytes_per_object <- sapply(split(object_file_bytes, object_df$object), sum);
         days_span <- sapply(split(object_df$days_old, object_df$object), function(i){
            diff(range(i))+1
         })
         per_object_df <- data.frame(object=names(number_saved),
            number_saved=number_saved,
            total_size=jamba::asSize(bytes_per_object[names(number_saved)]),
            days_span=days_span[names(number_saved)]);
      }
   }

      object_df <- jamba::mixedSortDF(object_df,
         byCols=c("object",
            "days_old",
            "object_path"));
   if (most_recent && nrow(object_df) > 1) {
      object_df <- subset(object_df, !duplicated(object));
   }

   if (add_stats) {
      object_df <- merge(object_df,
         per_object_df,
         all.x=TRUE,
         all.y=TRUE);
   }

   return(list(object_df=object_df,
      object=unique(object_df$object)))
}

#' search for saved R objects
#'
#' search for saved R objects
#'
#' This function searches for saved R objects using one or more
#' text or regular expression patterns.
#'
#' It is possible to supply multiple file paths using the
#' argument `objects_path`, in which case all objects are returned
#' for all directories, then they are sorted by `save_date`.
#' The input `objects_path` is stored as an ordered factor, such
#' that two files with the same `save_date` will still be ordered
#' consistent with the input directories in `objects_path`.
#' In this case, when `most_recent=TRUE` there will be one entry
#' returned per `object` name, and the first entry will be the
#' most recent entry ordered by `objects_path`.
#' In practice, it is recommended to use one `objects_path`
#' location, but this option exists.
#'
#' For example, `load_object(grep_objects("my_favorite_object"))`
#' will load the most recent object by `save_date` where any
#' ties prefer the order in `objects_path`. Similarly,
#' `load_object("my_favorite_object")` does the same thing,
#' without the grep_patten search step.
#'
#' See `save_object()` for examples.
#'
#' @return `data.frame` when `return_df=TRUE`; `character vector`
#'    when `return_df=FALSE`. The returned data will contain `object`
#'    names that match the `grep_string` input.
#'
#' @family jamsession objects
#'
#' @param grep_string `character` string with regular expression pattern,
#'    although usually any text substring will work. When `grep_string`
#'    contains multiple values, they are used in order to find matching
#'    object names, using `jamba::provigrep()`. The order typically
#'    has no effect, but can be used to retain the order of objects
#'    returned even when the objects have the same `save_date`.
#' @param save_date `character` string or vector, used as a regular
#'    expression search for matching values in the `save_date`
#'    column output from `list_objects()`. It can be used to search
#'    for objects with a specific `save_date`. Remember to use
#'    `most_recent=FALSE` when searching for past versions of
#'    stored R objects.
#' @param objects_path character vector of one or more file paths to
#'    search for saved R objects. When `objects_path=NULL`, it uses
#'    the output from `jamsession_paths()$objects`.
#' @param ignore.case `logical` passed to `grep()` indicating whether
#'    to ignore upper and lower case, by default `ignore.case=TRUE`.
#' @param return_df `logical` indicating whether to return a `data.frame`.
#' @param most_recent logical whether to return only the most recent
#'    saved version of each matching object, by default `most_recent=TRUE`.
#'    Note that it returns the most recent of *each* matching object.
#' @param add_stats `logical` indicating whether to include summary
#'    stats for each object: `number_saved` is the number of past versions
#'    of the object; `total_size` is the total file size for all saved
#'    versions.
#' @param verbose logical whether to print verbose output
#' @param ... additional arguments are passed to `grepl()`.
#'
#'
#' @export
grep_objects <- function
(grep_string=".",
 save_date=".",
 objects_path=jamsession_paths()$objects,
 ignore.case=TRUE,
 return_df=TRUE,
 most_recent=TRUE,
 add_stats=FALSE,
 verbose=FALSE,
 ...)
{
   ## Primary purpose is to search through the stored objects for substrings

   ## Obtain saved objects
   object_list_l <- list_objects(objects_path=objects_path,
      most_recent=most_recent,
      add_stats=add_stats,
      ...);
   object_df <- object_list_l$object_df;

   ## Handle multiple grep_string input using jamba::provigrep()
   if (length(grep_string) > 0) {
      object_rows <- jamba::provigrep(grep_string,
         object_df$object,
         ignore.case=ignore.case,
         value=FALSE);
      object_df <- object_df[object_rows,,drop=FALSE];
   }
   if (nrow(object_df) == 0) {
      return(object_df);
   }

   ## Handle multiple save_date input using jamba::provigrep()
   if (length(save_date) > 0) {
      object_rows <- jamba::provigrep(save_date,
         object_df$save_date,
         ignore.case=ignore.case,
         value=FALSE);
      object_df <- object_df[object_rows,,drop=FALSE];
   }
   if (nrow(object_df) == 0) {
      return(object_df);
   }

   ## Order for newest first
   if (!"days_old" %in% colnames(object_df)) {
      object_df$days_old <- jamba::dateToDaysOld(object_df$save_date);
   }
   object_df <- jamba::mixedSortDF(object_df,
      byCols=jamba::provigrep(
         c("days_old",
            "object",
            "object_dir"),
         colnames(object_df)));

   if (return_df) {
      return(object_df);
   } else {
      obj_list <- unique(object_df$object);
      return(obj_list);
   }
}


#' load R object by name
#'
#' load R object by name
#'
#' This function loads the most recently saved R object by name, from
#' a given file path. This mechanism is intended to help share useful R
#' objects across R sessions, especially where it may be helpful to discover
#' R objects by name, and where R objects may continue to be useful over time.
#'
#' Typically this function is called with the name of one or more saved
#' objects, and loads the most recent version of each. However, the argument
#' `save_date` can be supplied as a vector, which is recycled for each
#' saved object, as a method to load a specific version of an object.
#' This behavior is not intended for routine use, but is available
#' in rare cases that you may want to inspect previous versions of
#' an object.
#'
#' Note that `envir` can be used to load the object into a separate
#' environment, if needed.
#'
#' @return invisible name of the R object loaded. It also loads the R object
#' into the environment specified, by default the active global environment
#' .GlobalEnv.
#'
#' @family jamsession objects
#'
#' @param object `character` string indicating the name of the R
#'    object to load, or `data.frame` output from `grep_objects()`
#'    which contains colnames `("object", "object_path", "object_file")`.
#' @param save_date optional `character` string with a specific date to
#'    use when loading an object. This string is matched with
#'    the `"save_date"` returned by `list_objects()`.
#'    When `save_date` is `NULL`, the most recent
#'    object is loaded, which is the default behavior.
#' @param objects_path `character` vector of one or more file paths to
#'    search for saved R objects. When `objects_path=NULL`, it uses
#'    the output from `jamsession_paths()$objects`.
#' @param envir optional R `environment` inside which to load the R object.
#'    Note that if an R object already exists in this environment,
#'    it will overwritten without warning. This behavior is intended
#'    by `load_object()`.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are ignored.
#'
#' @examplesIf interactive()
#'
#' withr::with_options(list(jam.objects_path=tempdir()), {
#'
#' my_df <- data.frame(col1=LETTERS[1:5], col2=letters[1:5]);
#' print(my_df);
#'
#' save_object("my_df", verbose=FALSE);
#'
#' print(grep_objects("my_df"))
#'
#' rm(my_df);
#'
#' load_object("my_df", verbose=FALSE);
#' print(my_df);
#'
#' load_object(grep_objects("my_df"), verbose=FALSE)
#' print(my_df);
#' })
#'
#' ######################################
#' # Load into a separate environment
#' rm(my_df);
#' sep_env <- new.env();
#'
#' withr::with_options(list(jam.objects_path=tempdir()), {
#'
#' load_object("my_df", envir=sep_env, verbose=FALSE)
#'
#' # this object is not in the current workspace
#' print(find("sep_env"))
#'
#' # you can use get() to see this object
#' print(get("my_df", envir=sep_env));
#'
#' })
#'
#' ######################################
#' # example showing two objects stored together
#' withr::with_options(list(jam.objects_path=tempdir()), {
#' my_df2 <- data.frame(B=LETTERS[11:15], b=letters[11:15]);
#'
#' load_object("my_df", verbose=FALSE);
#' save_object(c("my_df", "my_df2"), verbose=FALSE);
#'
#' print(grep_objects("my_df2"))
#' loaded <- load_object(grep_objects("my_df2"), verbose=FALSE);
#'
#' # note that it loads two objects
#' print(loaded);
#' })
#' @export
load_object <- function
(object,
 save_date=NULL,
 objects_path=jamsession_paths()$objects,
 envir=globalenv(),
 verbose=TRUE,
 ...)
{
   ## Purpose is to load a stored RData object which contains only a fixed, named object

   object_names_list <- character(0);

   if (length(dim(object)) == 2) {
      object_df <- object;
      if (!"object" %in% colnames(object_df)) {
         stop("Input data.frame must have colnames that include 'object'");
      }
      object_dupes <- jamba::tcount(object_df$object, minCount=2);
      if (length(object_dupes) > 0) {
         stop(paste0("Object names cannot be duplicated in input data.frame: ",
            jamba::cPaste(names(object_dupes))));
      }
   } else {
      if (length(save_date) == 0) {
         object_df <- list_objects(
            objects_path=objects_path,
            most_recent=TRUE)$object_df;
         if (!all(object %in% object_df$object)) {
            missing_objects <- setdiff(object, object_df$object);
            if (length(missing_objects) == 1) {
               stop(paste0("One object file is not available: ",
                  missing_objects));
            } else {
               stop(paste0("Some object files are not available: ",
                  jamba::cPaste(missing_objects, sep=", ")));
            }
         }
         obj_match <- match(unique(object), object_df$object);
         object_df <- object_df[obj_match,,drop=FALSE];
      } else {
         object_df <- list_objects(
            objects_path=objects_path,
            most_recent=FALSE)$object_df;
         if (!all(object %in% object_df$object)) {
            missing_objects <- setdiff(object, object_df$object);
            if (length(missing_objects) == 1) {
               stop(paste0("One object file is not available for any date: ",
                  missing_objects));
            } else {
               stop(paste0("Some object files are not available for any date: ",
                  jamba::cPaste(missing_objects, sep=", ")));
            }
         }
         object_save_date <- paste0(object, "/", save_date);
         df_object_save_date <- jamba::pasteByRow(object_df[,c("object", "save_date"),drop=FALSE],
            sep="/");
         osd_match <- jamba::rmNA(match(object_save_date,
            df_object_save_date));
         if (!length(osd_match) == length(object)) {
            missing_objects <- setdiff(object_save_date,
               df_object_save_date);
            stop("Some object/save_date entries were not found: ",
               jamba::cPaste(missing_objects, sep=", "));
         }
         object_df_use <- object_df[osd_match,,drop=FALSE];
         object_df <- object_df_use;
      }
   }

   ## Load each file individually
   object_file_paths <- file.path(
      path.expand(as.character(object_df$object_path)),
      object_df$object_file);
   for (irow in seq_len(nrow(object_df))) {
      object_file_path <- object_file_paths[irow];
      object_name_list <- load(object_file_path,
         envir=envir);
      if (verbose) {
         if (length(save_date) > 0) {
            date_addon <- paste0(" from ", object_df$save_date[irow]);
         } else {
            date_addon <- NULL;
         }
         if (length(object_name_list) == 1) {
            jamba::printDebug("load_object(): ",
               c("Loaded object: '",
                  object_df$object[irow],
                  "'"),
               date_addon,
               sep="");
         } else {
            jamba::printDebug("load_object(): ",
               "Loaded objects: ",
               object_name_list,
               date_addon);
         }
      }
      object_names_list <- c(object_names_list, object_name_list);

      ## If object notes exist, load that file
      object_notes_file_name <- file.path(
         object_df$object_path[irow],
         paste0(object_df$object[irow],
            "_",
            object_df$save_date[irow],
            "_notes.txt"));
      if (file.exists(object_notes_file_name)) {
         object_notes_df <- utils::read.table(object_notes_file_name,
            sep="\t",
            check.names=FALSE,
            as.is=TRUE,
            fill=TRUE,
            quote="\"",
            allowEscapes=FALSE,
            comment.char="",
            header=TRUE,
            stringsAsFactors=FALSE);
         object_name_list1 <- subset(object_notes_df,
            object_notes_df[[1]] %in% c("object_name","objectName"))[,2];
         object_notes_list <- subset(object_notes_df,
            object_notes_df[[1]] %in% c("object_notes","objectNotes"))[,2];
         object_notes_df_use <- data.frame(check.names=FALSE,
            stringsAsFactors=FALSE,
            "object"=object_name_list1,
            "object_notes"=object_notes_list);
         if (verbose && any(nchar(object_notes_df_use$object_notes) > 0)) {
            print(object_notes_df_use);
         }
      }
   }
   # Return the list of object names, just in case
   invisible(object_names_list);
}

