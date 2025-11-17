
#' list available R functions for jamsession
#'
#' list available R functions for jamsession
#'
#' This function lists available R functions for jamsession,
#' in the given `functions_path`.
#' Available functions are defined by files with suffix
#' `'_functions.R'`.
#'
#' It is possible to supply multiple file paths using the
#' argument `functions_path`. All functions are returned,
#' sorted by `save_date`, then by the order of `functions_path`.
#' So a file saved on the same date will be returned by first
#' entry in `functions_path`.
#'
#' When using `most_recent=TRUE`, only the first entry is returned
#' for each unique value in the 'functions' column,
#' ordered by 'save_date' and 'functions_path'.
#'
#'
#' @returns `data.frame` by default, each row is a matching functions file,
#'    with rownames defined using 'functions' and 'save_date'.
#'    The rows are sorted by 'days_old', 'functions', then 'functions_path'
#'    by default.
#'    * Output with `return_type='functions'` is a `character` vector of
#'    functions file names.
#'    * Output when `return_type='list'` is a `list` with the two
#'    components above, named 'functions_df' and 'functions', respectively.
#'
#' @family jamsession internals
#'
#' @param functions_path `character` with default using
#'    `jamsession_paths()$functions`, with file paths to search
#'    for matching files with suffix `'_functions.R'`.
#' @param most_recent `logical` default FALSE, whether to return
#'    only the most recent file per unique functions name.
#' @param function_prefix,function_suffix `character` string used as
#'    regular expression patterns searching files in `functions_path`.
#'    Default uses no prefix, and suffix `'_functions.R'`.
#' @param return_type `character` default 'df' with type of object to return.
#'    * 'df': returns `data.frame`
#'    * 'functions': returns `character` vector of functions names.
#'    * 'list': returns `list` with both the above.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # by default the full list includes all past versions
#' head(list_functions());
#'
#' # you can return only the most recent for each function
#' list_functions(most_recent=TRUE);
#'
#' @export
list_functions <- function
(functions_path=jamsession_paths()$functions,
 most_recent=FALSE,
 function_prefix="",
 function_suffix="_functions[.]R$",
 return_type=c("df", "functions", "list"),
 ...)
{
   return_type <- match.arg(return_type);
   if (length(functions_path) == 0) {
      functions_path <- jamsession_paths()$functions;
   }
   function_pattern <- paste0(
      function_prefix,
      "(.+)",
      function_suffix);
   functions_files <- list.files(path=functions_path,
      pattern=function_pattern,
      full.names=TRUE);
   if (length(functions_files) == 0) {
      return(list(
         functions_df=data.frame(
            "functions"="blank",
            save_date="01jan2025",
            days_old=10L,
            file_size="100b",
            functions_path="path",
            functions_file="file_functions.R")[0, , drop=FALSE],
         functions=character(0)));
   }

   functions_file_info <- fileInfo(functions_files);
   functions_file_size <- functions_file_info[,"size"];
   functions_file_bytes <- file.info(functions_files)$size;

   functions_files2 <- gsub(function_suffix,
      "",
      gsub(function_prefix,
         "",
         basename(functions_files)));

   functions_df <- data.frame(check.names=FALSE,
      functions=functions_files2,
      save_date=tolower(format(
         functions_file_info$mtime, format="%d%b%Y")))

   functions_df$days_old <- jamba::dateToDaysOld(functions_df$save_date);
   functions_df$file_size <- functions_file_size;

   functions_df$functions_path <- dirname(functions_files);
   ## make functions_path factor with ordered levels so it
   ## will sort in order of functions_path
   functions_df$functions_path <- factor(functions_df$functions_path,
      levels=unique(c(functions_path,
         functions_df$functions_path)));
   functions_df$functions_file <- basename(functions_files);

   rownames(functions_df) <- jamba::makeNames(
      jamba::pasteByRow(
         functions_df[, c("functions", "save_date"), drop=FALSE]),
      renameFirst=FALSE,
      startN=2);

   # sort by days_old, name, path
   functions_df <- jamba::mixedSortDF(functions_df,
      byCols=c("days_old",
         "functions",
         "functions_path"));
   if (TRUE %in% most_recent && nrow(functions_df) > 1) {
      functions_df <- subset(functions_df, !duplicated(functions))
   }

   if ("df" %in% return_type) {
      return(functions_df)
   } else if ("functions" %in% return_type) {
      return(unique(functions_df$functions))
   }
   return(
      list(
         functions_df=functions_df,
         functions=unique(functions_df$functions)
      ))
}


#' search for saved R functions
#'
#' search for saved R functions
#'
#' This function searches for saved R functions in the appropriate
#' file paths as defined in `jamsession_paths()`, using
#' `jamsession_paths()$functions`, or customized with
#' argument `functions_path`.
#'
#' It is possible to supply multiple file paths using the
#' argument `functions_path`. All functions are returned,
#' sorted by `save_date`, then by the order of `functions_path`.
#' So a file saved on the same date will be returned by first
#' entry in `functions_path`.
#'
#' When using `most_recent=TRUE`, only the first entry is returned,
#' using the most recent entry ordered by `functions_path`.
#'
#' For example, `refresh_functions(grep_functions("myfunctions"))`
#' will load the most recent function by `save_date` where any
#' ties prefer the order in `functions_path`. Similarly,
#' `refresh_functions("my_favorite_function")` does the same thing,
#' without the grep_patten search step.
#'
#' @return one of:
#'    * When `return_df=TRUE` it returns `data.frame`
#'    * Otherwise `character` vector.
#'
#' @family jamsession functions
#'
#' @param grep_string `character` string with regular expression pattern,
#'    although usually any text substring will work.
#'    * Note that input may contain the date as seen in `rownames()`
#'    of `list_functions()` output.
#'    When `grep_string` contains multiple values, they are used
#'    in order to find matching object names, using `jamba::provigrep()`.
#'    The order typically has no effect, but can be used to retain the
#'    order of objects returned even when the objects have the same
#'    `save_date`.
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
#' @param verbose logical whether to print verbose output
#' @param ... additional arguments are passed to `grepl()`.
#'
#'
#' @export
grep_functions <- function
(grep_string=".",
 save_date=".",
 functions_path=jamsession_paths()$functions,
 ignore.case=TRUE,
 return_df=TRUE,
 most_recent=TRUE,
 verbose=FALSE,
 ...)
{
   ## Primary purpose is to search through the stored functions for substrings

   ## Obtain saved functions
   functions_df <- list_functions(functions_path=functions_path,
      most_recent=FALSE,
      ...);
   # functions_df <- function_list_l$functions_df;

   ## Handle multiple grep_string input using jamba::provigrep()
   if (length(grep_string) > 0) {
      functions_rows <- jamba::provigrep(grep_string,
         functions_df$functions,
         ignore.case=ignore.case,
         value=TRUE);
      print(functions_rows);# debug
      functions_df <- subset(functions_df,
         functions_df$functions %in% functions_rows |
            rownames(functions_df) %in% grep_string);
      # functions_df <- functions_df[functions_rows, , drop=FALSE];
   }
   if (nrow(functions_df) == 0) {
      return(functions_df);
   }

   # apply most_recent
   if (TRUE %in% most_recent) {
      subset(functions_df, !duplicated(functions));
   }

   ## Handle multiple save_date input using jamba::provigrep()
   if (length(save_date) > 0) {
      function_rows <- jamba::provigrep(save_date,
         functions_df$save_date,
         ignore.case=ignore.case,
         value=FALSE);
      functions_df <- functions_df[function_rows,,drop=FALSE];
   }
   if (nrow(functions_df) == 0) {
      return(functions_df);
   }

   ## Order for newest first
   if (!"days_old" %in% colnames(functions_df)) {
      functions_df$days_old <- jamba::dateToDaysOld(functions_df$save_date);
   }
   functions_df <- jamba::mixedSortDF(functions_df,
      byCols=jamba::provigrep(
         c("days_old",
            "functions",
            "function_dir"),
         colnames(functions_df)));

   if (return_df) {
      return(functions_df);
   } else {
      fn_list <- unique(functions_df$functions);
      return(fn_list);
   }
}

#' List functions file exports
#'
#' List functions file exports, experimental, and dependent upon the
#' R file using roxygen2 style '@exports'. Any .R files that do not
#' use roxygen2 comments will return no information.
#'
#' The process looks for any break in roxygen2 comments in an R file.
#' Specific steps are described:
#'
#' * It splits the lines of each file at the point where a non-roxygen2
#' line is immediately followed by a roxygen2 line.
#' * Within each block, it confirms that there is line that
#' begins with: `#' @export`. If there is no such line, the block
#' is ignored.
#' * Within remaining blocks, it looks for the first non-roxygen2
#' line that contains an alphabetic character, which also does not
#' have a '#' occurring before the alphabetic character.
#' It returns that one line as the "exported object" for that block.
#'
#' An example of a simple roxygen2 style R function is shown below:
#'
#' ```
#' #' Function Name
#' #'
#' #' @export
#' function_name <- function
#' (argument1, argument2, ...)
#' {
#'    # function code
#' }
#'
#' #' Another Function Name
#' #'
#' #' @export
#' another_function <- function
#' (argument1, argument2, ...)
#' {
#'    # function code
#' }
#'
#' #' Data object
#' #' @docType data
#' "Object_name"
#' ```
#'
#' In the example above, the results would be:
#'
#' ```
#' "function_name <- function"
#' "another_function <- function"
#' "Object_name"
#' ```
#'
#' @family jamsession functions
#'
#' @returns `list` named by matching `functions` files, each
#'    with a `character` vector of the matching function or exported
#'    object in the .R file.
#'
#' @param grep_string `character` passed to `grep_functions()` with
#'    default values except where provided via '...' arguments.
#' @param debug `logical` default FALSE, whether to print debug information
#'    to illustrate the processing.
#' @param ... all arguments are passed to `grep_functions()`
#'
#' @export
list_functions_exports <- function
(grep_string,
 debug=FALSE,
 ...)
{
   #
   functions_df <- grep_functions(grep_string=grep_string,
      ...)
   fnfile <- file.path(functions_df$functions_path,
      functions_df$functions_file)
   names(fnfile) <- rownames(functions_df);

   # iterate each fnfile
   fnfile_list <- lapply(fnfile, function(ifile){
      if (debug) {
         jamba::printDebug("ifile: ", ifile);# debug
      }

      ilines <- readLines(ifile);
      # keep only lines that are not empty `#'` roxygen2
      # ilines1 <- ilines[grep("^#'[ \t\r\n]*$", ilines, invert=TRUE)]
      ilines1 <- ilines;

      # split each roxygen2 block?
      blanklines <- grep("^[ \r\n\t]*$", ilines1)
      roxy2lines <- grep("^#'", ilines1)
      if (debug) {
         jamba::printDebug("blanklines: ", blanklines);# debug
         jamba::printDebug("roxy2lines: ", roxy2lines);# debug
      }
      # look for blank followed by roxy2
      splitat <- intersect(blanklines, roxy2lines - 1);

      if (debug) {
         jamba::printDebug("splitat: ", splitat);# debug
      }
      n <- length(splitat)
      if (length(splitat) == 0) {
         return(NULL)
      }

      splitvals <- rep(seq_len(length(splitat)),
         diff(c(splitat, length(ilines1))))
      splitlines <- split(ilines1, splitvals);
      splitexports <- jamba::rmNULL(lapply(seq_along(splitlines), function(inum){
         ilines2 <- splitlines[[inum]];
         if (debug) {
            jamba::printDebug("inum: ", inum,
               ", splitat:", splitat[[inum]]);# debug
         }
         if (debug > 1) {
            jamba::printDebug("ilines2: ");# debug
            cat(paste0(head(ilines2, 20), "\n"), "\n");
         }

         ilines2grep <- head(grep("^#' [@]export", ilines2), 1);
         if (length(ilines2grep) == 1) {
            # it contains @export
            ilines3 <- tail(ilines2, -ilines2grep);
            codelines <- head(jamba::vigrep("^[^#]*[a-zA-Z]", ilines3), 1);

            ## optionally consider adding line number at the start of the block
            # names(codelines) <- splitat[[inum]];

            codelines
         } else {
            if (debug) {
               jamba::printDebug("No #' @export");
            }
            return(NULL)
         }
      }))
      unlist(unname(splitexports));
   })
   return(fnfile_list)
}
