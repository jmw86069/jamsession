
#' List saved R sessions
#'
#' List saved R sessions
#'
#' This function lists available saved R sessions,
#' usually called by `grep_jamsessions()`.
#'
#' When `most_recent=TRUE` it returns only the most recent session
#' based upon the `save_date`.
#'
#' It is possible to supply multiple file paths using the
#' argument `sessions_path`, in which case all sessions are returned
#' for all directories, then they are sorted by `save_date`.
#' The input `sessions_path` is stored as an ordered factor, such
#' that two sessions with the same `save_date` will still be ordered
#' consistent with the input directories in `sessions_path`.
#' In this case, when `most_recent=TRUE` there will be one entry
#' returned per `object` name, and the first entry will be the
#' most recent entry ordered by `save_date` then `sessions_path`.
#' In practice, it is recommended to use one `sessions_path`
#' location, but this option exists.
#'
#' For example, `jamsession(grep_sessions("my_session"))`
#' will load the most recent session by `save_date` where any
#' ties prefer the order in `sessions_path`. Similarly,
#' `load_jamsession("my_session")` does the same thing,
#' without the grep_patten search step.
#'
#' @family jamsession sessions
#'
#' @return list containing
#' \describe{
#'    \item{session_df}{`data.frame` of R session information}
#'    \item{session}{`vector` of matching R session names}
#' }
#'
#' @param sessions_path character vector of one or more file paths to search
#'    for saved R sessions. When `NULL`, it uses the output from
#'    `jamsession_paths()$sessions`.
#' @param most_recent logical whether to return only the most recent
#'    saved entry for each session name. When `most_recent` is `FALSE`,
#'    this function returns all saved versions for all R session names.
#' @param session_prefix,session_suffix `character` string used as
#'    prefix or suffix when searching each path in `sessions_path`
#'    for matching file names.
#' @param add_stats `logical` indicating whether to include summary
#'    stats for each session: `number_saved` is the number of past versions
#'    of the session; `total_size` is the total file size for all saved
#'    versions.
#' @param ... additional parameters are ignored
#'
#' @examples
#' list_jamsessions();
#'
#' @export
list_jamsessions <- function
(sessions_path=jamsession_paths()$sessions,
 most_recent=FALSE,
 session_prefix="^inProgress_",
 session_suffix="[.]RData$",
 add_stats=FALSE,
 ...)
{
   ## Purpose is to provide a list of all stored R sessions,
   ## to be subsetted or searched
   if (length(sessions_path) == 0) {
      sessions_path <- jamsession_paths()$sessions;
   }
   session_pattern <- paste0(session_prefix,
      ".+",
      session_suffix);
   session_files <- list.files(path=sessions_path,
      pattern=session_pattern,
      full.names=TRUE);
   if (length(session_files) == 0) {
      return(list(
         session_df=data.frame(
            session=character(0),
            save_date=character(0),
            days_old=integer(0),
            file_size=character(0),
            session_path=character(0),
            session_file=character(0)),
         session=character(0)));
   }

   session_file_info <- jamba::fileInfo(session_files);
   session_file_size <- session_file_info[,"size"];
   session_file_bytes <- file.info(session_files)$size;

   session_files2 <- gsub(
      "_([^_]+)$",
      ":!!:\\1",
      gsub(session_suffix,
         "",
         gsub(session_prefix,
            "",
            basename(session_files))));
   session_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      jamba::rbindList(strsplit(session_files2, ":!!:")));
   colnames(session_df)[1:2] <- c("session", "save_date");
   session_df$days_old <- jamba::dateToDaysOld(session_df$save_date);
   session_df$file_size <- session_file_size;

   session_df[,"session_path"] <- dirname(session_files);
   ## make session_path factor with ordered levels so it
   ## will sort in order of sessions_path
   session_df$session_path <- factor(session_df$session_path,
      levels=unique(c(sessions_path,
         session_df$session_path)));
   session_df[,"session_file"] <- basename(session_files);

   ## Add descriptive stats
   if (add_stats) {
      if (!suppressPackageStartupMessages(require(data.table))) {
         session_dt <- data.table::data.table(session_df);
         session_dt$bytes <- session_file_bytes;
         per_session_df <- data.frame(check.names=FALSE,
            session_dt[,
               list(
                  number_saved=length(save_date),
                  total_size=jamba::asSize(sum(bytes)),
                  days_span=diff(range(days_old))+1),
               by=session]);
      } else {
         number_saved <- lengths(split(session_df$session, session_df$session));
         bytes_per_session <- sapply(split(session_file_bytes, session_df$session), sum);
         days_span <- sapply(split(session_df$days_old, session_df$session), function(i){
            diff(range(i))+1
         })
         per_session_df <- data.frame(session=names(number_saved),
            number_saved=number_saved,
            total_size=jamba::asSize(bytes_per_session[names(number_saved)]),
            days_span=days_span[names(number_saved)]);
      }
   }

   rownames(session_df) <- jamba::makeNames(
      jamba::pasteByRow(session_df[,c("session","save_date"),drop=FALSE]),
      renameFirst=FALSE,
      startN=2);

   if (most_recent && nrow(session_df) > 1) {
      session_df <- jamba::mixedSortDF(session_df,
         byCols=c("session",
            "session_path",
            "days_old"));
      session_unique_match <- match(unique(session_df$session),
         session_df$session);
      session_df <- session_df[session_unique_match,,drop=FALSE];
   }

   if (add_stats) {
      session_df <- merge(session_df,
         per_session_df,
         all.x=TRUE,
         all.y=TRUE);
   }

   return(list("session_df"=session_df,
      "session"=unique(session_df$session)));
}

#' Search for saved R sessions
#'
#' Search for saved R sessions
#'
#' This function searches for saved R sessions using one or more
#' text or regular expression patterns.
#'
#' See `list_jamsessions()` for more details about stored sessions.
#'
#' This function can be chained with `load_jamsession()`, for
#' example `load_jamsession(grep_jamsessions("my_project"))`,
#' and it will load the most recently saved R session.
#'
#' @family jamsession sessions
#'
#' @param pattern chracter pattern used with `grep()` to match session
#'    names.
#' @param sessions_path character vector of one or more file paths to search
#'    for saved R sessions. When `NULL`, it uses the output from
#'    `jamsession_paths()$sessions`.
#' @param return_df logical whether to return a vector (FALSE) or
#'    data.frame (TRUE) with detailed information about each session.
#' @param sort_by_date logical whether to sort results by date, as opposed
#'    to the order retrieved from \code{\link{list.files}}.
#' @param ignore.case logical sent to \code{\link{grep}}, by default TRUE
#'    which searches in case-insensitive mode.
#' @param most_recent logical whether to return only the most recent
#'    saved version of each matching session, by default `TRUE`.
#'    This argument is passed to `list_jamsessions()`.
#' @param include_attrs logical indicating whether to include attributes
#'    `"file_path"`, `"file_size"`, `"save_date"`, and `"days_old"`,
#'    only used when `return_df=FALSE`.
#' @param add_stats `logical` indicating whether to include summary
#'    stats for each session: `number_saved` is the number of past versions
#'    of the session; `total_size` is the total file size for all saved
#'    versions.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to `list_jamessions()`, and
#'    `jamba::provigrep()`, as relevent. If `session_prefix` or
#'    `session_suffix` should be customized, it should be done here.
#'
#' @examples
#' grep_jamsessions(".");
#'
#' @export
grep_jamsessions <- function
(pattern,
 sessions_path=jamsession_paths()$sessions,
 return_df=TRUE,
 sort_by_date=TRUE,
 ignore.case=TRUE,
 most_recent=TRUE,
 include_attrs=FALSE,
 add_stats=FALSE,
 verbose=FALSE,
 ...)
{
   ## Primary purpose is to search through the stored session names for substrings
   session_df <- list_jamsessions(
      sessions_path=sessions_path,
      most_recent=most_recent,
      add_stats=add_stats,
      ...)$session_df;

   ## use jamba::provigrep() to enable multiple grep patterns
   session_grep_v <- jamba::provigrep(patterns=pattern,
      x=session_df$session,
      ignore.case=ignore.case,
      ...);
   session_grep <- subset(session_df, session %in% session_grep_v)
   #session_grep_rows <- unlist(lapply(session_grep_v, function(i){
   #   which(session_df$session %in% i)
   #}))
   #session_grep <- session_df[session_grep_rows,,drop=FALSE];

   if (nrow(session_grep) == 0) {
      return(session_grep);
   }
   if (most_recent) {
      rownames(session_grep) <- session_grep$session;
   } else {
      rownames(session_grep) <- paste0(session_grep$session,
         "_",
         session_grep$save_date);
   }
   ## Optionally Sort for newest first
   if (sort_by_date) {
      session_grep <- jamba::mixedSortDF(session_grep,
         byCols=c("days_old", "session"));
   }

   ## Optionally return the session name
   if (!return_df) {
      session_grep1 <- session_grep$session;
      names(session_grep1) <- rownames(session_grep);
      if (include_attrs) {
         file_path <- file.path(session_grep$session_path,
            session_grep$session_file);
         file_size <- session_grep$file_size;
         save_date <- session_grep$save_date;
         days_old <- session_grep$days_old;
         names(file_path) <- names(session_grep1);
         names(file_size) <- names(session_grep1);
         names(save_date) <- names(session_grep1);
         names(days_old) <- names(session_grep1);
         attr(session_grep1, "file_path") <- file_path;
         attr(session_grep1, "file_size") <- file_size;
         attr(session_grep1, "save_date") <- save_date;
         attr(session_grep1, "days_old") <- days_old;
      }
      return(session_grep1);
   }
   return(session_grep);
}

#' load an R session
#'
#' load an R session
#'
#' This function loads a previously-saved R session, loading the most
#' recently saved R session by the `session`. It also by default
#' sets the R prompt using `jamba::setPrompt()`, to indicate the
#' `session` and help reinforce which R session is in the active
#' environment.
#'
#' The default workflow is to load the R session into the R
#' workspace, that is `envir=globalenv()`, because the target use
#' for `load_jamsession()` is to call this function in a new,
#' empty R session in order to continue work on the saved
#' R session. In this case is the `.Rhistory` file is also
#' available and `load_history=TRUE`, it is loaded as well. Note
#' that loading R history will overwrite the previous active
#' R history.
#'
#' However, when `envir` is defined as something other than `globalenv()`,
#' then the prompt is not altered, and the R session is
#' loaded into the specific environment. In this case the R history
#' is not loaded, even when `load_history=TRUE`.
#' When `assign_session=TRUE`
#' this environment is assigned to a new object in `globalenv()`
#' so that it can be attached with `attach()`.
#'
#' @return invisible list of R object names loaded into the environment.
#'    One could use this vector of R object names to learn which R objects
#'    are stored in an R session, by creating a new environment, loading
#'    an R session into that environment, then inspecting the R object
#'    names.
#'
#' @family jamsession sessions
#'
#' @param session character string corresponding to the `"session"`,
#'    which is matched directly with the `"session"` column output from
#'    `grep_jamsessions()`, or `data.frame` output from `grep_jamsessions()`.
#' @param sessions_path character vector of one or more file paths to search
#'    for saved R sessions. When `NULL`, it uses the output from
#'    `jamsession_paths()$session`. This value is used only when
#'    `session` is a character vector, and is passed to
#'    `grep_jamsessions()`.
#' @param save_date optional character string with a specific date to
#'    use when loading a session. This string is matched with
#'    the `"save_date"` returned by `list_jamsessions()`.
#'    When `save_date` is `NULL`, the most recent
#'    session is used, which is the default behavior.
#' @param envir `environment`, where `"environment" %in% class(envir)`.
#'    By default `envir` uses `globalenv()` which loads the R session
#'    into the workspace. This behavior is intended for new R sessions,
#'    to restore the R session to the state it was in when the R session
#'    was saved. If loading into an active R session, `envir` should
#'    be defined as a separate environment, in order to prevent
#'    R objects in the R session file from overwriting R objects
#'    with the same name in the local R workspace. For example
#'    `session_env=new.env();load_jamsession("New", envir=session_env)`.
#' @param assign_session logical whether to assign `session`
#'    to the `parent.env()` which is typically the global environment
#'    `.GlobalEnv`.
#' @param load_history logical whether to load the .Rhistory file for the
#'    R session, if available.
#' @param do_prompt logical whether to run
#'    `jamba::setPrompt()` to update the R prompt to include the
#'    `session`.
#' @param do_window_title logical whether to update the device options
#'    to define the title based upon the `session`. Currently when `TRUE`,
#'    the options `quartz.options()` and `X11.options()` are defined,
#'    if possible.
#' @param verbose logical whether to print verbose output.
#' @param ... additional arguments are passed to `jamba::setPrompt()`.
#'
#' @export
load_jamsession <- function
(session,
 sessions_path=jamsession_paths()$sessions,
 save_date=NULL,
 envir=globalenv(),
 assign_session=TRUE,
 load_history=TRUE,
 do_window_title=TRUE,
 do_prompt=TRUE,
 verbose=TRUE,
 ...)
{
   ## Purpose is to load a stored R session, given a session name
   ##
   ## TODO:
   ## -- handle the list of packages required for this session
   ##
   ## The envir=.GlobalEnv part is required for the session to be loaded into
   ## a reachable environment for the user, otherwise after this function call it disappears
   ##
   ## By default, the function invisibly returns the list of R object loaded.
   ## So one could load a session inside an R environment, keeping it encapsulated
   ## from the current R session, thus not over-writing any conflict R object names.
   ##    testEnv <- new.env(parent=.GlobalEnv);
   ##    loadSession(grepSession("testSession"), envir=testEnv, loadHistory=FALSE, doSetPrompt=FALSE);
   ##

   ## Remove some extraneous characters
   if (jamba::igrepHas("data.frame|tibble|tbl|matrix", class(session))) {
      session_df <- session;
      if (length(save_date) > 0) {
         use_rows <- (session_df$save_date %in% save_date);
         session_df <- subset(session_df, use_rows);
         if (nrow(session_df) == 0) {
            stop(
               paste0("No R session was found for the given save_date:",
                  save_date));
         }
      }
   } else if (is.atomic(session)) {
      session_df <- grep_jamsessions(pattern=session,
         return_df=TRUE,
         sessions_path=sessions_path,
         most_recent=(length(save_date) == 0) );
      if (any(session %in% session_df$session)) {
         if (length(save_date) > 0) {
            use_rows <- (jamba::pasteByRow(session_df[,c("session", "save_date")], sep="_") %in%
                  paste0(session, "_", save_date));
         } else {
            use_rows <- (session_df$session %in% session);
         }
         session_df <- subset(session_df, use_rows);
      } else {
         session_df <- grep_jamsessions(pattern=gsub("_[0-9]{2}[a-zA-Z]{3}[0-9]{4}$",
            "", session),
            return_df=TRUE,
            sessions_path=sessions_path,
            most_recent=FALSE);
         use_rows <- (jamba::pasteByRow(session_df[,c("session", "save_date")], sep="_") %in%
               session);
         session_df <- subset(session_df, use_rows);
      }
      if (nrow(session_df) == 0) {
         msg <- paste0("Not all sessions were found. Not found:",
            jamba::cPaste(setdiff(session, session_df$session)));
         stop(msg);
      }
   } else {
      stop("Input must be either character vector, or data.frame output from grep_jamsessions()");
   }
   if (!all(c("session_path", "session_file", "session", "save_date") %in%
         colnames(session_df))) {
      stop("Input data did not include expected colnames.");
   }
   if (nrow(session_df) == 0) {
      stop("No R session was found.");
   }

   ## Check environment arguments
   if (length(envir) == 0 && !"environment" %in% class(envir)) {
      stop("Argument 'envir' should be defined, for example envir=globalenv()");
   }
   ## Possibly decide assign_env=TRUE when not envir=globalenv()
   assign_env <- FALSE;
   if (!identical(globalenv(), envir)) {
      load_history <- FALSE;
   }

   loaded_objects <- list();
   if (nrow(session_df) > 1) {
      if (verbose) {
         jamba::printDebug("load_jamsession(): ",
            "loading multiple sessions:",
            session_df$session);
      }
   }
   for (irow in seq_len(nrow(session_df))) {
      session1 <- session_df$session[irow];
      save_date1 <- session_df$save_date[irow];
      rdata_file <- file.path(session_df$session_path[irow],
         session_df$session_file[irow]);
      history_file1 <- gsub("[.]RData",
         ".Rhistory",
         ignore.case=TRUE,
         rdata_file);

      ## Print some text about what is happening
      if (verbose) {
         jamba::printDebug("load_jamsession(): ",
            "Loading session: ",
            rdata_file);
      }

      loaded_objects1 <- list(load(rdata_file,
         envir=envir,
         verbose=FALSE));
      if (!identical(globalenv(), envir)) {
         attr(envir, "name") <- session1;
      }
      if (verbose) {
         jamba::printDebug("load_jamsession(): ",
            "Loaded session:",
            session1,
            " into environment '",
            environmentName(envir), "'");
      }
      names(loaded_objects1) <- session1;
      loaded_objects <- c(loaded_objects,
         loaded_objects1);
      if (assign_env) {
         env_name <- paste0(session1, "_session_env");
         if (verbose) {
            jamba::printDebug("load_jamsession(): ",
               "Assigning to new environment: ",
               env_name);
         }
         ## Assign the environment to the global environment
         assign(env_name,
            envir,
            envir=.GlobalEnv);
      }

      if (load_history && file.exists(history_file1)) {
         loadhistory(history_file1);
         if (verbose) {
            jamba::printDebug("load_jamsession(): ",
               "Loaded history:",
               session1);
         }
      }
   }

   ## Assign this value to the current environment, just to help keep track
   if (assign_session) {
      assign("session",
         session1,
         envir=envir);
   }
   ## Now define the default window title
   if (!identical(globalenv(), envir) && do_window_title) {
      window_title <- paste0("Session: ", session1);
      try({
         grDevices::quartz.options(title=window_title)
      });
      try({
         grDevices::X11.options(title=window_title)
      });
   }

   if (do_prompt) {
      if (verbose) {
         jamba::printDebug("load_jamsession(): ",
            "jamba::setPrompt()");
      }
      jamba::setPrompt(projectName=session1,
         ...);
   }
   invisible(list(loaded_objects=loaded_objects));
}

#' save an R session
#'
#' save an R session
#'
#' This function saves the current R session to a `.RData` file,
#' and the R history to a `.Rhistory` file. It saves these files
#' to the first write-accessible path in `sessions_path`.
#'
#' When `envir` is defined as something other than `globalenv()`,
#' only the R objects stored in that environment are saved.
#' However the R objects are saved as R objects, with no information
#' about the source environment.
#'
#' By default this function will assign the value of `session`
#' to `session` in the global environment, unless `assign_session=FALSE`,
#' in order to reinforce the active R session name, and which
#' is shown by the R prompt by default, via calling `jamba::setPrompt()`.
#' These changes are intended as visual reminders of the active R
#' session.
#'
#' @family jamsession sessions
#'
#' @param session `character` session name used to name the .RData file
#' @param assign_session logical whether to assign `session` in the
#'    global environment `.GlobalEnv`, to reinforce the active session.
#'    It is typically not advised to update the `.GlobalEnv` inside a
#'    function, however for typical use of `load_jamsession()` the
#'    intended result is to provide an consistent R session, and
#'    `session` is part of that expectation.
#' @param date the date string to use when naming the .RData file.
#'    By default the current date is used, called by `jamba::getDate()`.
#' @param do_timestamp logical whether to run `utils::timestamp()` so the
#'    current time and date are written into the `.Rhistory` file.
#' @param sessions_path character vector of one or more file paths to search
#'    for saved R sessions. When `NULL`, it uses the output from
#'    `jamsession_paths()$sessions`.
#' @param do_prompt logical whether to run `jamba::setPrompt()`
#'    to set the R prompt to include the `session`.
#' @param do_window_title logical whether to set the default device
#'    window title to include the `session`.
#' @param save_history logical whether to save the R command history
#'    into an `.Rhistory` file. Note that the full available R
#'    command history is saved, but this command history is subject
#'    to platform-specific limitations, and thus may not contain the
#'    entire R command history.
#' @param envir R environment to save, by default the active global
#'    environment `.GlobalEnv`.
#' @param verbose logical whether to print verbose output.
#' @param ... additional arguments passed to `base::save()`
#'
#' @export
save_jamsession <- function
(session=get("session", envir=envir),
 assign_session=TRUE,
 save_date=jamba::getDate(),
 do_timestamp=TRUE,
 sessions_path=jamsession_paths()$sessions,
 do_prompt=TRUE,
 do_window_title=TRUE,
 save_history=TRUE,
 envir=.GlobalEnv,
 session_prefix="inProgress_",
 session_suffix=".RData",
 verbose=TRUE,
 ...)
{
   ## Purpose is to store the .RData and .Rhistory files somewhere central,
   ## so they're not hidden files in some subdirectory and not easily found.
   ## The basic approach is to save sessions with a useful session name,
   ## and that session name will be used to find past sessions again later.
   ##
   ## TODO:
   ## -- handle the list of packages required for this session
   ##
   ## -- add the list of R objects stored in this session, in case they're
   ##    useful to be searched later (and because R by default doesn't store
   ##    this list separate from the RData file itself, which requires you to
   ##    load all data before seeing the list of R objects contained inside.
   ##    (Design idea: use the ll() function but include all objects, then
   ##    store as a tab-delimited text file.)
   ##

   ## Remove some extraneous characters
   session1 <- gsub("[._ /\\]+", "-", session);
   if (!session1 == session) {
      jamba::printDebug("save_jamsession(): ",
         c("session changed from '",
            session,
            "' to '",
            session1,
            "'"),
         sep="");
      session <- session1;
   }

   # Guess the date if not explicitly given
   if (length(save_date) == 0) {
      save_date <- jamba::getDate();
   }

   ## Optionally write timestamp to the history file
   if (do_timestamp) {
      utils::timestamp();
   }

   ## remove regular expression patterns if present
   session_prefix <- gsub("^[\\^]|[[]|]", "", session_prefix);
   session_suffix <- gsub("[[]|]|[$]", "", session_suffix);

   ## iterate session_path to use the first writeable directory
   for (use_session_path in sessions_path) {
      session_file <- paste0(
         path.expand(use_session_path),
         "/",
         session_prefix,
         session,
         "_",
         save_date,
         session_suffix);
      save_success <- tryCatch({
         save(
            list=ls(all.names=TRUE,
               envir=envir),
            file=session_file,
            envir=envir);
         TRUE
      }, error=function(e){
         FALSE;
      });
      if (save_success) {
         break;
      }
   }
   if (verbose) {
      jamba::printDebug("save_jamsession(): ",
         c("Saved jamsession '",
            session,
            "' to sessions_path '",
            use_session_path,
            "'"),
         sep="");
      jamba::printDebug("save_jamsession(): ",
         "session_file:",
         session_file);
   }

   if (save_history && interactive()) {
      ## define historyFile based upon sessionFile
      history_file <- gsub(paste0(session_suffix, "$"),
         ".Rhistory",
         session_file);
      tryCatch({
         savehistory(history_file);
         if (verbose) {
            jamba::printDebug("save_jamsession(): ",
               c("Saved history for '",
                  session,
                  "' to sessions_path '",
                  use_session_path,
                  "'"),
               sep="");
         }
      }, error=function(e){
         cat("\n\nerror saving history file:'",
            history_file,
            "\n");
      });
   }

   if (assign_session) {
      assign("session",
         session,
         envir=envir);
   }

   ## Now define the default window title
   if (do_window_title) {
      window_title <- paste0(session, " %d");
      try(
         grDevices::quartz.options(title=window_title)
      )
      try(
         grDevices::X11.options(title=window_title)
      )
   }

   if (do_prompt) {
      jamba::setPrompt(projectName=session);
   }
}


#' show versions saved for a session
#'
#' show versions saved for a session
#'
#' @family jamsession sessions
#'
#' @param session `character` indicating the session name.
#' @param session_path `character` vector of one or more file paths to search
#'    for saved R sessions.
#' @param most_recent `logical` whether to return only the most recently
#'    saved entry for each matching session name. If FALSE, it returns all
#'    saved versions of all R sessions.
#' @param ... additional arguments are passed to `grep_jamsessions()`.
#'
#' @export
show_session_versions <- function
(x=get("session", envir=.GlobalEnv),
 sessions_path=jamsession_paths()$sessions,
 most_recent=FALSE,
 return_df=TRUE,
 ...)
{
   ## Purpose is to show session information for the current session, in order to
   ## see things like last time saved, and the sizes of each session save.
   sdf <- grep_jamsessions(x,
      most_recent=most_recent,
      return_df=return_df,
      ...);
   if (length(sdf) > 0 && "days_old" %in% colnames(sdf)) {
      sdf[,"days_old"] <- jamba::dateToDaysOld(sdf[,"save_date"]);
   }
   sdf;
}
