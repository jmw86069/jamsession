
#' Load or refresh R file functions as a temporary package
#'
#' Load or refresh R file functions as a temporary package
#'
#' This function loads an R file that contains custom R functions,
#' as if that R file were part of a temporary R package.
#'
#' It assumes the R file or files contain only valid R functions,
#' which are optionally documented using the `roxygen2` style
#' described in the `roxygen2` package.
#'
#' This mechanism of loading R functions is an improvement over
#' using `base::source()` or `base::sys.source()` in these ways:
#'
#' 1. Help documents for each function will be prepared, if
#'    they were defined using `roxygen2` format for each function.
#' 2. R functions are loaded into a namespace, separate from the
#'    `.GlobalEnv` workspace. When saving
#'    the R session itself, the custom functions will not be
#'    included, because they are not part of the workspace.
#' 3. When the custom R functions are updated, repeat the
#'    call to this function and it will replace the namespace
#'    with the newer namespace.
#' 4. The package is included in `sessionInfo()` which helps
#'    indicate that custom functions were also included.
#'
#' Aside: It really is fast and easy to create a new R package,
#' however it makes sense that small one-off projects do not
#' justify creating a whole new R package for only one or
#' two custom R functions. However, the temporary R package
#' created by this function can be used to create an actual
#' R package, by copying the temporary folder, or by supplying
#' a specific folder with the argument `use_tempdir`.
#'
#' To find the temporary R package folder:
#'
#' `system.file(package=pkg_name)`
#'
#' Ideally you should also use version control like Git, subversion,
#' CVS, or a similar system. But for one-off projects, this mechanism
#' is a good start -- and we think it will become a gateway to
#' creating proper R packages as needed.
#'
#' Note that this function `refresh_functions()` does not validate
#' the `.R` file, nor does it check whether the R file is
#' valid to be part of an R package. However, because `refresh_functions()`
#' also calls `roxygen2::roxygenize()`, it inherits a lot of
#' validation from that process and will display error messages
#' as relevant. At this point, those error messages are intended
#' to be a *good thing* because they make syntax errors visible.
#'
#' Note that this function does not itself remove the temporary R package
#' directory, it assumes that when `use_tempdir` is defined by
#' `base::tempdir()`, see that function help for more information.
#' Note that on some linux systems, temporary files not accessed for
#' more than 7 days may be deleted automatically.
#'
#' Note that multiple files can be used as input to `session`, in which
#' case the `pkg_name` argument can be used to define a specific package
#' name for the full collection of R functions in the files.
#'
#' @family jamsession functions
#'
#' @param session `character` vector of one or more function files
#'    to load. Note than when multiple functions are loaded, they
#'    are combined into the same temporary package.
#' @param pkg_name `NULL` or `character` string indicating the name
#'    to use for the temporary package. By default it uses the first
#'    `session`, and if there are multiple `session` files then
#'    it appends the number of files as a suffix.
#' @param functions_path character vector of one or more file paths to search
#'    for saved R functions. When `NULL`, it uses the output from
#'    `jamsession_paths()$functions`. When `session` matches multiple
#'    files, then the most recently modified file is used, as defined
#'    by `jamba::newestFile()`.
#' @param fn_pattern `character` pattern appended to `session` to
#'    match compatible R files. The default format is `"session_functions.R"`
#'    and so default `fn_pattern="_functions.R"`.
#' @param define_defaults `logical` indicating whether to define
#'    `options("usethis.description")` using jamsession default values.
#'    If `getOption("usethis.description")` is not defined, then
#'    default values are used regardless. Otherwise, the
#'    values from `getOption("usethis.description")` are only modified
#'    when `default_defaults=TRUE`.
#'    As the R package is only temporary, these defaults have little
#'    actual impact. Just be sure to edit the DESCRIPTION file if
#'    you intend to use the R package long term.
#' @param pkg_suffix `NULL` or `character` string used as a suffix,
#'    appended to the end of the temporary package name. The `pkg_suffix`
#'    or `pkg_name` are useful to avoid name conflicts with proper R
#'    packages. For example `"igraph_functions.R"` should be not
#'    be loaded as `"igraph"` or else it will conflict with the actual
#'    `"igraph"` R package.
#' @param use_tempdir `character` path, or function which returns a valid
#'    file path suitable to create a subdirectory for this temporary
#'    R package. By default `base::tempdir()` returns a single temporary
#'    directory for each R session, note that this directory may be deleted
#'    when the R session is ended.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' # create two temporary functions
#' tempfn <- function(x){x}
#' another_tempfn <- function(x){length(x)}
#'
#' # save these functions to a temporary file
#' # tempfns_functions.R
#' tempfn_file <- file.path(tempdir(), "tempfns_functions.R");
#' dump(c("tempfn", "another_tempfn"), file=tempfn_file);
#'
#' # remove the functions from the environment
#' rm(tempfn);
#' rm(another_tempfn);
#'
#' # load the functions
#' refresh_functions("tempfns", functions_path=tempdir())
#'
#' # the tempfn() function is now inside a package
#' find("tempfn")
#' #> package:tempfns
#'
#' # the function can be called
#' tempfn(c("one", "two"))
#' #> [1] "one" "two"
#'
#' @export
refresh_functions <- function
(session,
 pkg_name=NULL,
 functions_path=jamsession_paths()$functions,
 fn_pattern="_functions.R$",
 define_defaults=FALSE,
 pkg_suffix="",
 use_tempdir=base::tempdir(),
 verbose=TRUE,
 ...)
{
   if (define_defaults || length(getOption("usethis.description")) == 0) {
      if (verbose > 1) {
         jamba::printDebug("refresh_functions(): ",
            "Defined package defaults for: ",
            c("Author", "License"));
      }
      options(
         usethis.description=list(
            `Authors@R`='person("jam", "jamsession", role=c("aut", "cre"))',
            License="MIT"
         ));
   }

   ## Process one or more session input values
   fn_files <- lapply(session, function(isession){
      fn_file <- list.files(path=functions_path,
         pattern=paste0("^",
            isession,
            fn_pattern),
         full.names=TRUE);
      if (verbose > 1) {
         jamba::printDebug("refresh_functions(): ",
            "fn_file matched: ",
            collapse="\n",
            fn_file)
      }
      if (length(fn_file) == 0) {
         stop(paste0("Function file not found for: '",
            isession,
            "'"));
      }
      if (length(fn_file) > 1) {
         fn_file <- jamba::newestFile(fn_file);
         if (verbose > 1) {
            jamba::printDebug("refresh_functions(): ",
               c("Choosing most recently-modified file:'",
                  fn_file, "'"),
               sep="");
         }
      } else {
         if (verbose > 1) {
            jamba::printDebug("refresh_functions(): ",
               c("Found file:'",
                  fn_file, "'"),
               sep="");
         }
      }
      fn_file;
   });
   if (length(pkg_name) == 0) {
      if (length(session) == 1) {
         pkg_name <- paste0(session,
            pkg_suffix);
      } else {
         pkg_name <- paste0(head(session, 1),
            ".",
            length(session),
            "files",
            pkg_suffix);
      }
   }

   ## usethis for temporary package and roxygen2 docs
   #rfile <- path.expand("~/Projects/R-scripts/groupedcolor_functions.R");
   #rstem <- gsub("_functions.R$", "", basename(fn_file));
   rdesc <- paste0(pkg_name,
      " - jamsession package from ",
      jamba::cPaste(session, sep=","),
      " at ",
      as.character(Sys.time()));
   rdesc2 <- paste0(pkg_name,
      " - jamsession temporary package from:\n   ",
      jamba::cPaste(unlist(fn_files), sep="\n   "),
      "\n   created ",
      Sys.time());
   rtempdir <- file.path(use_tempdir,
      pkg_name);
   rrdir <- file.path(rtempdir, "R");

   ## By default, remove previous incarnation of the temp package
   if (dir.exists(rtempdir)) {
      if (verbose > 1) {
         jamba::printDebug("refresh_functions(): ",
            c("Removed previous temp dir: '",
               rtempdir, "'"),
            sep="");
      }
      unlink(rtempdir,
         recursive=TRUE);
   }

   ## Create temp package, capture output to keep things quiet
   c1 <- capture.output(
      usethis::create_package(rtempdir,
         fields=list(Package=pkg_name,
            Title=rdesc,
            Description=rdesc2,
            Roxygen="list(markdown = TRUE)"),
         open=FALSE,
         rstudio=FALSE))
   for (ifn_file in fn_files) {
      base::file.copy(from=ifn_file,
         to=rrdir);
   }
   if (verbose > 1) {
      jamba::printDebug("refresh_functions(): ",
         c("Created temporary package: '",
            rtempdir,
            "'"),
         sep="");
   }

   ## Load roxygen2 help docs which also loads the namespace
   ## as a virtual R package
   c2 <- capture.output(
      suppressMessages(roxygen2::roxygenize(rtempdir)));
   ## functions loaded
   ## remove library.dynam.unload() which is added by roxygen2
   fns_loaded <- setdiff(
      ls(paste0("package:", pkg_name)),
      c("library.dynam.unload"));
   if (verbose) {
      jamba::printDebug("refresh_functions(): ",
         c("Loaded temporary package as '",
            basename(rtempdir), "'"),
         sep="");
   }
   if (verbose > 1) {
      fns_vector <- format(paste0(fns_loaded, "()"));
      ncol <- max(c(1,
         floor(getOption("width") / (nchar(head(fns_vector, 1)) + 2))));
      added_num <- ncol - (length(fns_loaded) %% ncol);
      if (added_num == ncol) {
         added_num <- 0;
      }
      added <- rep("", added_num);
      jamba::printDebug("refresh_functions(): \n     ",
         sep="\n     ",
         jamba::pasteByRow(sep="  ",
            matrix(ncol=ncol,
               byrow=FALSE,
               c(format(paste0(fns_loaded, "()")), added))
            )
         );
   }
   invisible(fns_loaded);
}

#' Print package DESCRIPTION
#'
#' Print package DESCRIPTION
#'
#' This function is a simple wrapper around functions to
#' find the `DESCRIPTION` file for an installed R package,
#' and print lines to the console.
#'
#' When the package is not installed, or a temporary R package
#' is not in the search path, error messages are returned accordingly.
#'
#' @param pkg `character` string indicating a package name.
#'
#' @family jamsession internals
#'
#' @examples
#' print_pkg_description("base")
#'
#' @export
print_pkg_description <- function
(pkg)
{
   desc_file <- system.file("DESCRIPTION",
      package=pkg);
   if (length(desc_file) > 0 && nchar(desc_file) > 0) {
      cat(paste(readLines(desc_file), collapse="\n"),
         "\n");
   } else {
      if (!pkg %in% search()) {
         stop(paste0("Package is not in search(): '",
            pkg, "'"));
      }
      stop(paste0("DESCRIPTION not found for package '",
         pkg, "'"));
   }
}
