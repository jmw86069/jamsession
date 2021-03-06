% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamsession_init.R
\name{jamsession_paths}
\alias{jamsession_paths}
\title{jamsession file paths}
\usage{
jamsession_paths(
  sessions = NULL,
  objects = NULL,
  functions = NULL,
  create = FALSE,
  mode = "0755",
  recursive = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{sessions, objects, functions}{character vector with one or more
file directory path locations, suitable for use by \code{list.files(path=x)}.
When a value is defined, it is used to override the existing
\code{options()} value, so the updated value will be used for all
related jamsession functions. Note that any value \code{""} is removed,
for example \code{sessions=""} is equivalent to \code{sessions=NULL}.}

\item{create}{logical indicating whether to create a file path
that does not already exist. The directory is created using
\code{dir.create()} using the arguments \code{mode} and \code{recursive}.}

\item{mode, recursive}{arguments passed to \code{dir.create()} when
necessary, by the argument \code{create}.}

\item{verbose}{logical indicating whether to print verbose output.}

\item{...}{additional arguments are passed to \code{dir.create()}.}
}
\value{
\code{list} is returned, with named elements
\code{"sessions"}, \code{"objects"}, and \code{"functions"}.
}
\description{
jamsession file paths
}
\details{
This function retrieves the file path for each jamsession type:
\enumerate{
\item \code{"sessions"} - the folder where R sessions are saved,
this path is also stored in \code{getOption("jam.sessions_path")}.
\item \code{"objects"} - the folder where R objects are saved,
this path is also stored in \code{getOption("jam.objects_path")}.
\item \code{"functions"} - the folder where R functions are saved,
this path is also stored in \code{getOption("jam.functions_path")}.
}

To use custom file paths in each R session, use either \verb{~/.Rprofile}
or an appropriate method to define the relevant options:
\itemize{
\item \code{options("jam.sessions_path"="/some/specific/path/sessions")},
\item \code{options("jam.objects_path"="/some/specific/path/objects")},
\item \code{options("jam.functions_path"="/some/specific/path/functions")}.
}

When a file path is not defined in \code{options()}, the corresponding
default path is used:
\itemize{
\item \code{"sessions"} uses default \code{"~/Projects/R-sessions"}
\item \code{"objects"} uses default \code{"~/Projects/R-objects"}
\item \code{"functions"} uses default \code{"~/Projects/R-scripts"}
}

(Note \code{~} refers to the home directory, which may also be defined
by \verb{$HOME} depending upon the computer operating system, for
example linux, Mac OSX, or Microsoft Windows.)
}
\examples{
## Display the current or default paths
c(jamsession_paths(functions=NULL))

## Update the functions path
jamsession_paths(functions="~/Projects/R-functions")

## confirm the functions path has been updated
c(jamsession_paths())
getOption("jam.functions_path")

## Revert the functions path
c(jamsession_paths(functions="~/Projects/R-scripts"))

}
\seealso{
Other jamsession utilities: 
\code{\link{print_pkg_description}()}
}
\concept{jamsession utilities}
