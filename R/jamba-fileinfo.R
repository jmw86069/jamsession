#
# jamba graveyard functions

#' file information in data.frame format
#'
#' file information in data.frame format
#'
#' This function is a minor extension to `file.info()` in that it
#' adds the filename as a proper colname, and "size" which contains a text
#' file size.
#'
#' @return `data.frame` with file information, including "filename" and "size"
#'    as additional colnames as compared to `file.info()` output.
#'
#' @param fileList `character` vector with one or more file paths.
#' @param ... Additional arguments are ignored.
#'
#' @family jamsession internals
#'
#' @export
fileInfo <- function
(fileList,
   ...)
{
   # Purpose is to wrapper file.info() so it returns a pretty tabular
   # summary for one or more files.
   # One cool pattern to follow is to list files contained within a package:
   # colsHead(fileInfo(list.files(path=find.package("jamba"), full.names=TRUE)))
   fileList <- path.expand(fileList);
   fi1 <- file.info(fileList, ...);

   # convert size to readable label
   fi1[,"size"] <- jamba::asSize(fi1[,"size"]);

   fi1 <- data.frame(
      check.names=FALSE,
      stringsAsFactors=FALSE,
      "filename"=rownames(fi1),
      fi1);

   # Left-justify the text by right-padding with spaces
   # making it easier to read
   fi1$filename <- format(
      fi1$filename,
      justify="left")

   return(fi1);
}
