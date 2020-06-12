
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jamsession

The goal of jamsession is to help manage

  - multiple R sessions,
  - associated R data objects, and
  - custom R functions.

## Overview

### Load custom R functions on the fly.

> `refresh_functions("project")`

Functions are loaded inside a temporary R package, separate from your R
workspace.

### Save and re-use R objects across projects.

> `save_object("some_fancy_object")`

> `load_object("some_fancy_object")`

Objects are loaded into the user environment, but can be loaded into a
separate environment if needed.

### Save R session for rapid re-use in future.

> `save_jamsession("myproject")`

> `load_jamsession("myproject")`

The full R session and R history is loaded (if available) into the user
workspace. It can be loaded into a separate environment if needed, in
which case the history is not loaded.

## How to install

``` r
# install.packages(remotes)
remotes::install_github("jmw86069/jamsession")
```

## Who might use jamsession?

Have you ever been asked a follow-up question about your R analysis
results?

  - Did it involve repeating the R workflow, or loading the R session
    from an `.RData` file?

Do you sometimes create useful R data objects that you later re-use in
other R analysis projects?

  - And did that R object data not fit comfortably into a text file?

Do you create and modify custom functions for your projects, before they
may (or may not) become part of an R package?

  - And do you need a safe method of refreshing the function while you
    make updates to the function?

My answers are YES, YES, YES\! (And YES for each of the sub-questions.)

## The solution

### R sessions

  - Central place to store R sessions.
  - Convenient way to find past R sessions, re-load, then perform
    follow-up analysis work.
  - Option to load an R session inside an environment (protect your
    workspace as needed)

### R data objects

  - Central place to store important, re-usable R data objects.
  - Convenient way to load saved R data object into any active R
    session.
  - Option to load R object inside an environment (protect your
    workspace as needed)

### Custom R functions

  - Central place to store R files with custom functions.
  - Convenient way to load and refresh R functions.
  - Option to document functions using `roxygen2`.

## Package Reference

A full online function reference is available via the pkgdown
documentation:

[Full jamsession command
reference](https://jmw86069.github.io/jamsession)

## Background

### R projects

For me, it is *routine* to get asked follow-up questions, sometimes a
year or more after the original analysis. Sometimes a manuscript
reviewer asked us to re-cluster data with different distance metric; or
make the font size larger on a figure; or try a new method on the
results…

I need a rapid way to re-load a past R session, with all the supporting
R objects in that session, so I can continue work right where I left
off.

> First time analyzing data for a new project…

``` r
library(jamsession);
## Some analysis work
lotty_dah <- data.frame(A=1:10, B=LETTERS[1:10]);

## Save the project at the end
save_jamsession("LottyDah");
#> ##------ Fri Jun 12 14:09:02 2020 ------##
#> ##  (14:09:02) 12Jun2020:  save_jamsession(): Saved jamsession 'LottyDah' to sessions_path '~/Projects/R-sessions' 
#> ##  (14:09:03) 12Jun2020:  save_jamsession(): session_file:/Users/wardjm/Projects/R-sessions/inProgress_LottyDah_12jun2020.RData
```

> Deliver results to Dr. Dah, who is very excited, reviews results for a
> few weeks, then responds, “Hey what about C?” Meanwhile, I’ve been
> working on other projects, but I can answer with confidence: “Sure
> thing, let me just load the project and look into C.”

``` r
## Look for Lotty's project
grep_jamsessions("Lotty");
```

<div class="kable-table">

|          | session  | save\_date | days\_old | file\_size | session\_path                     | session\_file                         |
| :------- | :------- | :--------- | --------: | :--------- | :-------------------------------- | :------------------------------------ |
| LottyDah | LottyDah | 12jun2020  |         0 | 1.1 kb     | /Users/wardjm/Projects/R-sessions | inProgress\_LottyDah\_12jun2020.RData |

</div>

``` r
#> "LottyDah"

load_jamsession("LottyDah");
#> ##  (14:09:03) 12Jun2020:  load_jamsession(): Loading session: /Users/wardjm/Projects/R-sessions/inProgress_LottyDah_12jun2020.RData 
#> ##  (14:09:03) 12Jun2020:  load_jamsession(): Loaded session:LottyDah into environment 'R_GlobalEnv' 
#> ##  (14:09:03) 12Jun2020:  load_jamsession(): jamba::setPrompt()

## booyah
lotty_dah$C <- letters[1:10];

## Save the updated project
save_jamsession();
#> ##------ Fri Jun 12 14:09:03 2020 ------##
#> ##  (14:09:03) 12Jun2020:  save_jamsession(): Saved jamsession 'LottyDah' to sessions_path '~/Projects/R-sessions' 
#> ##  (14:09:03) 12Jun2020:  save_jamsession(): session_file:/Users/wardjm/Projects/R-sessions/inProgress_LottyDah_12jun2020.RData
```

> Dr. Lotty Dah I’m sure is thinking: “Wow that was fast, thanks so
> much\!”

### R data objects

I also very often create very useful reference data, that is not easy to
save in a text file.

For example, Bioconductor package `GenomicRanges` defines an object
`GenomicRangesList` that can store gene exons, genomic coordiantes, and
associated annotations at the gene, and gene-exon level. It takes
non-trivial time to build a proper data object, and is not easily stored
in a text file. (Ask a Bioinformatician about GTF and GFF3 files…)

Once the data is built, I really just want to save the R object
directly, and re-use it in subsequent R sessions. I do this a lot.

> The code below is not evaluated, but used as an example.

``` r
## Load Bioconductor package GenomicFeatures
library(GenomicFeatures)

## This step takes a few minutes
txdb_hg19_v32 <- GenomicFeatures::makeTxDbFromGFF("Gencode_hg19_v32.gtf")

## This step may also take a few minutes
exonsByGene_hg19_v32 <- GenomicFeatures::exonsBy(txdb_hg19_v32, by="gene")

## Add gene_id to each exon
values(exonsByGene_hg19_v32@unlistData)$gene_id <- rep(names(exonsByGene_hg19_v32),
   lengths(exonsByGene_hg19_v32));

## Some more fancy annotation stuff

## Whoa finally, save this result!
save_object("exonsByGene_hg19_v32");
```

> For another project, I confidently say to myself: “Hey let me just
> re-use that cool gene-exon data I spent the time to make. Hmm… What
> did I call that?”

``` r
## Let me look for exonsByGene
grep_objects("exonsByGene");
#> "exonsByGene_hg19_v27"
#> "exonsByGene_hg19_v28"
#> "exonsByGene_hg19_v32"
#> "exonsByGene_mm10_vM17"
#> "exonsByGene_mm10_vM24"

## Oh yeah I need hg19 version 32
load_object("exonsByGene_hg19_v32");

## Data is loaded in a few seconds
```

### Custom R functions

Like many R analysts, I realize that a custom R function can make a lot
of workflows much cleaner and more consistent. For a complex series of
steps, I usually create an R function to perform those steps, then call
the R function each time it’s needed.

Sadly, I never create the perfect function the first time. I usually add
features as they occur to me, or as I notice the many bugs in that
function. (The majority of my code seems to be error-checking.) So I
need a fast, reliable way to update the R function, refresh in my
environment, then try again.

> I create a file, `"myproject_functions.R"`, and a custom function
> `mydim()` to that file.

Note that for this example, I save the file in `tempdir()`, then I point
that path with argument `functions_path=tempdir()`. For routine use,
`functions_path` is already defined, and I save all my R functions to
one known file path.

``` r
## Create custom function
mydim <- function(x){dim(x)}

## Save it to a file
fn_file <- file.path(tempdir(), "myproject_functions.R");
dump(c("mydim"), file=fn_file);
rm(mydim);

## Now I can refresh_functions() to load that file
refresh_functions("myproject", functions_path=tempdir());
#> ##  (14:09:04) 12Jun2020:  refresh_functions(): Loaded temporary package as 'myproject'
```

> Now what’s nice is that you can see the package using `search()`, and
> use `find()` to find the function to confirm.

``` r
## See "myproject" as one of the packages
print(search())
#>  [1] ".GlobalEnv"           "devtools_shims"       "package:myproject"   
#>  [4] "package:S4Vectors"    "package:BiocGenerics" "package:parallel"    
#>  [7] "package:stats4"       "package:matrixStats"  "package:colorspace"  
#> [10] "package:farver"       "package:rstudioapi"   "package:crayon"      
#> [13] "package:jamsession"   "package:stats"        "package:graphics"    
#> [16] "package:grDevices"    "package:utils"        "package:datasets"    
#> [19] "package:methods"      "Autoloads"            "package:base"

## use find() to locate mydim()
find("mydim")
#> [1] "package:myproject"

## Run the function
mydim(10);
#> NULL

## OOPS I forgot dim() doesn't work on vectors...
```

> Oops, I realize `mydim()` should handle `vector` different than
> `data.frame`. So I edit `"myproject_functions.R"` and edit the
> `mydim()` function.

``` r
mydim <- function(x){adim <- dim(x);if (length(adim) == 0) {length(x)} else {adim}}
fn_file <- file.path(tempdir(), "myproject_functions.R");
dump(c("mydim"), file=fn_file);
rm(mydim);

refresh_functions("myproject", functions_path=tempdir());
#> ##  (14:09:05) 12Jun2020:  refresh_functions(): Loaded temporary package as 'myproject'

mydim(10);
#> [1] 1

mydim(matrix(1:4, ncol=2))
#> [1] 2 2
```

> There are other convenient benefits. If you include `roxygen2` help
> documentation for your function, it can be accessed with `? mydim`

> What if you rename the function? Not a problem, the package is
> reloaded and the previous function name is no longer present.

``` r
mysize <- function(x){adim <- dim(x);if (length(adim) == 0) {length(x)} else {adim}}
dump(c("mysize"), file=fn_file);
rm(mysize);

refresh_functions("myproject", functions_path=tempdir());
#> ##  (14:09:05) 12Jun2020:  refresh_functions(): Loaded temporary package as 'myproject'

find("mydim");
#> character(0)

find("mysize");
#> [1] "package:myproject"
```

Notice that `mydim()` is no longer found, but `mysize()` is found.
