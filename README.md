shinyDTQ is designed to preview data.table query intermediate steps while chaining `DT[...][...][...]`.
Should not be used with `:=` yet.

# Installation

```r
library(devtools)
library(shiny)
library(shinyTree) # mandatory: install_github("trestletech/shinyTree")
library(data.table) # optional: install_github("jangorecki/data.table",ref="timing")

DT = data.table(x = 1:4, y = letters[1:4], key="x")
jn = data.table(x = 2:4, z = c(TRUE,FALSE,TRUE), key="x")
options("datatable.timing"=TRUE) # optional
DT[, list(x,y=paste(y,y,sep="_"))
   ][x %in% c(1:4), list(x,y)
     ][jn[z==TRUE], nomatch=0L
       ][, list(x,y,z)]

## if you are not using data.table@timing branch you need to prepare the call for shiny app by wrapping into `.dtq = quote()` function
.dtq = quote(DT[, list(x,y=paste(y,y,sep="_"))][x %in% c(1:4),list(x,y)][jn[z==TRUE],nomatch=0L][,list(x,y,z)])

# runApp from github
runGitHub("jangorecki/shinyDTQ")
# or locally after: git clone https://github.com/jangorecki/shinyDTQ.git
runApp(appDir)
```
