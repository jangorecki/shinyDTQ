library(devtools)
library(shiny)
library(shinyTree)
library(data.table) # install_github("jangorecki/data.table",ref="timing")
`[.data.table` <- data.table:::`[.data.table`

# DT <- data.table(x = 1:4, y = letters[1:4], key="x")
# jn <- data.table(x = 2:4, z = c(TRUE,FALSE,TRUE), key="x")
# options("datatable.timing"=TRUE); dtt(purge=TRUE)
# DT[, list(x,y=paste(y,y,sep="_"))][x %in% c(1:4),list(x,y)][jn[z==TRUE],nomatch=0L][,list(x,y,z)]

# dtq recursive -----------------------------------------------------------

# turns calls `[` into lists
decall.dtq <- function(x){
  if(is.call(x)){
    if(x[[1L]]==as.name("[") || x[[1L]]==as.name("[.data.table")) x <- as.list(match.call(match.fun("[.data.table"), x)) else return(x)
  } else return(x)
  nms <- names(x)
  for(nm in nms[nms %in% c("x","i")]) x[[nm]] <- decall.dtq(x[[nm]])
  x
}

# construct lists of `[` to calls
recall.dtq <- function(x){
  if(!is.list(x)) return(x)
  nms <- names(x)
  for(nm in nms[nms %in% c("x","i")]) x[[nm]] <- recall.dtq(x[[nm]])
  as.call(x)
}

# returns integer number of subsets `[["x"]]` required to access initial data.table using: `DTQ[[rep("x",1L)]]`
length.dtq <- function(x, rc.limit=getOption('datatable.dtq.rc.limit',20L)){
  stopifnot(is.list(x))
  len <- 0L
  for(i in seq.int(rc.limit)){
    if(i > rc.limit) stop("recursive call limit exceeded, use: options('datatable.dtq.rc.limit')")
    if(is.name(x[[rep('x',i)]])){
      len <- i
      break
    }
  }
  len
}

# list with all sub lists, each list is independent from the others
list.dtq <- function(x){
  stopifnot(is.list(x))
  len.dtq <- length.dtq(x)
  stopifnot(len.dtq > 0L)
  lapply(len.dtq:1L, function(i) x[[rep("x",i)]])
}

# flat lists of top level provided list in sequence of calls, to print whole chain use `print.list.dtq`
print.dtq <- function(x){ # [ ] TO DO deparse
  if(!is.list(x)){
    if(is.language(x)) return(paste(deparse(x,width.cutoff = 500L),collapse="\n")) else return(x)
  }
  nms <- names(x)
  for(nm in nms[nms %in% c("i")]) x[[nm]] <- recall.dtq(x[[nm]])
  for(nm in nms[!(nms %in% c("","x"))]) x[[nm]] <- paste(deparse(x[[nm]],width.cutoff = 500L),collapse="\n")
  x[!(nms %in% c("","x"))]
}

# flat list with no nested lists, each lists requires `x` from previous list to be `eval(as.call(`
print.list.dtq <- function(x){
  stopifnot(is.list(x))
  len.dtq <- length.dtq(x)
  stopifnot(len.dtq > 0L)
  l <- lapply(len.dtq:1L, function(i) print.dtq(x[[rep("x",i)]]))
  transformation_nms <- sapply(l, function(x) paste0("[ ",paste(names(x),collapse=", ")," ]"))
  names(l) <- c("#0 : data.table",paste0("#",seq.int(length(transformation_nms)-1L)," : ",transformation_nms[-1L]))
  lapply(l, function(x){
    names(x) <- paste(names(x),":",x)
    x
    })
}

# dtq ---------------------------------------------------------------------

dtq <- if(exists(".dtq")) .dtq else{
  if(!exists("dtt")) stop("Catch data.table query into `.dtq <- quote(DT[...][...])` or use 'jangorecki/data.table@timing' branch and `options('datatable.timing'=TRUE)` to runApp using your recently used data.table query.")
  dtq <- dtt(pretty=FALSE,last=1)$dtq[[1L]]
}
stopifnot(is.call(dtq))

DTQ <- decall.dtq(dtq)
