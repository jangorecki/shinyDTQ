shinyServer(function(input, output, session){
  
  output$tree <- renderTree({
    print.list.dtq(DTQ)
  })
  output$tree_str <- renderPrint({
   str(input$tree)
  })
  
  dtq_call <- reactive({
    tree <- input$tree
    isolate({
      if(is.null(tree)) return(invisible())
      select <- get_selected(tree, format = "names")
      if(!length(select)) return(invisible())
      if(length(attr(select[[1L]],"ancestry",TRUE))) sel <- attr(select[[1L]],"ancestry",TRUE)
      else sel <- select[[1L]]
      sel <- strsplit(sel,' ')[[1L]][[1L]]
      call_i <- length.dtq(DTQ) - as.integer(substr(sel,2,nchar(sel)))
      if(call_i > 0) recall.dtq(DTQ[[rep("x",call_i)]]) else recall.dtq(DTQ)
    })
  })
  output$dtq_call <- renderPrint({
    cat(paste(deparse(dtq_call(),width.cutoff=500L),collapse="\n"))
  })
  
  dtq_eval <- reactive({
    eval(dtq_call())
  })
  
  output$dt <- renderDataTable(dtq_eval(), options = list(pageLength = 10, lengthMenu = c(10,25,50,100)))
  
})