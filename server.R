server = function(input, output){
  observe({if (input$quit == 1)
    stopApp()})
  
  output$manySliders1 <- renderUI({
    slide.bars1 <- list()
    for (j in 1:6){
      if (preds[[j]]$dataClass == "factor"){
        slide.bars1[[j]] <- list(selectInput(names(preds)[j], names(preds)[j], 
                                             preds[[j]]$v.levels, 
                                             selected = preds[[j]]$v.levels[1],
                                             multiple = FALSE))
      }
      if (preds[[j]]$dataClass == "numeric"){
        slide.bars1[[j]] <- list(sliderInput(names(preds)[j], 
                                             names(preds)[j],
                                             min = preds[[j]]$v.min,
                                             max = preds[[j]]$v.max, 
                                             value = preds[[j]]$v.mean))
      }
    }
    do.call(tagList, slide.bars1)
  })
  
  output$manySliders2 <- renderUI({
    slide.bars2 <- list()
    for (j in 7:length(preds)){
      if (preds[[j]]$dataClass == "factor"){
        slide.bars2[[j]] <- list(selectInput(names(preds)[j], names(preds)[j], 
                                             preds[[j]]$v.levels, 
                                             selected = preds[[j]]$v.levels[1],
                                             multiple = FALSE))
      }
      if (preds[[j]]$dataClass == "numeric"){
        slide.bars2[[j]] <- list(sliderInput(names(preds)[j], 
                                             names(preds)[j],
                                             min = preds[[j]]$v.min,
                                             max = preds[[j]]$v.max, 
                                             value = preds[[j]]$v.mean))
      }
    }
    do.call(tagList, slide.bars2)
  })
  
  input.d <- reactive({
    input$add
    input.v <- vector("list", length(preds))
    names(input.v) <- names(preds)
    for (i in 1:length(preds)) {
      input.v[[i]] <- isolate({
        input[[names(preds)[i]]]
      })
      # names(input.v)[i] <- names(preds)[i]
    }
    out <- data.frame(lapply(input.v, cbind))
    # if (a == 0) {
    #   wher <- match(names(out), names(input.data))
    #   out <- out[wher]
    #   input.data <<- rbind(input.data, out)
    # }
    # if (a > 0) {
    #   wher <- match(names(out), names(input.data))
    #   out <- out[wher]
    #   if (!isTRUE(compare(old.d, out))) {
    #     input.data <<- rbind(input.data, out)
    #   }}
    # a <<- a + 1
    out
    
  })
  
  
  # output$inputDataTable <- renderTable({
  #   input.d()
  # })
  
  # output$inputDataTable2 <- renderTable({
  #   surv_d(input.d())
  # })
  
  
  output$plot <- renderPlot({
    surv_plot(input.d())
  })
  
  output$plot2 <- renderPlot({
    exp_plot(input.d())
  })
  
}