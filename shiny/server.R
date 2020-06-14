# for sample size from clinicaltrials.gov
# Jun 2020

shinyServer(function(input, output) {
  
  # reactive function depending on user inputs
  results = reactive({
    
    # different interval widths
    if(input$interval_width=="45%"){plow=0.05; phigh=0.95}
    if(input$interval_width=="40%"){plow=0.1; phigh=0.9}
    if(input$interval_width=="30%"){plow=0.2; phigh=0.8}
    if(input$interval_width=="25% (inter-quartile range)"){plow=0.25; phigh=0.75}
    if(input$interval_width=="20%"){plow=0.3; phigh=0.7}
    if(input$interval_width=="10%"){plow=0.4; phigh=0.6}
    
    ## filter on studies
    # a) clinicaltrials.gov
    if(input$database=="clinicaltrials.gov"){
    res = clintrials
    # do not filter if all of the above selected
    if(input$purpose.c != 'all'){
      res = filter(res, 
                   purpose == input$purpose.c)
    }
    if(input$phase.c != 'all'){
      res = filter(res, 
                   phase == input$phase.c)
    }
    if(input$assignment.c != 'all'){
      res = filter(res, 
                   assignment == input$assignment.c)
    }
    if(input$alloc != 'all'){
      res = filter(res, 
                   allocation == input$alloc)
    }
    # stats
    stats = group_by(res, sample_size_type) %>%
      summarise(n = n(), 
                med = round(median(sample_size)),
                lower = round(quantile(sample_size, probs=plow)),
                upper = round(quantile(sample_size, probs=phigh))) %>%
      ungroup() %>%
      mutate(x = as.numeric(sample_size_type=='Target'), # for x-axis of plot
             x = factor(x, levels=0:1, labels=c('Actual', 'Target')))
    } # end of clinicaltrials.gov if
    
    # b) ANZCTR
    if(input$database=="ANZCTR"){
      res = studies
      if(input$assignment.a != 'all'){
        res = filter(res, 
                     assignment == input$assignment.a)
      }
      if(input$phase.a != 'all'){
        res = filter(res, 
                     phase == input$phase.a)
      }
      if(input$endpoint != 'all'){
        res = filter(res, 
                     endpoint == input$endpoint)
      }      
      if(input$control != 'all'){
        res = filter(res, 
                     control == input$control)
      }
      if(input$condition != 'all'){
        res = filter(res, 
                     ccode1 == input$condition)
      }
      stats = group_by(res, sample_size_type) %>%
        summarise(n = n(), 
                  med = round(median(sample_size)),
                  lower = round(quantile(sample_size, probs=plow)),
                  upper = round(quantile(sample_size, probs=phigh))) %>%
        ungroup() %>%
        mutate(x = as.numeric(sample_size_type=='Target'), # for x-axis of plot
               x = factor(x, levels=0:1, labels=c('Actual', 'Target')))
    } # end of ANZCTR if
    
    # if fewer than 10 for all results
    no_results = FALSE
    if(all(stats$n < 10)){no_results = TRUE}

    # return
    to_return = list()
    to_return$no_results = no_results
    to_return$stats = stats
    to_return$clintrials = res
    return(to_return)
  })
  
  output$target_sample_size <- renderText({
    #print(results()$stats) # temporary code check
    if(results()$no_results==TRUE){
      out = 'There are fewer than 10 trials for this combination.'
    }
    if(results()$no_results==FALSE){
      to_show = filter(results()$stats, sample_size_type == 'Target')
      out = paste('The median sample size is ', to_show$med, '.\n',
          'The lower interval is ', to_show$lower, 
          ' and the upper interval is ', format(to_show$upper, big.mark = ','), '.\n',
          'These statistics are from ', format(to_show$n, big.mark=','), ' trials.\n', sep='')
    }
    print(out)
  })
  
  output$actual_sample_size <- renderText({
    if(results()$no_results==TRUE){
      out = 'There are fewer than 10 trials for this combination.'
    }
    if(results()$no_results==FALSE){
      to_show = filter(results()$stats, sample_size_type == 'Actual')
      out = paste('The median sample size is ', to_show$med, '.\n',
          'The lower interval is ', to_show$lower, 
          ' and the upper interval is ', format(to_show$upper, big.mark=','), '.\n',
          'These statistics are from ', format(to_show$n, big.mark=','), ' trials.\n', sep='')
    }
    print(out)
  })
  
  # summary stats plot
  output$sample_size_stats <- renderPlot({
    if(results()$no_results==TRUE){
      p = NULL
    }
    if(results()$no_results==FALSE){
      p = ggplot(data=results()$stats, aes(x=x, y=med, ymin=lower, ymax=upper, col=x))+
        geom_point(size=5)+
        scale_color_manual(NULL, values=c('indianred1','blue'))+
        geom_errorbar(width=0, size=1.05)+
        coord_flip()+
        scale_y_continuous(limits=c(0,NA))+ # always start at zero
        xlab('')+
        ylab('Sample size')+
        theme_bw()+
        theme(text=element_text(size=18), legend.position = 'none')
      if(input$log_scale_stats=='Log'){ # replace scale with log scale
        p = p + scale_y_log10()+
          ylab('Sample size (log scale)')
      }
    }
    print(p)
  }, height=340, width=480)
  
  # distribution with percentiles
  output$sample_size_distribution <- renderPlot({
    if(results()$no_results==TRUE){
      p = NULL
    }
    if(results()$no_results==FALSE){
      p = ggplot(data=results()$clintrials, aes(x=sample_size, fill=sample_size_type))+
        geom_histogram(size=5)+
        scale_fill_manual(NULL, values=c('indianred1','blue'))+
        ylab('Count')+
        xlab('Sample size')+
        scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
        geom_vline(data=results()$stats, aes(xintercept=lower), lty=2, col='grey')+ # lines for intervals that respect facet
        geom_vline(data=results()$stats, aes(xintercept=upper), lty=2, col='grey')+ # lines for intervals that respect facet
        facet_wrap(~sample_size_type)+
        theme_bw()+
        theme(text = element_text(size=18),
              axis.text.x = element_text(angle=45, hjust=1),
              panel.grid.minor = element_blank(),
              legend.position = 'none')
      if(input$log_scale_dist=='Log'){ # replace scale with log scale
        p = p + scale_x_log10(breaks=c(1,10,100,1000,10000,100000,1000000,10000000), 
                              labels = scales::number_format(accuracy = 1, big.mark = ","))+
          xlab('Sample size (log scale)')
      }
    }
    print(p)
  }, height=340, width=480)
  
})
