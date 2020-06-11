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
    
    # filter on studies
    res = mutate(studies, 
                 sample_size_type = ifelse(sample_size_type=='Anticipated', 'Target', sample_size_type)) %>% # rename
      filter(study_type %in% input$stype, 
             assignment == input$assignment,  # 
             purpose %in% input$purpose, 
             allocation %in% input$alloc)
    if(any(input$stype == 'Interventional')){ # filter on phase only for interventional
      res = filter(res, 
                   phase == input$phase)
    }
    stats = group_by(res, sample_size_type) %>%
      summarise(n = n(), 
                med = round(median(sample_size)),
                lower = round(quantile(sample_size, probs=plow)),
                upper = round(quantile(sample_size, probs=phigh))) %>%
      ungroup() %>%
      mutate(x = as.numeric(sample_size_type=='Target'), # for x-axis of plot
             x = factor(x, levels=0:1, labels=c('Actual', 'Target')))
    to_return = list()
    to_return$stats = stats
    to_return$studies = res
    return(to_return)
  })
  
  output$target_sample_size <- renderText({
    print(results()$stats)
    to_show = filter(results()$stats, sample_size_type == 'Target')
    paste('The median sample size is ', to_show$med, '.\n',
          'The lower interval is ', to_show$lower, 
          ' and the upper interval is ', format(to_show$upper, big.mark = ','), '.\n',
          'These statistics are from ', format(to_show$n, big.mark=','), ' studies.\n', sep='')
  })
  
  output$actual_sample_size <- renderText({
    to_show = filter(results()$stats, sample_size_type == 'Actual')
    paste('The median sample size is ', to_show$med, '.\n',
          'The lower interval is ', to_show$lower, 
          ' and the upper interval is ', format(to_show$upper, big.mark=','), '.\n',
          'These statistics are from ', format(to_show$n, big.mark=','), ' studies.\n', sep='')
  })
  
  output$sample_size_stats <- renderPlot({
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
    if(input$log_scale==TRUE){ # replace scale with log scale
      p = p + scale_y_log10()+
        ylab('Sample size (log scale)')
    }
    print(p)
  }, height=340, width=480)
  
  
  output$sample_size_distribution <- renderPlot({
    p = ggplot(data=results()$studies, aes(x=sample_size, fill=sample_size_type))+
      geom_histogram(size=5)+
      scale_fill_manual(NULL, values=c('indianred1','blue'))+
      ylab('Count')+
      xlab('Sample size')+
      theme_bw()+
      facet_wrap(~sample_size_type)+
      theme(text=element_text(size=18), 
            panel.grid.minor = element_blank(),
            legend.position = 'none')
    if(input$log_scale==TRUE){ # replace scale with log scale
      p = p + scale_x_log10(breaks=c(1,10,100,1000,10000,100000,1000000))+
       xlab('Sample size (log scale)')
    }
    print(p)
  }, height=340, width=480)
  
})
