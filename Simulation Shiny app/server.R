source("startup.R")

function(input, output, session) {
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data = subset_data(input)
    
    # tidy up
    data = data %>% select( k, 
                            minN, 
                            V,
                            dist.pretty,
                            delta, 
                            N.orig,
                            POrig.Method,
                            Reject.mn ) %>%
      mutate( Reject.mn = round( Reject.mn, 3 ) )
    
    names(data) = c("Number replications",
                    "N per replication",
                    "Variance of true effects",
                    "Distribution",
                    "Delta",
                    "N original",
                    "Heterogeneity method",
                    "Mean rejection rate")
    
    data
  }))
  
  
  # output$table2 <- DT::renderDataTable(DT::datatable({
  #   
  #   data = subset_data(input)
  #   
  #   t2 = data %>% summarise( `Number of scenarios` = n(),
  #                            `Mean rejection rate across scenarios` = round( mean(Reject.mn), 3 ),
  #                            `Max rejection rate across scenarios` = round( max(Reject.mn), 3 )
  #                            )
  #   t2
  #   
  #   
  # }))
  
  output$table2 <- renderTable({
    
    data = subset_data(input)
    
    t2 = data %>% summarise( `Number of scenarios` = n(),
                             `Mean rejection rate across scenarios` = round( mean(Reject.mn), 3 ),
                             `Max rejection rate across scenarios` = round( max(Reject.mn), 3 )
    )
    t2
    
    
  })
  
}





