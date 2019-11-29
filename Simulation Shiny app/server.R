source("startup.R")

function(input, output, session) {
  
  ##### Table 1: Dataset for Tab 1 #####
  # Filter data based on selections
  output$table1 <- DT::renderDataTable(DT::datatable({
    data = subset_agg1(input)
    
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
  
  ##### Table 2: Summary for Tab 1 #####
  output$table2 <- renderTable({

    data = subset_agg1(input)

    t2 = data %>% summarise( `Number of scenarios` = n(),
                             `Rejection rate` = round( mean(Reject.mn), 3 ),
                             `Max rejection rate` = round( max(Reject.mn), 3 )
    )
    t2
  })
  
  ##### Table 3: Dataset for Tab 2 #####
  # Filter data based on selections
  output$table3 <- DT::renderDataTable(DT::datatable({
    data = subset_agg2(input)

    # tidy up
    data = data %>% select( k,
                            minN,
                            V,
                            dist.pretty,
                            TheoryP,
                            Phat.mn,
                            Phat.bias.mn,
                            Cover.mn,
                            EmpVar,
                            RMSE ) %>%
      mutate( Cover.mn = round( Cover.mn, 3),
              Phat.mn = round( Phat.mn, 3),
              Phat.bias.mn = round( Phat.mn, 3),
              EmpVar = round( Phat.mn, 3),
              RMSE = round( Phat.mn, 3) )

    names(data) = c("Number replications",
                    "N per replication",
                    "Variance of true effects",
                    "Distribution",
                    "True proportion",
                    "Mean Phat",
                    "Phat bias",
                    "Phat coverage",
                    "Phat empirical var",
                    "Phat RMSE")

    data
  }))

  ##### Table 4: Summary for Tab 2 #####
  output$table4 <- renderTable({

    data = subset_agg2(input)

    t2 = data %>% summarise( `Number of scenarios` = n(),
                             `Phat bias` = round( mean(Phat.bias.mn), 3 ),
                             `Max Phat bias` = round( max(Phat.bias.mn), 3 ),
                             `Phat RMSE` = round( mean(Phat.bias.mn), 3 ),
                             `Max Phat RMSE` = round( max(Phat.bias.mn), 3 ),
                             `Coverage` = round( mean(Cover.mn), 3 ),
                             `Min coverage` = round( min(Cover.mn), 3 )
    )
    t2
  })
  
}   # end entire script





