

source("startup.R")


navbarPage( "Explore simulation results", id = "navbar",
            
            
            theme = shinytheme("flatly"),
            
            tabPanel( "Instructions",
                      
                      
                      mainPanel(
                        
                        wellPanel(  
    
                          HTML(paste(
                                               
                                               "This website allows you to explore simulation results regarding the performance of 
                                               <a href='https://osf.io/w89s5/'>Mathur & VanderWeele's (2019)</a>'s statistical 
                                               methods for multisite replication projects. The first tab shows Type I error and power for
                                               the consistency metric,
                                               P<sub>orig</sub>. The second tab shows bias, root mean square error, and confidence interval coverage
                                               for the proportion of true effects stronger than a threshold, P<sub>>q</sub>.",
                                               
                                               sep="<br/><br/>"))
                                    
                                    
                        ),
                        
                        width=12
                        
                        
                      )
                      
                      # sidebarPanel(
                      #   HTML(paste("<b>Please use the following citations:</b>",
                      # 
                      #              "<a href='https://journals.lww.com/epidem/Citation/publishahead/Website_and_R_Package_for_Computing_E_Values.98679.aspx'>(1) Mathur MB, Ding P, Riddell CA, VanderWeele TJ. (2018). Website and R package
                      #                          for computing E-values. <i>Epidemiology</i>, 29(5), e45-e47.",
                      # 
                      #              "<a href='http://annals.org/aim/article-abstract/2643434/sensitivity-analysis-observational-research-introducing-e-value?doi=10.7326%2fM16-2607'>(2) VanderWeele TJ,
                      #                          & Ding P. (2017). Sensitivity analysis in observational research: introducing the
                      #                          E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.</a>",
                      # 
                      # 
                      #              "<b>Bug reports</b>",
                      # 
                      #              "Submit any bug reports to: <i>mmathur [AT] stanford [DOT] edu</i> or open
                      #                               an issue on <a href='https://github.com/mayamathur/evalue/issues'>Github</a>.",
                      # 
                      #              sep="<br/><br/>") )
                      # 
                      # )
            ),
            
            tabPanel( HTML("P<sub>orig</sub>"),
                      
                      wellPanel( h3("Choose simulation scenarios"),
                                 "To explore Type I error, set delta to 0 (i.e., the original study is exactly consistent with the replications).
                                 To explore power, set delta to a value greater than 0 (i.e., the original study is inconsistent with the replications).",
                                 headerPanel(""),
                                 # checkboxGroupInput( "k",
                                 #                     label = "Number of replications" ),
                                 
                                 fluidRow(
                                   column(4,
                                          checkboxGroupInput( "k", label = "Number of replications",
                                                              choices = unique(as.character(agg1$k) ),
                                                              selected = c("10", "15", "20", "25") ),
                                          
                                          checkboxGroupInput( "minN", label = "Sample size per replication",
                                                              choices = unique(as.character(agg1$minN) ),
                                                              selected = unique(as.character(agg1$minN) ) ),
                                          
                                          checkboxGroupInput( "Norig", label = "Sample size in original",
                                                              choices = unique(as.character(agg1$N.orig) ),
                                                              selected = unique(as.character(agg1$N.orig) ) )
                                          
                                   ),
                                   
                                   column(4, 
                                          
                                          checkboxGroupInput( "V", label = "Variance of true effects",
                                                              choices = unique(as.character(agg1$V) ),
                                                              selected = "0.01" ),
                                          
                                          checkboxGroupInput( "dist", label = "True effect distribution",
                                                              choices = unique(as.character(agg1$dist.pretty) ),
                                                              selected = "Normal" ),
                                          
                                          checkboxGroupInput( "delta", label = "Delta (standardized mean difference between replication true mean and original true effect)",
                                                              choices = unique(as.character(agg1$delta) ),
                                                              selected = unique(as.character(agg1$delta) ) )
                                          
                                          
                                          
                                          
                                   ),
                                   
                                   column(4, 
                                          
                                          
                                          
                                          
                                          checkboxGroupInput( "POrigMethod", label = "Heterogeneity estimation method ",
                                                              choiceValues = unique(as.character(agg1$POrig.Method) ),
                                                              choiceNames = c("Paule & Mandel", "REML"),
                                                              selected = "reml" ) )
                                   
                                   
                                 ),
                                 
                                 width = 12
                                 
                      ),  # ends mainPanel
                      
                      wellPanel(
                        h3("Summary across your chosen scenarios"),
                        tableOutput("table2") ),
                      
                      wellPanel(
                        h3("Full results from your chosen scenarios"),
                        "Each row represents results for one simulation scenario. You can sort on a particular variable using the arrows by the variable's name.",
                        headerPanel(""),  # just for whitespace
                        DT::dataTableOutput("table")
                      )
                      
            ) # end tab panel
            
)










