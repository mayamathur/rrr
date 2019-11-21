

source("startup.R")


navbarPage( "Explore simulation results", id = "navbar",
            
            
            theme = shinytheme("flatly"),
            
            tabPanel( "Type I error and power",
                      
                      wellPanel( h3("Choose simulation scenarios"),
                                 # checkboxGroupInput( "k",
                                 #                     label = "Number of replications" ),
                                 
                                 fluidRow(
                                   column(4,
                                          checkboxGroupInput( "k", label = "Number of replications",
                                                              choices = unique(as.character(agg$k) ),
                                                              selected = c("10", "15", "20", "25") ),
                                          
                                          checkboxGroupInput( "minN", label = "Sample size per replication",
                                                              choices = unique(as.character(agg$minN) ),
                                                              selected = unique(as.character(agg$minN) ) ),
                                          
                                          checkboxGroupInput( "Norig", label = "Sample size in original",
                                                              choices = unique(as.character(agg$N.orig) ),
                                                              selected = unique(as.character(agg$N.orig) ) )
                                          
                                   ),
                                   
                                   column(4, 
                                          
                                          checkboxGroupInput( "V", label = "Variance of true effects",
                                                              choices = unique(as.character(agg$V) ),
                                                              selected = "0.01" ),
                                          
                                          checkboxGroupInput( "dist", label = "True effect distribution",
                                                              choices = unique(as.character(agg$dist.pretty) ),
                                                              selected = "Normal" ),
                                          
                                          checkboxGroupInput( "delta", label = "Delta (standardized mean difference between replication true mean and original true effect)",
                                                              choices = unique(as.character(agg$delta) ),
                                                              selected = unique(as.character(agg$delta) ) )
                                          
                                          
                                          
                                          
                                   ),
                                   
                                   column(4, 
                                          
                                          
                                          
                                          
                                          checkboxGroupInput( "POrigMethod", label = "Heterogeneity estimation method ",
                                                              choiceValues = unique(as.character(agg$POrig.Method) ),
                                                              choiceNames = c("Paule & Mandel", "REML"),
                                                              selected = "reml" ) )
                                   
                                   
                                 ),
                                 
                                 width = 12
                                 
                      )  # ends mainPanel
                      
                      # # panel for contour plot
                      # sidebarPanel(width = 6),
                      
            ), # end tab panel
            
            wellPanel(
              h3("Summary across your chosen scenarios"),
              tableOutput("table2") ),
            
            wellPanel(
              h3("Full results from your chosen scenarios"),
              "Each row represents results for one simulation scenario. You can sort on a particular variable using the arrows by the variable's name.",
              headerPanel(""),  # just for whitespace
              DT::dataTableOutput("table")
            )
            
            
            # tabPanel( "Instructions",
            #           
            #           
            #           mainPanel(
            #             
            #             
            #             # Google Analytics
            #             # this JS is from the "tracking code" available on the Google Analytics website
            #             tags$head( HTML( '<script async src="https://www.googletagmanager.com/gtag/js?id=UA-125815848-1"></script>
            #             <script>
            #                                            window.dataLayer = window.dataLayer || [];
            #                                            function gtag(){dataLayer.push(arguments);}
            #                                            gtag("js", new Date());
            # 
            #                                            gtag("config", "UA-125815848-1");
            #                                            </script>' ) ),
            #             # END Google Analytics
            #             
            #             wellPanel(  HTML(paste("<b>Computing an E-value</b>",
            #                                    
            #                                    'The tab "Compute an E-value" computes the E-value, defined as the minimum strength of association
            #                           on the risk ratio scale that an unmeasured confounder would need to have with both the exposure
            #                           and the outcome, conditional on the measured covariates, to fully explain away a specific
            #                           exposure-outcome association. Note that for outcome types other than relative risks, assumptions
            #                           are involved with the approximate conversions used. See citation (2) for details.',
            #                                    
            #                                    'Alternatively, you can consider the confounding strength capable of moving the observed
            #                           association to any other value (e.g. attenuating the observed association to a true causal
            #                           effect that is no longer scientifically important, or alternatively increasing a near-null
            #                           observed association to a value that is of scientific importance). For this purpose, simply
            #                           type a non-null effect size into the box "True causal effect to which to shift estimate"
            #                           when computing the E-value.',
            #                                    
            #                                    "<b>Computing a bias factor</b>",                     
            #                                    
            #                                    "Additionally, if you have substantive knowledge on the strength of the relationships
            #                           between the unmeasured confounder(s) and the exposure and outcome, you can use these
            #                           numbers to <a href='https://bias-factor.hmdc.harvard.edu'>calculate the bias factor</a>.",
            #                                    sep="<br/><br/>"))
            #                         
            #                         
            #             ),
            #             
            #             width=6
            #             
            #             
            #           ),
            #           
            #           sidebarPanel(
            #             HTML(paste("<b>Please use the following citations:</b>",
            #                        
            #                        "<a href='https://journals.lww.com/epidem/Citation/publishahead/Website_and_R_Package_for_Computing_E_Values.98679.aspx'>(1) Mathur MB, Ding P, Riddell CA, VanderWeele TJ. (2018). Website and R package
            #                                    for computing E-values. <i>Epidemiology</i>, 29(5), e45-e47.",
            #                        
            #                        "<a href='http://annals.org/aim/article-abstract/2643434/sensitivity-analysis-observational-research-introducing-e-value?doi=10.7326%2fM16-2607'>(2) VanderWeele TJ,
            #                                    & Ding P. (2017). Sensitivity analysis in observational research: introducing the
            #                                    E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.</a>",
            #                        
            #                        
            #                        "<b>Bug reports</b>",
            #                        
            #                        "Submit any bug reports to: <i>mmathur [AT] stanford [DOT] edu</i> or open
            #                                         an issue on <a href='https://github.com/mayamathur/evalue/issues'>Github</a>.",
            #                        
            #                        sep="<br/><br/>") )
            #             
            #           )
            # ),
            
)










