

# Latent Class Shiny

## Dina Arch



# Load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(MplusAutomation)
library(glue)
library(bslib)

# Define UI ---------------------------------------------------------------

ui <- navbarPage(theme = bs_theme(bootswatch = "flatly"),
  id = 'main',
  title = "Mixture Model Visualizations",
  #set theme here
  
  
  # *- Tab 1: How-To -------------------------------------------
  
  tabPanel(
    'How-To',
    h4('Welcome to the Mixture Model Visuals Shiny App', align = "center", style = "display: block; margin-left: auto; margin-right: auto; padding: 50px"),
      tags$body(
        tags$br(),
        div(img(src='lca.png'), style = "text-align: center;"),
        tags$br(),
        
        
        tags$p(
  
          style = "text-align: left;",
          "This app was created to assist researchers using ",
          tags$em("Mplus"), "for their Latent Class Analyses. This app creates data visualizations for mixture models starting from the 
          enumeration step to adding auxiliary variables. Researchers can directly upload their output received from Mplus to the Shiny application. 
          After uploading the Mplus output, you are provided with graphs and plots that visualize results in APA format, along with R code that 
          directly corresponds with the graphics. Specifcally, this app is designed to visualize models using the", tags$em("Three-Step/BCH"), "models.",
          tags$br(),
          tags$br(),
          "Each tab provides a walkthough on how to generate the plots. Below is a summary of what each tab is for:",
          tags$br(),
          tags$br(),
          tags$b("Enumeration:"),
          tags$br(),
          "This tab generates", tags$em("Item Probability Plots"), "from the enumeration step of the analyses. Class enumeration is the first step to finding the number of classes based on a set of items. After using fit indices to decide on the best fitting model, a visual of the item probabilites for the chosen latent class model can be used.",
          tags$br(),
          tags$br(),
          tags$b("Binary Covariate Plots:"),
          tags$br(),
          "This tab generates", tags$em("Binary Covariate Plots"), "from the last step in the", tags$em("Three-Step/BCH")," (Asparouhov & Muthen, 2014) step of the analyses. 
          The first step, as mentioned, is to estimate a latent class model. The second step, is to create most likely class variable using the latent class posterior distrbituion obtained 
          in the enuemeration step. And finally, the last step is to enter the auxiliary varaibles, or covariates and distal outcomes, into the model with the latent class variable. 
          This tab assumes completion of the third step. Additionally, this tab is for visualizations of binary covariates.",
          tags$br(),
          tags$br(),
          tags$br(),
          tags$b("COMING SOON:"),
          tags$br(),
          "Continuous simple slope graphs and plots.",
          tags$br(),
          tags$br(),
          tags$br(),
          tags$b("References"),
          tags$br(),
          tags$a(
            href = "https://www.statmodel.com/download/asparouhov_muthen_2014.pdf",
            "Asparouhov, T., & Muthén, B. (2014)."),"Auxiliary Variables in Mixture Modeling: 
          Using the BCH Method in Mplus to Estimate a Distal Outcome Model and an Arbitrary Secondary Model.", 
          tags$i("Structural Equation Modeling: A Multidisciplinary Journal."),"21:3, 329-341.",
          tags$br(),
          tags$a(
            href = "http://www.statmodel.com/download/webnotes/webnote15.pdf",
            "Asparouhov, T., & Muthén, B. (2014)."),"Auxiliary Variables in Mixture Modeling: 3-Step Approaches Using Mplus.", 
          tags$i("Mplus Web Notes"),": No. 15. August 5, 2014. Revised December 23, 2020.",
          tags$br(),
          tags$a(
            href = "http://www.statmodel.com/download/usersguide/MplusUserGuideVer_8.pdf",
            "Muthén, L. K., & Muthén, B. O. (1998-2020)."),
          tags$i("Mplus User's Guide"),". Sixth Edition. Los Angeles, CA: Muthén & Muthén.",
          tags$br(),
          tags$a(
            href = "https://www.statmodel.com/download/LCA_tech11_nylund_v83.pdf",
            "Nylund, K. L., & Asparouhov, T., & Muthén, B. (2007)."),
          tags$i("Structural Equation Modeling"),". 14, 535-569. DOI: 10.1080/10705510701575396",
          
          
        )
      )
      
    
  ),
  
  
  
  # *- Tab 2: Item Probability Plot  ---------------------------------------------------
  
  tabPanel(
    'Enumeration',
    h4('Enumeration', align = "center", 
       style = "display: block; margin-left: auto; margin-right: auto;"),
      align = "left",
      tags$body(
        "This tab generates Item Probability Plots from the enumeration step of the analyses. 
        Class enumeration is the first step to finding the number of classes based on a set of items. 
        After using fit indices to decide on the best fitting model, 
        a visual of the item probabilites for the chosen latent class model can be used. See", tags$a(
          href = "https://www.statmodel.com/download/LCA_tech11_nylund_v83.pdf","(Nylund, Asparouhov, & Muthén, 2007)"), " for an in-depth review of latent class enumeration.",
        tags$br(),
        tags$br(),
        "Please upload your Mplus output file (.out) from your chosen latent class model. There should be no covariates entered in the model yet. The syntax should look something like this:",
        tags$br(),
        tags$br(),
        div(img(src='enumeration.png'), style = "text-align: left;"),
        tags$br(),
        "It is important to note that the variable names should be written out like example above (i.e., not 'var1-var6;'). 
        Finally, this Shiny is for binary indicators. It is assumed that 'Category 1' of the indiactor is for non endorsement 
        and 'Category 2' is for endorsement of the item. For example:",
        tags$br(),
        tags$br(),
        div(img(src='cat.png'), style = "text-align: left;"),
        tags$br(),
        "Here, individuals in Class 1 have an 86.2% chance of endorsing the first category. This shiny assumes the endorsement category is 'Category 2.'",
        tags$br(),
        tags$br(),
        
      
    ),
    
    
    
    # *--- Step 1: Enumeration Upload -------------------------------------------------------------
    
    
    tabsetPanel(
      tabPanel(
        "Upload Enumeration File",
        value = "tab1",
        sidebarLayout(
          position = "left",
          sidebarPanel(
            "",
            width = 4,
            
            #Upload .out Enumeration File
            fileInput(
              inputId = "mplus_file",
              label = strong("Upload .out file here:"),
              multiple = FALSE
            )
          ),
          # Data Summary Table
          mainPanel(
            width = 8,
            fluidRow(style = "padding-right: 10px;",
                     splitLayout(
                       tableOutput(outputId = "mplus_df")
                     ))
          )
        )
      ),
      # *--- Step 2: View Item Probability Plot -------------------------------------------------------------
      
      tabPanel(
        "Item Probability Plot",
        value = "tab2",
        
        sidebarLayout(
          position = "left",
          sidebarPanel(width = 4,
            strong("Aesthetics"),
            tags$br(),
            tags$br(),
            # Checkbox for error bars
            checkboxInput(
              inputId = "error_bars",
              label = "Show show error bars",
              value = TRUE
            ),
            tags$br(),
            sliderInput('ipp_size', 
                        label='Text Size', 
                        min = 10,
                        max = 30, 
                        value = 18, 
                        step = 5),
            tags$br(),
            strong("R - Code"),
            tags$br(),
            tags$br(),
            # Checkbox for ggplot code
            checkboxInput(
              inputId = "ipp_text",
              label = "Show `ggplot` R code",
              value = FALSE
            ),
            
            # Download ggplot
            downloadButton(outputId = "download_plot",
                           label = strong("Download Figure")),
            
          ),
          mainPanel(
            "",
            width = 8,
            fluidRow(
              style = "padding-right: 10px;",
              splitLayout(plotOutput(outputId = "ip_plot",
                                     width = "900px",
                                     height = "600px")),
              
            ),

            conditionalPanel(
              condition = "input.ipp_text == true",
              fluidRow(style = "padding-right: 20px;",
                       textOutput("ipp_text", container = pre))
            )
          )
        )
      ),
      # *--- Step 3: View Data Table for Plot -------------------------------------------------------------
      
      tabPanel(
        "Data Table for Plot",
        value = "tab3",
        
        sidebarLayout(
          position = "left",
          sidebarPanel(
            "Use this dataframe and the accompanying R-code to recreate the item probability plot in the previous tab.",
            tags$br(),
            width = 4,
            tags$br(),
            # Download .csv
            downloadButton(outputId = "download_csv",
                           label = strong("Download .csv")),
            
          ),
          mainPanel(
            "",
            width = 8,
            
            # Plot Data
            fluidRow(style = "padding-right: 10px;",
                      tableOutput("ipp_table")),
            
          )
        )
      )
      
    )
  ),

  # *- Tab 3: Binary Covariate Table  ---------------------------------------------------
  
  tabPanel(
    'Binary Covariate Plots',
    h4('Binary Covariate Plots', align = "center", 
       style = "display: block; margin-left: auto; margin-right: auto;"),
    align = "left",
    tags$body(
      "This tab generates", tags$em("Binary Covariate Plots"), "from the last step in the", tags$em("Three-Step/BCH")," (Asparouhov & Muthen, 2014) step of the analyses. 
          The first step, as mentioned, is to estimate a latent class model. The second step, is to create most likely class variable using the latent class posterior distrbituion obtained 
          in the enuemeration step. And finally, the last step is to enter the auxiliary varaibles, or covariates and distal outcomes, into the model with the latent class variable. 
          This tab assumes completion of the third step. Additionally, this tab is for visualizations of binary covariates.",
      tags$br(),
      tags$br()
      )
    ,  
    
    # *--- Step 1: Three-Step Upload -------------------------------------------------------------
    
    
    tabsetPanel(
      tabPanel(
        "Upload .out from 3-Step",
        value = "tab4",
        sidebarLayout(
          position = "left",
          sidebarPanel(
            "",
            width = 4,
            
            #Upload .out Enumeration File
            fileInput(
              inputId = "threestep_binary_mplus_file",
              label = strong("Upload .out file here:"),
              multiple = FALSE
            )
          ),
          # Data Summary Table
          mainPanel(
            strong("Data Summary of Covariate Estimates",
                   style = "padding-right: 30px"),
            width = 8,
            tags$br(),
            fluidRow(style = "padding-right: 50px;",
                     splitLayout(
                       tableOutput(outputId = "threestep_binary_mplus_df")
                     ))
          )
        )
      ),
      # *--- Step 2: Binary Covariate Plot -------------------------------------------------------------
      
      tabPanel(
        "Binary Covariate Plot",
        value = "tab5",
        
        sidebarLayout(
          position = "left",
          sidebarPanel(
            strong("View up to two covariate plots"),
            tags$br(),
            width = 4,

            # Checkbox for Covariate 1 code
            checkboxInput(
              inputId = "covariate1_plot",
              label = "Show `Covariate 1` plot",
              value = TRUE
            ),
            
            # Checkbox for Covariate 2 code
            checkboxInput(
              inputId = "covariate2_plot",
              label = "Show `Covariate 2` plot",
              value = TRUE
            ),
            tags$br(),
            strong("R - Code"),
            # Checkbox for ggplot code
            checkboxInput(
              inputId = "threestep_text",
              label = "Show `ggplot` code",
              value = FALSE
            ),
          
            
            # Download ggplot
            #downloadButton(outputId = "binary_download_plot",
            #               label = strong("Download Figure")),
            
          ),
          mainPanel(
            "",
            width = 8,
            # Covariate 1 table
            conditionalPanel(
              condition = "input.covariate1_plot == true",
              fluidRow(style = "padding-right: 20px;",
                       plotOutput("covariate1_plot"))
            ),
            conditionalPanel(
              condition = "input.covariate2_plot == true",
              fluidRow(style = "padding-right: 20px;",
                       plotOutput("covariate2_plot"))
            ),
            conditionalPanel(
              condition = "input.threestep_text == true",
              fluidRow(style = "padding-right: 20px;",
                       textOutput("threestep_text", container = pre))
            ),
          )
        )
      ),
      # *--- Step 3: View Data Table for Plot -------------------------------------------------------------
      
      tabPanel(
        "Data Table for Plot",
        value = "tab6",
        
        sidebarLayout(
          position = "left",
          sidebarPanel(
            "",
            width = 4,
            
            # Checkbox for Covariate 1 code
            checkboxInput(
              inputId = "cov1_table",
              label = "Show `Covariate 1` Table",
              value = TRUE
            ),
            
            # Checkbox for Covariate 2 code
            checkboxInput(
              inputId = "cov2_table",
              label = "Show `Covariate 2` Table",
              value = TRUE
            ),
            
            tags$br(),
            tags$br(),
            
            
            # Download .csv
            downloadButton(outputId = "download_cov1_csv",
                           label = strong("Download `Covariate `1 .csv")),
            tags$br(),
            tags$br(),
            # Download .csv
            downloadButton(outputId = "download_cov2_csv",
                           label = strong("Download `Covariate 2` .csv")),
            
          ),
          mainPanel(
            "",
            width = 8,
            
            #Plot data
            
            conditionalPanel(
              condition = "input.cov1_table == true",
              fluidRow(style = "padding-right: 20px;",
                       tableOutput("cov1_table"))
            ),
            conditionalPanel(
              condition = "input.cov2_table == true",
              fluidRow(style = "padding-right: 20px;",
                       tableOutput("cov2_table"))
            )

          )
        )
      )
    )
  
  )
)  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
# *- Tab 2: Item Probability Plot  ---------------------------------------------------  
  
  # *--- Step 1: Enumeration Upload -------------------------------------------------------------
  
        # Read in data
  
  df <- reactive({
    inFile <- input$mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("parameters", "class_counts"))
    
    # Find number of classes
    class_no <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
      nrow()
    
    # Create class size in percentages (%)
    c_size <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["proportion"]])
    colnames(c_size) <- paste0("cs")
    c_size <- c_size %>%
      mutate(cs = round(cs * 100, 2))
    
    ## Extract Item Probabilities
    pp <-
      as.data.frame(data[["parameters"]][["probability.scale"]]) %>%
      filter(category == "2") %>%
      select(est, LatentClass, param) %>%
      pivot_wider(names_from = LatentClass, values_from = est)
    
    # Item Probabilities
    pp_noparam <- pp %>%
      select(-param)
    colnames(pp_noparam) <-
      paste0("Class ", 1:class_no, glue(" ({c_size[1:class_no,]}%)"))
    #
    # Extract Variable Names
    var <- pp %>%
      select(param) %>%
      mutate(param = as_factor(param)) %>%
      rename(Variable = param)
    
    
    final_df <- cbind(var, pp_noparam)
    
    return(final_df)
    
  })
  
  
  output$mplus_df <- renderTable({
    df()
    
  })

  # *--- Step 2:  Item Probability Plot  -------------------------------------------------------------
  
  
  # Item probability plot
  
  ipp_df <- reactive({
    inFile <- input$mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("parameters", "class_counts"))
    
    # Find number of classes
    class_no <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
      nrow()
    
    # Call table from first tab
    final_df <- df()
    
    #Add se for error bars
    se <- as.data.frame(data[["parameters"]][["probability.scale"]]) %>% 
      filter(category == "2") %>% 
      select(se)
    
    # Change dateframe from wide to long format
   final_df %>% 
      pivot_longer(cols = 2:all_of(class_no + 1), names_to = "class") %>% 
      add_column(se)

  })
  
  # Data Wrangling
  
  plot_data <- reactive({
    inFile <- input$mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("parameters", "class_counts"))
    
    # Find number of classes
    class_no <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
      nrow()
    
    #Rename first reactive df
    
    final_df <- df()
    
    # Find number of indicators
    ind_no <- final_df %>%
      nrow()
    
    p <- ipp_df() %>%
      ggplot(aes(
        x = as.integer(Variable),
        y =  value,
        shape = class,
        colour = class,
        lty = class
      )) +
      geom_point(size = 4) +
      geom_line() +
      scale_x_continuous("Indicators",
                         breaks = 1:ind_no,
                         labels = final_df$Variable) +
      scale_colour_grey(start = 0, end = 0.5) +
      theme_classic() +
      theme(
        legend.title = element_blank(),
        legend.position = "top",
        text = element_text(size = input$ipp_size)
      ) +
      labs(title = "Item Probability Plot", y = "Probabilities")
   
   errorbars <- geom_errorbar(aes(ymin=value-se, ymax=value+se),
                             size=.3,    # Thinner lines
                             width=.2)
    if(input$error_bars) p = p + errorbars
   
   p
   
  })
  
  # Plot
  
  output$ip_plot <- renderPlot({
    plot_data()
    
  })
  
  
  # Download Plot
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      ggsave(file,
             plot = plot_data(),
             width = 12,
             height = 7)
      
    }
    
  )
  

      # Text
  
  output$ipp_text <- renderText({
    final_df <- df()
    
    # Find number of indicators
    ind_no <- final_df %>%
      nrow()
    
    errorbars <- paste('
       geom_errorbar(aes(ymin=value-se, ymax=value+se),
                               size=.3,
                               width=.2) +')
    
    p <- paste(
      'ggplot(data, aes(x = as.integer(Variable), y = value,
      shape = class, colour = class, lty = class)) +
       geom_point(size = 4) +
       geom_line() +
       scale_x_continuous("Indicators", breaks = 1:',ind_no,', labels = data$Variable) +
       scale_colour_grey(start = 0, end = 0.5) + ',
      if (input$error_bars) {
        errorbars
      },
      '
       theme_classic() +
       theme(legend.title = element_blank(),
              legend.position = "top",
              text = element_text(size =',input$ipp_size,') +
       labs(title = "Item Probability Plot", y = "Probabilities")',
      sep = ''
    )
    
    
  })
 
  # *--- Step 3: Plot Data View -------------------------------------------------------------
  
     # Table
  
  output$ipp_table <- renderTable({
    ipp_df <- ipp_df()
    
    ipp_df
    
  })
  
      # Download .csv for Plot
  
  output$download_csv <- downloadHandler (
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ipp_df(), file)
    }
  )
  
# *- Tab 3: Binary Covariate Plot  ---------------------------------------------------  
  
  # *--- Step 1: 3-Step Upload -------------------------------------------------------------
  
  # Read in data
  
  df_binary <- reactive({
    inFile <- input$threestep_binary_mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("sampstat","parameters", "class_counts"))
    
    # Extract logits
    logits <-
      as.data.frame(data[["parameters"]][["unstandardized"]]) %>%
      filter(paramHeader != "Means") %>%
      filter(paramHeader != "New.Additional.Parameters") %>%
      select(paramHeader, param, est)
    
    return(logits)
    
  })
  
  
  output$threestep_binary_mplus_df <- renderTable({
    
    df_binary()
    
  })
  
  # *--- Step 2:  Binary Covariate Plot  -------------------------------------------------------------
  
  
  # Binary Covariate plot
  
  cov1_df <- reactive({
    inFile <- input$threestep_binary_mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("sampstat","parameters", "class_counts"))
    
    # Extract Means
    means <- as.data.frame(data[["sampstat"]][["means"]])
    
    # Extract dummy codes
    cov_codes <-
      as.data.frame(data[["sampstat"]][["univariate.sample.statistics"]]) %>%
      select(Minimum, Maximum) %>% 
      t() %>% 
      as.data.frame()
    
    
    # Extract names of Covariates
    cov_names <- names(means)
    
    # Relabel
    first_cov <- cov_names[1]
    non_first_cov <- paste("non", cov_names[1])
    sec_cov <- cov_names[2]
    non_sec_cov <- paste("non", cov_names[2])
    
    # Extract logits
    logits <-
      as.data.frame(data[["parameters"]][["unstandardized"]]) %>%
      filter(paramHeader != "Means") %>%
      filter(paramHeader != "New.Additional.Parameters") %>%
      select(paramHeader, param, est)
    
    # Extract intercept
    intercept <- logits %>%
      filter(paramHeader == "Intercepts")
    
    # Extract logits
    beta <- logits %>%
      filter(paramHeader != "Intercepts")
    
    
    # Create class size in percentages (%)
    c_size <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["proportion"]])
    colnames(c_size) <- paste0("cs")
    c_size <- c_size %>%
      mutate(cs = round(cs * 100, 2))
    
    
    ### Convert logits to probabilities
    
    # First, how many covariates do they have?
    cov_no <- cov_names %>%
      length()
    
    # Second, how many classes do they have?
    class_no <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
      nrow()
    
    # Prep data for analysis
    # COV#1: FEMALE
    beta_2cov_1 <- logits %>%
      filter(param == cov_names[1]) %>%
      select(est) %>%
      rename(beta1 = est)
    
    
    # COV#2: URM
    beta_2cov_2 <- logits %>%
      filter(param == cov_names[2]) %>%
      select(est) %>%
      rename(beta2 = est)
    
    #Intercept
    int <- intercept %>%
      select(est) %>%
      rename(int = est)
    
    
    #Mean 1: URM
    #Mean 2: FEMALE
    rename_means <- means %>%
      rename(mean1 = cov_names[1],
             mean2 = cov_names[2])
    
    df <-
      cbind(int, beta_2cov_1, beta_2cov_2, rename_means) %>%
      as.data.frame()
    
    # Extract names of Covariates
    cov_names <- names(means)
    
    # Relabel
    first_cov <- cov_names[1]
    non_first_cov <- paste("non", cov_names[1])
    sec_cov <- cov_names[2]
    non_sec_cov <- paste("non", cov_names[2])
    
    
    # Reference Class
    ref_class <- class_no - 1
    
    # Create function for two covariates
    cov <- function(int, beta1, mean1, beta2, mean2) {
      exp(int + (beta1) * (mean1) + (beta2) * (mean2))
    }
    
    ## COVARIATE 1
    
    #Covariate 1, 0
    cov1_0 <- NULL
    for (i in 1:ref_class) {
      x <- cov(df[i, 1], df[i, 2], cov_codes[1,1], df[i, 3], df$mean2)
      cov1_0 <- rbind(cov1_0, x)
      
    }
    
    cov1_0 <- as.data.frame(cov1_0) %>%
      select(1) %>%
      remove_rownames() %>%
      rename(COV1_0 = V1)
    cov1_0 <- cov1_0  %>%
      add_row(COV1_0 = 1)
    
    
    # Convert to probabilities
    
    
    prob0 <- cov1_0 %>%
      mutate(prob0 = COV1_0 / sum(COV1_0)) %>% 
      select(prob0)
    
    colnames(prob0) <- glue("{non_first_cov}")
    
    
    #Covariate 1, 1
    cov1_1 <- NULL
    for (i in 1:ref_class) {
      x <- cov(df[i, 1], df[i, 2], cov_codes[2,1], df[i, 3], df$mean2)
      cov1_1 <- rbind(cov1_1, x)
      
    }
    
    cov1_1 <- as.data.frame(cov1_1) %>%
      select(1) %>%
      remove_rownames() %>%
      rename(COV1_1 = V1)
    cov1_1 <- cov1_1  %>%
      add_row(COV1_1 = 1)
    
    
    # Convert to probabilities
    prob1 <- cov1_1 %>%
      mutate(prob1 = COV1_1 / sum(COV1_1)) %>% 
      select(prob1)
    colnames(prob1) <- glue("{first_cov}")
    
    # Add class labels
    cov1_prob <- cbind(prob0, prob1) %>%
      add_column(class = paste0("Class ", 1:class_no, glue(" ({c_size[1:class_no,]}%)")))
    
    #Plot dataframe
    
    plot_df_cov1 <- cov1_prob %>% 
      pivot_longer(!class, names_to = "cov", values_to = "prob")
    
    return(plot_df_cov1)
    
   
  })
  
  
  
  # Covariate 2 Plot Data 
  
  cov2_df <- reactive({
    
    inFile <- input$threestep_binary_mplus_file
    if (is.null(inFile))
      return(NULL)
    data <-
      MplusAutomation::readModels(inFile$datapath, what = c("sampstat","parameters", "class_counts"))
    
    # Extract Means
    means <- as.data.frame(data[["sampstat"]][["means"]])
    
    # Extract dummy codes
    cov_codes <-
      as.data.frame(data[["sampstat"]][["univariate.sample.statistics"]]) %>%
      select(Minimum, Maximum) %>% 
      t() %>% 
      as.data.frame()
    
    
    # Extract names of Covariates
    cov_names <- names(means)
    
    # Relabel
    first_cov <- cov_names[1]
    non_first_cov <- paste("non", cov_names[1])
    sec_cov <- cov_names[2]
    non_sec_cov <- paste("non", cov_names[2])
    
    # Extract logits
    logits <-
      as.data.frame(data[["parameters"]][["unstandardized"]]) %>%
      filter(paramHeader != "Means") %>%
      filter(paramHeader != "New.Additional.Parameters") %>%
      select(paramHeader, param, est)
    
    # Extract intercept
    intercept <- logits %>%
      filter(paramHeader == "Intercepts")
    
    # Extract logits
    beta <- logits %>%
      filter(paramHeader != "Intercepts")
    
    
    # Create class size in percentages (%)
    c_size <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["proportion"]])
    colnames(c_size) <- paste0("cs")
    c_size <- c_size %>%
      mutate(cs = round(cs * 100, 2))
    
    
    ### Convert logits to probabilities
    
    # First, how many covariates do they have?
    cov_no <- cov_names %>%
      length()
    
    # Second, how many classes do they have?
    class_no <-
      as.data.frame(data[["class_counts"]][["modelEstimated"]][["class"]]) %>%
      nrow()
    
    # Prep data for analysis
    # COV#1: FEMALE
    beta_2cov_1 <- logits %>%
      filter(param == cov_names[1]) %>%
      select(est) %>%
      rename(beta1 = est)
    
    
    # COV#2: URM
    beta_2cov_2 <- logits %>%
      filter(param == cov_names[2]) %>%
      select(est) %>%
      rename(beta2 = est)
    
    #Intercept
    int <- intercept %>%
      select(est) %>%
      rename(int = est)
    
    
    #Mean 1: URM
    #Mean 2: FEMALE
    rename_means <- means %>%
      rename(mean1 = cov_names[1],
             mean2 = cov_names[2])
    
    df <-
      cbind(int, beta_2cov_1, beta_2cov_2, rename_means) %>%
      as.data.frame()
    
    # Extract names of Covariates
    cov_names <- names(means)
    
    # Relabel
    first_cov <- cov_names[1]
    non_first_cov <- paste("non", cov_names[1])
    sec_cov <- cov_names[2]
    non_sec_cov <- paste("non", cov_names[2])
    
    
    # Reference Class
    ref_class <- class_no - 1
    
    # Create function for two covariates
    cov <- function(int, beta1, mean1, beta2, mean2) {
      exp(int + (beta1) * (mean1) + (beta2) * (mean2))
    }
    
    
    ## COVARIATE 2
    
    #Covariate 2, 0
    cov2_0 <- NULL
    for (i in 1:ref_class) {
      x <- cov(df[i, 1], df[i, 2], df$mean1, df[i, 3], cov_codes[1,1])
      cov2_0 <- rbind(cov2_0, x)
      
    }
    
    cov2_0 <- as.data.frame(cov2_0) %>%
      select(1) %>%
      remove_rownames() %>%
      rename(COV2_0 = V1)
    cov2_0 <- cov2_0  %>%
      add_row(COV2_0 = 1)
    
    
    # Convert to probabilities
    
    
    prob0 <- cov2_0 %>%
      mutate(prob0 = COV2_0 / sum(COV2_0)) %>% 
      select(prob0)
    
    colnames(prob0) <- glue("{non_sec_cov}")
    
    
    #Covariate 2, 1
    cov2_1 <- NULL
    for (i in 1:ref_class) {
      x <- cov(df[i, 1], df[i, 2], df$mean1, df[i, 3], cov_codes[2,1])
      cov2_1 <- rbind(cov2_1, x)
      
    }
    
    cov2_1 <- as.data.frame(cov2_1) %>%
      select(1) %>%
      remove_rownames() %>%
      rename(COV2_1 = V1)
    cov2_1 <- cov2_1  %>%
      add_row(COV2_1 = 1)
    
    
    # Convert to probabilities
    prob1 <- cov2_1 %>%
      mutate(prob1 = COV2_1 / sum(COV2_1)) %>% 
      select(prob1)
    colnames(prob1) <- glue("{sec_cov}")
    
    # Add class labels
    cov2_prob <- cbind(prob0, prob1) %>%
      add_column(class = paste0("Class ", 1:class_no, glue(" ({c_size[1:class_no,]}%)")))
    
    #Plot dataframe
    
    plot_df_cov2 <- cov2_prob %>% 
      pivot_longer(!class, names_to = "cov", values_to = "prob")
    
    plot_df_cov2 %>% 
      as.data.frame()
    
  })

  # Covariate 1 Plot
  
  plot_cov1<- reactive({

    
    plot_df <- cov1_df()
    
    p <-  plot_df %>%
      ggplot(aes(x= as.factor(class), y = as.numeric(prob), fill = cov)) +
      geom_bar(stat = "identity", position=position_dodge(), colour="black") +
      geom_text(aes(label = round(prob,2)), vjust=-1, size=3.5,
                position = position_dodge(0.9))+
      scale_fill_grey(start = .4, end = .8) +
      theme_classic() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 12)
      ) +
      labs(title = "Binary Covariate Plot", x = "", y = "Probabilities") 
    p
    
  })
  
  # Plot
  
  output$covariate1_plot <- renderPlot({
    plot_cov1()
    
  })
  
  # Covariate 2 Plot
  
  plot_cov2<- reactive({
    
    
    plot_df <- cov2_df()
    
    p <-  plot_df %>%
      ggplot(aes(x= as.factor(class), y = as.numeric(prob), fill = cov)) +
      geom_bar(stat = "identity", position=position_dodge(), colour="black") +
      geom_text(aes(label = round(prob,2)), vjust=-1, size=3.5,
                position = position_dodge(0.9))+
      scale_fill_grey(start = .4, end = .8) +
      theme_classic() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 12)
      ) +
      labs(title = "Binary Covariate Plot", x = "", y = "Probabilities") 
    p
    
  })
  
  # Plot
  
  output$covariate2_plot <- renderPlot({
    plot_cov2()
    
  })
  
  # Download Plot
  
  #output$download_cov_plot <- downloadHandler(
  #  filename = function() {
  #    "plot.png"
  #  },
  #  content = function(file) {
  #    ggsave(file,
  #           plot = plot_cov_data(),
  #           width = 7,
  #           height = 7)
  #    
  #  }
    
  #)
  
  # Text
  
  output$threestep_text <- renderText({
    
    p <- paste(
      'ggplot(binary_df, aes(x= as.factor(class), y = as.numeric(prob), fill = cov)) + 
      geom_bar(stat = "identity", position=position_dodge(), colour="black") +
      geom_text(aes(label = round(prob,2)), vjust=-1, size=3.5,
                position = position_dodge(0.9))+
      scale_fill_grey(start = .4, end = .8) +
      theme_classic() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 12)
      ) +
      labs(title = "Binary Covariate Plot", x = "", y = "Probabilities") ',
      sep = ''
    )
    
  })

  # *--- Step 3: Plot Data View -------------------------------------------------------------
  
   
  # Table - Covariate 1
  
  output$cov1_table <- renderTable({
    
     cov1_df()
    

    
  })
  
  # Table - Covariate 1
  
  output$cov2_table <- renderTable({
    
    cov2_df()

    
  })
  
  # Download .csv for Plot
  
  output$download_cov1_csv <- downloadHandler(
    filename = function() {
      paste("binary_df_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cov1_df(), file)
    }
  )
  
  # Download .csv for Plot
  
  output$download_cov2_csv <- downloadHandler(
    filename = function() {
      paste("binary_df_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cov2_df(), file)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
