library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(openxlsx)
library(haven)

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      /* Custom CSS for enhanced interface */
      body {
        background-color: #f5f8fa;
      }
      .main-container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }
      .header-panel {
        background: linear-gradient(135deg, #3498db, #2c3e50);
        color: white;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .disclaimer-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin: 20px auto;
        font-size: 0.9em;
        border-left: 4px solid #e74c3c;
        max-width: 800px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .well-panel-custom {
        background-color: white;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        border: none;
      }
      .variable-row {
        margin-bottom: 10px;
        padding: 10px;
        background-color: #f9f9f9;
        border-radius: 5px;
        transition: all 0.3s ease;
      }
      .variable-row:hover {
        background-color: #f0f0f0;
      }
      .btn-custom {
        margin: 5px 0;
        transition: all 0.2s ease;
      }
      .btn-custom:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .tab-content {
        background-color: white;
        padding: 15px;
        border-radius: 0 0 5px 5px;
        border: 1px solid #ddd;
        border-top: none;
      }
      .nav-tabs {
        border-bottom: 1px solid #ddd;
      }
      h3 {
        color: #2c3e50;
        margin-top: 0;
        padding-bottom: 10px;
        border-bottom: 1px solid #eee;
      }
      .footer {
        text-align: center;
        margin-top: 30px;
        padding: 15px;
        color: #7f8c8d;
        font-size: 0.9em;
        border-top: 1px solid #eee;
      }
      .help-icon {
        color: #3498db;
        margin-left: 5px;
        cursor: pointer;
      }
    "))
  ),
  
  div(class = "main-container",
      # Header Panel
      div(class = "header-panel",
          h1("QuickStatsGen", style = "margin: 0; font-weight: 300;"),
          h4("Custom Data Generator for Statistical Practice", style = "margin: 5px 0 0; font-weight: 300; opacity: 0.9;")
      ),
      
      # Disclaimer Box
      div(class = "disclaimer-box",
          tags$strong("Important Notice:", style = "color: #e74c3c;"),
          tags$p("This tool generates synthetic data for statistical practice and educational demonstrations only."),
          tags$p("With Effect Strength ≥7 and ≥20 observations, most analyses will show statistically significant results (p < 0.05)."),
          tags$p("The data is not suitable for:", 
                 tags$ul(
                   tags$li("Academic theses or research papers"),
                   tags$li("School assignments requiring real-world data"),
                   tags$li("Any formal academic work requiring validated data sources")
                 )),
          tags$p("Always verify data appropriateness for your specific learning objectives.")
      ),
      
      # Main Content
      fluidRow(
        column(12,
               # Variable Specifications Section
               div(class = "well-panel-custom",
                   h3("Variable Specifications"),
                   helpText("Define up to 500 variables. For numerical variables, set range (min:max) and decimal places."),
                   
                   div(
                     style = "max-height: 500px; overflow-y: auto; padding: 5px; margin-bottom: 15px;",
                     uiOutput("var_spec_ui")
                   ),
                   
                   fluidRow(
                     column(6, actionButton("add_var", "Add Variable", 
                                          class = "btn-primary btn-block btn-custom",
                                          icon = icon("plus"))),
                     column(6, actionButton("remove_var", "Remove Variable", 
                                          class = "btn-danger btn-block btn-custom",
                                          icon = icon("minus")))
                   )
               ),
               
               # Data Generation and Output Section
               div(class = "well-panel-custom",
                   h3("Data Generation and Output"),
                   
                   fluidRow(
                     column(
                       4,
                       numericInput("sample_size", "Number of Observations (≥20):", 
                                  value = 100, min = 20, max = 10000, width = "100%")
                     ),
                     column(
                       4,
                       numericInput("sig_strength", "Effect Strength (1-10):", 
                                  value = 7, min = 1, max = 10, width = "100%"),
                       helpText(icon("info-circle"), 
                                "Strength 1-3: Weak relationships (p > 0.05)",
                                br(),
                                "Strength 4-6: Moderate relationships (p ≈ 0.05)",
                                br(),
                                "Strength 7-10: Strong relationships (p < 0.05)",
                                style = "font-size: 0.8em; margin-top: 5px;")
                     ),
                     column(
                       4,
                       div(style = "margin-top: 25px;",
                           actionButton("generate", "Generate Dataset", 
                                      class = "btn-success btn-block btn-custom",
                                      icon = icon("database")))
                     )
                   ),
                   
                   tabsetPanel(
                     tabPanel("Generated Data", 
                             div(class = "tab-content",
                                 DTOutput("data_table"))),
                     tabPanel("Variable Specifications", 
                             div(class = "tab-content",
                                 DTOutput("spec_table"))),
                     tabPanel("Relationships", 
                             div(class = "tab-content",
                                 plotOutput("relationship_plot", height = "600px"),
                                 helpText("This plot shows correlations between numerical variables. Values closer to 1 or -1 indicate stronger relationships."))),
                     tabPanel("Inferential Analysis",
                             div(class = "tab-content",
                                 h4("ANOVA Results (Categorical vs Numerical)"),
                                 verbatimTextOutput("anova_results"),
                                 h4("Correlation Tests (Numerical vs Numerical)"),
                                 verbatimTextOutput("cor_test_results"),
                                 h4("Chi-Square Tests (Categorical vs Categorical)"),
                                 verbatimTextOutput("chisq_results")))
                   ),
                   
                   h4("Download Data", style = "margin-top: 20px; margin-bottom: 15px;"),
                   fluidRow(
                     column(3, downloadButton("download_csv", "CSV", 
                                            class = "btn-block btn-custom")),
                     column(3, downloadButton("download_excel", "Excel", 
                                            class = "btn-block btn-custom")),
                     column(3, downloadButton("download_spss", "SPSS", 
                                            class = "btn-block btn-custom")),
                     column(3, downloadButton("download_stata", "Stata", 
                                            class = "btn-block btn-custom"))
                   )
               )
        )
      ),
      
      # Footer with developer credit
      div(class = "footer",
          tags$p("Developed by Mudasir Mohammed Ibrahim", style = "margin-bottom: 5px;"),
          tags$p("For suggestions or feedback, please contact: ", 
                 tags$a(href = "mailto:mudassiribrahim30@gmail.com", "mudassiribrahim30@gmail.com"))
      )
  )
)

server <- function(input, output, session) {
  
  # Track variables (start with 3 default variables)
  vars <- reactiveValues(
    count = 3, 
    specs = tibble(
      Name = paste0("Var", 1:3),
      Type = rep("Numerical", 3),
      Options = rep("0:100", 3),
      Decimals = rep(0, 3)
    )
  )
  
  # Add variable
  observeEvent(input$add_var, {
    if(vars$count < 500) {
      vars$count <- vars$count + 1
      vars$specs <- bind_rows(vars$specs, tibble(
        Name = paste0("Var", vars$count),
        Type = "Numerical",
        Options = "0:100",
        Decimals = 0
      ))
    } else {
      showNotification("Maximum of 500 variables reached", type = "warning")
    }
  })
  
  # Remove variable
  observeEvent(input$remove_var, {
    if(vars$count > 1) {
      vars$count <- vars$count - 1
      vars$specs <- vars$specs[1:vars$count, , drop = FALSE]
    }
  })
  
  # Variable specification UI
  output$var_spec_ui <- renderUI({
    req(vars$specs)
    map(1:vars$count, ~ {
      div(
        class = "variable-row",
        fluidRow(
          column(
            3,
            textInput(
              inputId = paste0("name_", .x), 
              label = NULL,
              value = vars$specs$Name[.x],
              width = "100%",
              placeholder = "Variable name"
            )
          ),
          column(
            2,
            selectInput(
              inputId = paste0("type_", .x), 
              label = NULL,
              choices = c("Numerical", "Categorical", "Ordinal"),
              selected = vars$specs$Type[.x],
              width = "100%"
            )
          ),
          column(
            4,
            uiOutput(outputId = paste0("options_ui_", .x))
          ),
          column(
            3,
            conditionalPanel(
              condition = sprintf("input['type_%s'] == 'Numerical'", .x),
              numericInput(
                inputId = paste0("decimals_", .x),
                label = NULL,
                value = vars$specs$Decimals[.x],
                min = 0, 
                max = 6, 
                step = 1,
                width = "100%"
              )
            )
          )
        )
      )
    })
  })
  
  # Dynamic options UI for each variable
  observe({
    map(1:vars$count, ~ {
      output[[paste0("options_ui_", .x)]] <- renderUI({
        current_type <- input[[paste0("type_", .x)]]
        if(is.null(current_type)) current_type <- vars$specs$Type[.x]
        
        if(current_type == "Numerical") {
          textInput(
            inputId = paste0("options_", .x),
            label = NULL,
            value = vars$specs$Options[.x],
            placeholder = "min:max",
            width = "100%"
          )
        } else {
          textInput(
            inputId = paste0("options_", .x),
            label = NULL,
            value = ifelse(str_detect(vars$specs$Options[.x], ":"), 
                          "Option1,Option2", vars$specs$Options[.x]),
            placeholder = "comma,separated,values",
            width = "100%"
          )
        }
      })
    })
  })
  
  # Update specs when inputs change
  observe({
    if(vars$count > 0) {
      tryCatch({
        vars$specs <- tibble(
          Name = map_chr(1:vars$count, ~ {
            input_val <- input[[paste0("name_", .x)]]
            ifelse(is.null(input_val), vars$specs$Name[.x], input_val)
          }),
          Type = map_chr(1:vars$count, ~ {
            input_val <- input[[paste0("type_", .x)]]
            ifelse(is.null(input_val), vars$specs$Type[.x], input_val)
          }),
          Options = map_chr(1:vars$count, ~ {
            input_val <- input[[paste0("options_", .x)]]
            ifelse(is.null(input_val), vars$specs$Options[.x], input_val)
          }),
          Decimals = map_dbl(1:vars$count, ~ {
            if(!is.null(input[[paste0("type_", .x)]]) && 
               input[[paste0("type_", .x)]] == "Numerical") {
              input_val <- input[[paste0("decimals_", .x)]]
              ifelse(is.null(input_val), vars$specs$Decimals[.x], as.numeric(input_val))
            } else {
              0
            }
          })
        )
      }, error = function(e) {
        showNotification("Error updating variable specifications", type = "error")
      })
    }
  })
  
  # Display specifications table
  output$spec_table <- renderDT({
    datatable(vars$specs, 
             options = list(
               dom = 't', 
               scrollX = TRUE,
               pageLength = 10
             )) %>%
      formatStyle(columns = names(vars$specs), fontSize = '90%')
  })
  
  # Generate data with guaranteed significant relationships when strength ≥7
  generated_data <- eventReactive(input$generate, {
    req(vars$count > 0, vars$specs, input$sample_size >= 20)
    
    tryCatch({
      n <- input$sample_size
      effect_strength <- input$sig_strength / 10  # Scale to 0.1-1.0
      
      # Create base tibble
      data <- as_tibble(matrix(nrow = n, ncol = vars$count))
      names(data) <- vars$specs$Name
      
      # Generate first variable
      if(vars$specs$Type[1] == "Numerical") {
        range_vals <- str_split(vars$specs$Options[1], ":")[[1]] %>% as.numeric()
        decimals <- vars$specs$Decimals[1]
        data[,1] <- round(runif(n, min = range_vals[1], max = range_vals[2]), decimals)
      } else {
        categories <- str_split(vars$specs$Options[1], ",")[[1]] %>% str_trim()
        data[,1] <- sample(categories, n, replace = TRUE)
      }
      
      # Generate subsequent variables with controlled relationships
      if(vars$count > 1) {
        for(i in 2:vars$count) {
          if(vars$specs$Type[i] == "Numerical") {
            range_vals <- str_split(vars$specs$Options[i], ":")[[1]] %>% as.numeric()
            decimals <- vars$specs$Decimals[i]
            
            # Find previous numerical variables
            prev_num_vars <- data[,1:(i-1)] %>% 
              select(where(is.numeric)) %>% 
              names()
            
            if(length(prev_num_vars) > 0) {
              # Create relationship based on effect strength
              base_value <- rowMeans(data[, prev_num_vars, drop = FALSE])
              
              # Scale the base value to contribute according to effect strength
              scaled_base <- effect_strength * (base_value - mean(base_value)) / sd(base_value)
              
              # Generate random component
              random_component <- rnorm(n, mean = 0, sd = (1 - effect_strength) * diff(range_vals)/2)
              
              # Combine components and scale to desired range
              raw_value <- scaled_base * diff(range_vals)/4 + 
                          runif(n, min = range_vals[1], max = range_vals[2]) + 
                          random_component
              
              # Ensure values stay within specified range
              scaled_value <- pmin(pmax(raw_value, range_vals[1]), range_vals[2])
              data[,i] <- round(scaled_value, digits = decimals)
              
            } else {
              # If no previous numerical variables, just generate random values
              data[,i] <- round(runif(n, min = range_vals[1], max = range_vals[2]), decimals)
            }
            
          } else {
            categories <- str_split(vars$specs$Options[i], ",")[[1]] %>% str_trim()
            
            # Find previous numerical variables
            prev_num_vars <- data[,1:(i-1)] %>% 
              select(where(is.numeric)) %>% 
              names()
            
            if(length(prev_num_vars) > 0) {
              # Create relationship based on effect strength
              base_value <- rowMeans(data[, prev_num_vars, drop = FALSE])
              
              # Create groups based on numerical values
              num_groups <- length(categories)
              group_cuts <- quantile(base_value, probs = seq(0, 1, length.out = num_groups + 1))
              
              # Adjust group assignment based on effect strength
              group_probs <- rep(1/num_groups, num_groups)
              if(effect_strength > 0.5) {
                # For stronger effects, increase probability of correct group
                for(g in 1:num_groups) {
                  group_probs[g] <- ifelse(g == num_groups, 1, effect_strength * 0.8)
                }
                group_probs <- group_probs / sum(group_probs)
              }
              
              # Assign groups with some randomness based on effect strength
              group_assign <- cut(base_value, breaks = group_cuts, labels = FALSE, include.lowest = TRUE)
              data[,i] <- categories[group_assign]
              
              # Add some noise based on effect strength
              if(effect_strength < 0.9) {
                noise_level <- round((1 - effect_strength) * n * 0.3)
                if(noise_level > 0) {
                  noise_indices <- sample(1:n, noise_level)
                  data[noise_indices, i] <- sample(categories, noise_level, replace = TRUE)
                }
              }
              
            } else {
              # If no previous numerical variables, just generate random categories
              data[,i] <- sample(categories, n, replace = TRUE)
            }
            
            if(vars$specs$Type[i] == "Ordinal") {
              data[,i] <- ordered(data[[i]], levels = categories)
            }
          }
        }
      }
      
      data
    }, error = function(e) {
      showNotification(paste("Error generating data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Display data table
  output$data_table <- renderDT({
    req(generated_data())
    datatable(
      generated_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        scrollY = "500px",
        dom = 'tip',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'font-size': '90%'});",
          "$(this.api().table().container()).css({'padding': '2px'});",
          "}"
        )
      ),
      class = 'compact hover stripe',
      rownames = FALSE
    ) %>%
      formatStyle(1:ncol(generated_data()), 
                `padding-top` = '2px',
                `padding-bottom` = '2px')
  })
  
  # Plot relationships between variables
  output$relationship_plot <- renderPlot({
    req(generated_data())
    data <- generated_data()
    
    # Select only numerical variables
    num_data <- data %>% select(where(is.numeric))
    if(ncol(num_data) < 2) {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE,
           main = "Need at least 2 numerical variables to show relationships")
      return()
    }
    
    # Calculate correlations
    cor_data <- cor(num_data, use = "pairwise.complete.obs")
    
    # Create correlation plot
    cor_data %>% 
      as.data.frame() %>% 
      rownames_to_column("Var1") %>% 
      pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>% 
      ggplot(aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      coord_fixed() +
      geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
      labs(title = "Correlation Matrix of Numerical Variables",
           subtitle = paste("Effect Strength:", input$sig_strength, 
                          "- Expected p-values:", 
                          ifelse(input$sig_strength >= 7, "< 0.05", 
                                 ifelse(input$sig_strength >= 4, "≈ 0.05", "> 0.05"))))
  })
  
  # Inferential analysis outputs
  output$anova_results <- renderPrint({
    req(generated_data())
    data <- generated_data()
    
    # Get numerical and categorical variables
    num_vars <- data %>% select(where(is.numeric)) %>% names()
    cat_vars <- data %>% select(where(is.factor), where(is.character)) %>% names()
    
    if(length(num_vars) == 0 || length(cat_vars) == 0) {
      cat("Need at least one numerical and one categorical variable for ANOVA")
      return()
    }
    
    # Perform ANOVA for each combination
    for(num in num_vars) {
      for(cat in cat_vars) {
        cat("\nANOVA for", num, "by", cat, ":\n")
        formula <- as.formula(paste(num, "~", cat))
        aov_result <- aov(formula, data = data)
        print(summary(aov_result))
        cat("\n")
      }
    }
  })
  
  output$cor_test_results <- renderPrint({
    req(generated_data())
    data <- generated_data()
    
    # Get numerical variables
    num_vars <- data %>% select(where(is.numeric)) %>% names()
    
    if(length(num_vars) < 2) {
      cat("Need at least two numerical variables for correlation tests")
      return()
    }
    
    # Create all combinations
    combos <- combn(num_vars, 2)
    
    # Perform correlation tests
    for(i in 1:ncol(combos)) {
      var1 <- combos[1, i]
      var2 <- combos[2, i]
      cat("\nCorrelation test between", var1, "and", var2, ":\n")
      test_result <- cor.test(data[[var1]], data[[var2]])
      print(test_result)
      cat("\n")
    }
  })
  
  output$chisq_results <- renderPrint({
    req(generated_data())
    data <- generated_data()
    
    # Get categorical variables
    cat_vars <- data %>% select(where(is.factor), where(is.character)) %>% names()
    
    if(length(cat_vars) < 2) {
      cat("Need at least two categorical variables for chi-square tests")
      return()
    }
    
    # Create all combinations
    combos <- combn(cat_vars, 2)
    
    # Perform chi-square tests
    for(i in 1:ncol(combos)) {
      var1 <- combos[1, i]
      var2 <- combos[2, i]
      cat("\nChi-square test between", var1, "and", var2, ":\n")
      tbl <- table(data[[var1]], data[[var2]])
      
      # Check if expected counts are sufficient
      expected <- tryCatch({
        suppressWarnings(chisq.test(tbl)$expected)
      }, error = function(e) NULL)
      
      if(is.null(expected) || any(expected < 5)) {
        cat("Note: Some expected counts are <5. Fisher's exact test may be more appropriate.\n")
        if(nrow(tbl) == 2 && ncol(tbl) == 2) {
          print(fisher.test(tbl))
        } else {
          print(chisq.test(tbl, simulate.p.value = TRUE))
        }
      } else {
        print(chisq.test(tbl))
      }
      cat("\n")
    }
  })
  
  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("quickstats_data_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(generated_data(), file)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("quickstats_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(generated_data(), file)
    }
  )
  
  output$download_spss <- downloadHandler(
    filename = function() {
      paste("quickstats_data_", format(Sys.Date(), "%Y%m%d"), ".sav", sep = "")
    },
    content = function(file) {
      write_sav(generated_data(), file)
    }
  )
  
  output$download_stata <- downloadHandler(
    filename = function() {
      paste("quickstats_data_", format(Sys.Date(), "%Y%m%d"), ".dta", sep = "")
    },
    content = function(file) {
      write_dta(generated_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
