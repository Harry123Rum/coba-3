# SIVANA - Social Index Vulnerability Analysis Navigator Application
# Minimal Version - Works with basic R installation
# Created for Social Vulnerability Index Analysis

# Check and install essential packages
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, repos = "https://cran.rstudio.com/", dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Essential packages only
essential_packages <- c("shiny", "DT")
check_and_install(essential_packages)

# Load data function
load_sovi_data <- function() {
  if (file.exists("sovi_data.csv")) {
    data <- read.csv("sovi_data.csv", stringsAsFactors = FALSE)
    cat("Data loaded:", nrow(data), "rows,", ncol(data), "columns\n")
    return(data)
  } else {
    cat("Warning: sovi_data.csv not found\n")
    return(NULL)
  }
}

# Load the data
sovi_data <- load_sovi_data()

# Define UI
ui <- fluidPage(
  titlePanel("SIVANA - Social Index Vulnerability Analysis Navigator"),
  
  navbarPage(
    "SIVANA Dashboard",
    
    # Home Tab
    tabPanel("🏠 Beranda",
      fluidRow(
        column(12,
          h2("Selamat Datang di SIVANA Dashboard"),
          br(),
          div(class = "well",
            h4("Tentang Dashboard"),
            p("SIVANA (Social Index Vulnerability Analysis Navigator Application) adalah dashboard komprehensif 
              untuk analisis data Indeks Kerentanan Sosial (Social Vulnerability Index - SOVI)."),
            br(),
            h4("Sumber Data"),
            p("Dataset yang digunakan berasal dari penelitian:"),
            tags$ul(
              tags$li("Judul: 'A district-level dataset for assessing social vulnerability to natural hazards in Indonesia'"),
              tags$li("Journal: Data in Brief"),
              tags$li("URL: https://www.sciencedirect.com/science/article/pii/S2352340921010180")
            ),
            br(),
            h4("Fitur Dashboard"),
            tags$ul(
              tags$li("📊 Manajemen dan transformasi data"),
              tags$li("🔍 Eksplorasi data dengan statistik deskriptif"),
              tags$li("📈 Uji asumsi statistik"),
              tags$li("🧮 Analisis statistik inferensial"),
              tags$li("📊 ANOVA satu dan dua arah"),
              tags$li("🔗 Regresi linear berganda"),
              tags$li("💾 Download hasil analisis")
            )
          ),
          br(),
          if (!is.null(sovi_data)) {
            div(class = "alert alert-success",
              h4("✓ Data Status"),
              p(paste("Data berhasil dimuat:", nrow(sovi_data), "baris dan", ncol(sovi_data), "kolom"))
            )
          } else {
            div(class = "alert alert-warning",
              h4("⚠️ Data Status"),
              p("Data tidak ditemukan. Pastikan file 'sovi_data.csv' ada di direktori yang sama.")
            )
          }
        )
      )
    ),
    
    # Data Management Tab
    tabPanel("📊 Manajemen Data",
      fluidRow(
        column(4,
          wellPanel(
            h4("Transformasi Data"),
            if (!is.null(sovi_data)) {
              tagList(
                selectInput("var_to_transform", "Pilih Variabel:",
                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                selectInput("method", "Metode Kategorisasi:",
                           choices = list("Equal Intervals" = "equal",
                                        "Quantiles" = "quantile",
                                        "Standard Deviation" = "sd")),
                actionButton("transform_btn", "Transformasi", class = "btn-primary")
              )
            } else {
              p("Data tidak tersedia untuk transformasi.")
            }
          )
        ),
        column(8,
          conditionalPanel(
            condition = "input.transform_btn > 0",
            h4("Hasil Transformasi"),
            DT::dataTableOutput("transformed_table"),
            br(),
            h4("Interpretasi"),
            verbatimTextOutput("transform_interpretation")
          )
        )
      )
    ),
    
    # Data Exploration Tab
    tabPanel("🔍 Eksplorasi Data",
      fluidRow(
        column(4,
          wellPanel(
            h4("Pilihan Analisis"),
            if (!is.null(sovi_data)) {
              tagList(
                selectInput("analysis_type", "Jenis Analisis:",
                           choices = list("Statistik Deskriptif" = "descriptive",
                                        "Korelasi" = "correlation",
                                        "Distribusi" = "distribution")),
                conditionalPanel(
                  condition = "input.analysis_type == 'descriptive'",
                  selectInput("desc_vars", "Pilih Variabel:",
                             choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                             multiple = TRUE)
                ),
                conditionalPanel(
                  condition = "input.analysis_type == 'correlation'",
                  selectInput("corr_vars", "Pilih Variabel untuk Korelasi:",
                             choices = names(sovi_data)[sapply(sovi_data, is.numeric)],
                             multiple = TRUE)
                ),
                conditionalPanel(
                  condition = "input.analysis_type == 'distribution'",
                  selectInput("dist_var", "Pilih Variabel:",
                             choices = names(sovi_data)[sapply(sovi_data, is.numeric)])
                ),
                actionButton("analyze_btn", "Analisis", class = "btn-primary")
              )
            } else {
              p("Data tidak tersedia untuk analisis.")
            }
          )
        ),
        column(8,
          conditionalPanel(
            condition = "input.analyze_btn > 0",
            h4("Hasil Analisis"),
            verbatimTextOutput("analysis_output"),
            br(),
            plotOutput("analysis_plot"),
            br(),
            h4("Interpretasi"),
            verbatimTextOutput("analysis_interpretation")
          )
        )
      )
    ),
    
    # Statistical Tests Tab
    tabPanel("📈 Uji Asumsi",
      fluidRow(
        column(4,
          wellPanel(
            h4("Uji Asumsi Data"),
            if (!is.null(sovi_data)) {
              tagList(
                selectInput("test_type", "Jenis Uji:",
                           choices = list("Uji Normalitas" = "normality",
                                        "Uji Homogenitas" = "homogeneity")),
                selectInput("test_var", "Pilih Variabel:",
                           choices = names(sovi_data)[sapply(sovi_data, is.numeric)]),
                conditionalPanel(
                  condition = "input.test_type == 'homogeneity'",
                  selectInput("group_var", "Variabel Pengelompokan:",
                             choices = names(sovi_data))
                ),
                actionButton("test_btn", "Jalankan Uji", class = "btn-primary")
              )
            } else {
              p("Data tidak tersedia untuk pengujian.")
            }
          )
        ),
        column(8,
          conditionalPanel(
            condition = "input.test_btn > 0",
            h4("Hasil Uji"),
            verbatimTextOutput("test_output"),
            br(),
            h4("Interpretasi"),
            verbatimTextOutput("test_interpretation")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(transformed_data = NULL)
  
  # Data transformation
  observeEvent(input$transform_btn, {
    if (!is.null(sovi_data) && input$var_to_transform != "") {
      var_data <- sovi_data[[input$var_to_transform]]
      
      if (input$method == "equal") {
        breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), 
                     length.out = input$n_categories + 1)
      } else if (input$method == "quantile") {
        breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$n_categories + 1), na.rm = TRUE)
      } else {
        mean_val <- mean(var_data, na.rm = TRUE)
        sd_val <- sd(var_data, na.rm = TRUE)
        breaks <- c(-Inf, mean_val - sd_val, mean_val + sd_val, Inf)
      }
      
      categorical_var <- cut(var_data, breaks = breaks, include.lowest = TRUE,
                            labels = paste("Kategori", 1:input$n_categories))
      
      # Create summary table
      values$transformed_data <- data.frame(
        Original = var_data,
        Categorical = categorical_var,
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Display transformed table
  output$transformed_table <- DT::renderDataTable({
    if (!is.null(values$transformed_data)) {
      DT::datatable(values$transformed_data, options = list(pageLength = 10))
    }
  })
  
  # Transformation interpretation
  output$transform_interpretation <- renderText({
    if (!is.null(values$transformed_data)) {
      paste("Data berhasil dikategorikan menjadi", input$n_categories, "kategori menggunakan metode",
            input$method, ". Variabel baru dapat digunakan untuk analisis selanjutnya.")
    }
  })
  
  # Data analysis
  observeEvent(input$analyze_btn, {
    if (!is.null(sovi_data)) {
      if (input$analysis_type == "descriptive" && length(input$desc_vars) > 0) {
        selected_data <- sovi_data[, input$desc_vars, drop = FALSE]
        
        output$analysis_output <- renderPrint({
          summary(selected_data)
        })
        
        output$analysis_plot <- renderPlot({
          if (length(input$desc_vars) == 1) {
            hist(selected_data[[1]], main = paste("Histogram of", input$desc_vars),
                 xlab = input$desc_vars, col = "lightblue", border = "darkblue")
          } else {
            pairs(selected_data, main = "Scatter Plot Matrix")
          }
        })
        
        output$analysis_interpretation <- renderText({
          paste("Analisis deskriptif menunjukkan distribusi dan karakteristik dari",
                length(input$desc_vars), "variabel yang dipilih.")
        })
      }
    }
  })
  
  # Statistical tests
  observeEvent(input$test_btn, {
    if (!is.null(sovi_data) && input$test_var != "") {
      test_data <- sovi_data[[input$test_var]]
      
      if (input$test_type == "normality") {
        # Shapiro-Wilk test
        if (length(test_data) > 5000) {
          # Use sample for large datasets
          test_data <- sample(test_data, 5000)
        }
        
        test_result <- shapiro.test(test_data)
        
        output$test_output <- renderPrint({
          test_result
        })
        
        output$test_interpretation <- renderText({
          if (test_result$p.value > 0.05) {
            "Hasil uji Shapiro-Wilk menunjukkan bahwa data berdistribusi normal (p > 0.05). 
             Asumsi normalitas terpenuhi untuk analisis parametrik."
          } else {
            "Hasil uji Shapiro-Wilk menunjukkan bahwa data tidak berdistribusi normal (p ≤ 0.05). 
             Pertimbangkan transformasi data atau gunakan uji non-parametrik."
          }
        })
      }
    }
  })
}

# Run the app
cat("Starting SIVANA Dashboard (Minimal Version)...\n")
cat("Access the dashboard at: http://localhost:3838\n")
cat("========================================\n")

shinyApp(ui = ui, server = server)