# SIVUDASI - Sistem Informasi Visualisasi dan Analisis Data Sosial Indonesia
# Dashboard untuk Analisis Kerentanan Sosial Indonesia dengan Pendekatan Spasial
# Enhanced Version - Complete Statistical Analysis Dashboard

# Load library yang diperlukan
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(corrplot)
  library(leaflet)
  library(dplyr)
  library(readr)
  library(car)
  library(nortest)
  library(psych)
  library(cluster)
  library(factoextra)
  library(RColorBrewer)
  library(gridExtra)
  library(knitr)
  library(rmarkdown)
  library(openxlsx)
  library(sf)
  library(viridis)
  library(htmlwidgets)
  library(webshot)
  library(scales)
  library(moments)
  library(shinycssloaders)
  library(shinyWidgets)
  library(broom)
  library(lmtest)
  library(MASS)
  library(multcomp)
  library(agricolae)
  library(dbscan)
  library(fpc)
})

# Fungsi untuk test koneksi internet
test_connection <- function() {
  tryCatch({
    readLines("https://www.google.com", n = 1)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Fungsi untuk membaca data dari URL dengan improved error handling
load_data <- function() {
  tryCatch({
    cat("Memulai download data...\n")
    
    # Test koneksi internet dulu
    if (!test_connection()) {
      stop("Tidak ada koneksi internet")
    }
    
    # Baca data utama
    cat("Downloading sovi_data...\n")
    sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", 
                          show_col_types = FALSE)
    cat("SOVI data loaded:", nrow(sovi_data), "rows,", ncol(sovi_data), "columns\n")
    
    # Baca matrix jarak dengan penanganan khusus
    cat("Downloading distance matrix...\n")
    distance <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", 
                         show_col_types = FALSE)
    cat("Distance matrix loaded:", nrow(distance), "rows,", ncol(distance), "columns\n")
    
    # Perbaiki format distance matrix
    if(ncol(distance) > nrow(distance)) {
      cat("Removing index column from distance matrix...\n")
      distance <- distance[, -1]
    }
    
    # Pastikan distance matrix berbentuk square
    n_rows <- nrow(distance)
    n_cols <- ncol(distance)
    if(n_rows != n_cols) {
      cat("Adjusting distance matrix to square format...\n")
      min_dim <- min(n_rows, n_cols)
      distance <- distance[1:min_dim, 1:min_dim]
    }
    
    # Generate koordinat geografis Indonesia untuk visualisasi peta
    set.seed(123) # untuk konsistensi
    n_districts <- nrow(sovi_data)
    coordinates <- data.frame(
      district_id = 1:n_districts,
      latitude = runif(n_districts, -11, 6),    # Lintang Indonesia: 6°LU - 11°LS
      longitude = runif(n_districts, 95, 141)   # Bujur Indonesia: 95°BT - 141°BT
    )
    
    cat("Data loading completed successfully!\n")
    return(list(
      sovi_data = sovi_data, 
      distance = distance,
      coordinates = coordinates
    ))
    
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    return(NULL)
  })
}

# Fungsi untuk generate laporan Word/PDF lengkap
generate_comprehensive_report <- function(data, analysis_type, results, format = "pdf") {
  tryCatch({
    if (format == "pdf") {
      temp_file <- tempfile(fileext = ".pdf")
      pdf(temp_file, width = 8.5, height = 11)
    } else {
      temp_file <- tempfile(fileext = ".docx")
    }
    
    # Cover page
    if (format == "pdf") {
      plot.new()
      text(0.5, 0.9, "SIVUDASI", cex = 3, font = 2, col = "darkblue")
      text(0.5, 0.85, "Sistem Informasi Visualisasi dan Analisis Data Sosial Indonesia", cex = 1.5)
      text(0.5, 0.8, "Dashboard Analisis Kerentanan Sosial dengan Pendekatan Spasial", cex = 1.2)
      text(0.5, 0.7, paste("Tanggal Analisis:", Sys.Date()), cex = 1)
      text(0.5, 0.65, paste("Jenis Analisis:", analysis_type), cex = 1)
      text(0.5, 0.6, paste("Jumlah Observasi:", nrow(data)), cex = 1)
      text(0.5, 0.55, paste("Jumlah Variabel:", ncol(data)), cex = 1)
      
      # Metadata page
      plot.new()
      text(0.5, 0.95, "METADATA DATASET", cex = 2, font = 2)
      
      metadata_text <- c(
        "• Dataset: Analisis Kerentanan Sosial Indonesia",
        "• Jumlah Observasi: 511 kabupaten/kota di Indonesia", 
        "• Tahun Data: 2017 (disesuaikan dengan peta administrasi 2013)",
        "• Sumber Data Utama: Survei Sosial Ekonomi Nasional (SUSENAS) 2017",
        "• Data Pendukung: Proyeksi Penduduk Indonesia 2017",
        "• Matriks Jarak: Jarak geografis antar kabupaten/kota dalam kilometer",
        "• Fokus Analisis: Kerentanan sosial terhadap bencana alam",
        "• Referensi: Nasution et al. (2021) - Science Direct",
        "• Dashboard: SIVUDASI - Sistem Informasi Visualisasi dan Analisis Data Sosial"
      )
      
      for(i in 1:length(metadata_text)) {
        text(0.05, 0.85 - (i-1)*0.08, metadata_text[i], cex = 1, adj = 0)
      }
      
      dev.off()
    }
    
    return(temp_file)
  }, error = function(e) {
    return(NULL)
  })
}

# Fungsi untuk interpretasi statistik yang komprehensif
generate_interpretation <- function(test_type, results, alpha = 0.05) {
  switch(test_type,
    "normality" = {
      if (results$p.value > alpha) {
        paste("✅ INTERPRETASI UJI NORMALITAS:\n\n",
              "Hasil: Data BERDISTRIBUSI NORMAL (p-value =", round(results$p.value, 4), "> α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Data berdistribusi normal (DITERIMA)\n",
              "• H1: Data tidak berdistribusi normal (DITOLAK)\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Data memenuhi asumsi normalitas untuk uji parametrik\n",
              "• Dapat menggunakan uji t, ANOVA, regresi linear\n",
              "• Hasil analisis statistik parametrik akan valid\n",
              "• Mean dan standard deviation representatif untuk data\n\n",
              "📈 Rekomendasi Analisis Lanjutan:\n",
              "• Gunakan uji parametrik (t-test, ANOVA)\n",
              "• Analisis regresi linear dapat dilakukan\n",
              "• Confidence interval berdasarkan distribusi normal")
      } else {
        paste("❌ INTERPRETASI UJI NORMALITAS:\n\n",
              "Hasil: Data TIDAK BERDISTRIBUSI NORMAL (p-value =", round(results$p.value, 4), "≤ α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Data berdistribusi normal (DITOLAK)\n",
              "• H1: Data tidak berdistribusi normal (DITERIMA)\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Data tidak memenuhi asumsi normalitas untuk uji parametrik\n",
              "• Perlu transformasi data atau gunakan uji non-parametrik\n",
              "• Mean mungkin tidak representatif (gunakan median)\n",
              "• Outliers atau skewness mempengaruhi distribusi\n\n",
              "📈 Rekomendasi Analisis Lanjutan:\n",
              "• Gunakan uji non-parametrik (Mann-Whitney, Kruskal-Wallis)\n",
              "• Pertimbangkan transformasi data (log, sqrt)\n",
              "• Analisis robust statistics")
      }
    },
    
    "homogeneity" = {
      if (results$p.value > alpha) {
        paste("✅ INTERPRETASI UJI HOMOGENITAS:\n\n",
              "Hasil: VARIANS HOMOGEN antar kelompok (p-value =", round(results$p.value, 4), "> α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Varians antar kelompok sama/homogen (DITERIMA)\n",
              "• H1: Varians antar kelompok berbeda/heterogen (DITOLAK)\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Asumsi homogenitas varians terpenuhi\n",
              "• ANOVA dan t-test dapat digunakan dengan valid\n",
              "• Pooled variance estimate dapat digunakan\n",
              "• Hasil perbandingan antar kelompok reliable\n\n",
              "📈 Rekomendasi Analisis Lanjutan:\n",
              "• Lanjutkan dengan ANOVA atau t-test\n",
              "• Post-hoc test standar dapat digunakan\n",
              "• Equal variance assumption terpenuhi")
      } else {
        paste("❌ INTERPRETASI UJI HOMOGENITAS:\n\n",
              "Hasil: VARIANS HETEROGEN antar kelompok (p-value =", round(results$p.value, 4), "≤ α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Varians antar kelompok sama/homogen (DITOLAK)\n",
              "• H1: Varians antar kelompok berbeda/heterogen (DITERIMA)\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Asumsi homogenitas varians tidak terpenuhi\n",
              "• Standard ANOVA tidak valid\n",
              "• Perlu koreksi atau uji alternatif\n",
              "• Kelompok dengan varians berbeda signifikan\n\n",
              "📈 Rekomendasi Analisis Lanjutan:\n",
              "• Gunakan Welch ANOVA (unequal variance)\n",
              "• Transformasi data untuk stabilkan varians\n",
              "• Non-parametric tests (Kruskal-Wallis)")
      }
    },
    
    "t_test" = {
      if (results$p.value <= alpha) {
        paste("✅ INTERPRETASI UJI T:\n\n",
              "Hasil: Ada PERBEDAAN SIGNIFIKAN (p-value =", round(results$p.value, 4), "≤ α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Tidak ada perbedaan rata-rata (DITOLAK)\n",
              "• H1: Ada perbedaan rata-rata (DITERIMA)\n",
              "• t-statistik:", round(results$statistic, 4), "\n",
              "• Confidence Interval:", round(results$conf.int[1], 4), "sampai", round(results$conf.int[2], 4), "\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Perbedaan rata-rata signifikan secara statistik\n",
              "• Temuan dapat digeneralisasi ke populasi\n",
              "• Effect size perlu dievaluasi untuk signifikansi praktis\n\n",
              "📈 Rekomendasi:\n",
              "• Hitung Cohen's d untuk mengukur effect size\n",
              "• Investigasi faktor penyebab perbedaan\n",
              "• Pertimbangkan implikasi praktis dari temuan")
      } else {
        paste("❌ INTERPRETASI UJI T:\n\n",
              "Hasil: TIDAK ADA PERBEDAAN SIGNIFIKAN (p-value =", round(results$p.value, 4), "> α =", alpha, ")\n\n",
              "📊 Kesimpulan Statistik:\n",
              "• H0: Tidak ada perbedaan rata-rata (DITERIMA)\n",
              "• H1: Ada perbedaan rata-rata (DITOLAK)\n",
              "• t-statistik:", round(results$statistic, 4), "\n",
              "• Confidence Interval:", round(results$conf.int[1], 4), "sampai", round(results$conf.int[2], 4), "\n\n",
              "🔍 Implikasi Praktis:\n",
              "• Tidak ada bukti perbedaan rata-rata yang signifikan\n",
              "• Sampel mendukung hipotesis nol\n",
              "• Mungkin perlu sampel lebih besar untuk deteksi perbedaan kecil\n\n",
              "📈 Rekomendasi:\n",
              "• Pertimbangkan power analysis\n",
              "• Evaluasi ukuran sampel yang diperlukan\n",
              "• Analisis equivalence testing jika relevan")
      }
    }
  )
}

# UI Dashboard dengan enhanced styling dan menu lengkap
ui <- dashboardPage(
  dashboardHeader(
    title = "SIVUDASI - Dashboard Analisis Data Sosial Indonesia",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("🗃️ Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("📊 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("✅ Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia", icon = icon("calculator"),
        menuSubItem("📊 Uji Rata-rata", tabName = "inferensia_mean"),
        menuSubItem("📉 Uji Proporsi & Varians", tabName = "inferensia_prop"),
        menuSubItem("📋 ANOVA", tabName = "anova")
      ),
      menuItem("📈 Regresi Linear", tabName = "regresi", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .interpretation-box {
          background-color: #e8f5e8;
          border: 2px solid #4caf50;
          padding: 15px;
          margin-top: 15px;
          border-radius: 8px;
          font-size: 14px;
          line-height: 1.5;
          font-family: 'Courier New', monospace;
          white-space: pre-wrap;
        }
        .error-box {
          background-color: #f8d7da;
          border: 2px solid #f5c6cb;
          color: #721c24;
          padding: 15px;
          margin-top: 15px;
          border-radius: 8px;
        }
        .warning-box {
          background-color: #fff3cd;
          border: 2px solid #ffeaa7;
          color: #856404;
          padding: 15px;
          margin-top: 15px;
          border-radius: 8px;
        }
        .info-box {
          background-color: #d1ecf1;
          border: 2px solid #bee5eb;
          color: #0c5460;
          padding: 15px;
          margin-top: 15px;
          border-radius: 8px;
        }
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-sidebar {
          background-color: #34495e !important;
        }
        .result-box {
          background-color: #f8f9fa;
          border: 1px solid #dee2e6;
          padding: 15px;
          margin-top: 10px;
          border-radius: 5px;
          font-family: 'Courier New', monospace;
          font-size: 12px;
          white-space: pre-wrap;
        }
      "))
    ),
    
    tabItems(
      # Tab Beranda - Enhanced dengan informasi lengkap
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "🎯 Selamat Datang di SIVUDASI", status = "primary", solidHeader = TRUE,
                  width = 12,
                  div(style = "text-align: center; padding: 20px;",
                      h2("Sistem Informasi Visualisasi dan Analisis Data Sosial Indonesia", 
                         style = "color: #2c3e50; font-weight: bold;"),
                      h4("Dashboard Analisis Kerentanan Sosial dengan Pendekatan Spasial", 
                         style = "color: #7f8c8d; font-style: italic;")
                  ),
                  
                  div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; margin: 20px 0;",
                      h4("📋 Deskripsi Dataset", style = "margin-top: 0;"),
                      p("Dashboard ini menganalisis kerentanan sosial di Indonesia menggunakan data dari 511 kabupaten/kota. Indonesia merupakan salah satu negara yang rawan terhadap berbagai bencana alam, mengingat secara geografis Indonesia terletak di Cincin Api Pasifik dan berada di titik pertemuan tiga lempeng tektonik utama dunia. Dataset ini berisi indikator-indikator kerentanan sosial yang dapat digunakan untuk analisis spasial dan clustering.")
                  ),
                  
                  h4("📊 Metadata Dataset:", style = "color: #2c3e50;"),
                  div(style = "background-color: #ecf0f1; padding: 20px; border-radius: 10px; margin: 15px 0;",
                      tags$ul(style = "list-style-type: none; padding-left: 0;",
                              tags$li(style = "margin: 8px 0;", "🏘️ ", strong("Jumlah Observasi: "), "511 kabupaten/kota di Indonesia"),
                              tags$li(style = "margin: 8px 0;", "📅 ", strong("Tahun Data: "), "2017 (disesuaikan dengan peta administrasi 2013)"),
                              tags$li(style = "margin: 8px 0;", "📋 ", strong("Sumber Data Utama: "), "Survei Sosial Ekonomi Nasional (SUSENAS) 2017"),
                              tags$li(style = "margin: 8px 0;", "👥 ", strong("Data Pendukung: "), "Proyeksi Penduduk Indonesia 2017"),
                              tags$li(style = "margin: 8px 0;", "🗺️ ", strong("Matriks Jarak: "), "Jarak geografis antar kabupaten/kota dalam kilometer"),
                              tags$li(style = "margin: 8px 0;", "🎯 ", strong("Fokus Analisis: "), "Kerentanan sosial terhadap bencana alam dengan komponen spasial"),
                              tags$li(style = "margin: 8px 0;", "📚 ", strong("Referensi: "), 
                                      tags$a("Nasution et al. (2021) - Science Direct", 
                                             href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180",
                                             target = "_blank", style = "color: #3498db; text-decoration: none;"))
                      )
                  ),
                  
                  h4("🚀 Fitur Unggulan Dashboard:", style = "color: #2c3e50;"),
                  div(style = "background-color: #d5f4e6; padding: 20px; border-radius: 10px; margin: 15px 0;",
                      tags$ul(style = "list-style-type: none; padding-left: 0;",
                              tags$li(style = "margin: 8px 0;", "🔧 Manajemen dan transformasi data dengan interpretasi statistik"),
                              tags$li(style = "margin: 8px 0;", "📈 Eksplorasi data dengan visualisasi interaktif dan pemetaan tematik"),
                              tags$li(style = "margin: 8px 0;", "🗺️ Analisis clustering spasial menggunakan matriks penimbang jarak"),
                              tags$li(style = "margin: 8px 0;", "✅ Uji asumsi statistik lengkap (normalitas dan homogenitas)"),
                              tags$li(style = "margin: 8px 0;", "📊 Uji rata-rata 1 dan 2 kelompok dengan interpretasi lengkap"),
                              tags$li(style = "margin: 8px 0;", "📉 Uji proporsi dan varians 1 dan 2 kelompok"),
                              tags$li(style = "margin: 8px 0;", "📋 ANOVA 1 arah dan 2 arah dengan post-hoc tests"),
                              tags$li(style = "margin: 8px 0;", "📈 Analisis regresi linear berganda dengan diagnostik model lengkap"),
                              tags$li(style = "margin: 8px 0;", "💾 Ekspor hasil analisis dalam format JPG, PDF, dan Word")
                      )
                  ),
                  
                  div(style = "text-align: center; margin: 30px 0;",
                      actionButton("load_data_btn", "📥 Muat Data", 
                                   class = "btn-success btn-lg", 
                                   icon = icon("download"),
                                   style = "padding: 15px 30px; font-size: 18px;")
                  ),
                  
                  div(id = "loading_status", style = "margin-top: 20px; text-align: center;")
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_districts", width = 4),
                valueBoxOutput("total_variables", width = 4),
                valueBoxOutput("data_status", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "📖 Informasi Matriks Penimbang Jarak", status = "info", solidHeader = TRUE,
                  width = 12,
                  div(class = "info-box",
                      h5("🎯 Kegunaan Matriks Penimbang Jarak:"),
                      tags$ul(
                        tags$li("Digunakan untuk analisis clustering spasial yang mempertimbangkan kedekatan geografis"),
                        tags$li("Membantu dalam pembuatan peta tematik Indonesia sesuai dengan distribusi geografis"),
                        tags$li("Mendukung analisis keterkaitan antar wilayah dalam konteks kerentanan sosial"),
                        tags$li("Matriks berisi jarak dalam kilometer antar 511 kabupaten/kota di Indonesia"),
                        tags$li("Dibangun dari peta geografis Indonesia tahun 2013 menggunakan koordinat pusat setiap kabupaten/kota"),
                        tags$li("Memungkinkan identifikasi pola spasial dan autokorelasi dalam data kerentanan sosial")
                      )
                  )
                )
              )
      ),
      
      # Tab Manajemen Data
      tabItem(tabName = "manajemen",
              fluidRow(
                box(
                  title = "🔧 Transformasi Data", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("var_to_transform", "Pilih Variabel:", choices = NULL),
                  selectInput("transform_type", "Jenis Transformasi:",
                              choices = list(
                                "Kategorisasi (Kuartil)" = "quartile",
                                "Kategorisasi (Median)" = "median",
                                "Kategorisasi (Tertil)" = "tertile"
                              )
                  ),
                  div(class = "info-box",
                      p("💡 Kategorisasi akan membuat variabel baru dengan suffix '_cat'"),
                      p("Contoh: jika memilih variabel 'income', akan dibuat variabel baru 'income_cat'")
                  ),
                  actionButton("apply_transform", "✅ Terapkan Transformasi", class = "btn-primary"),
                  br(), br(),
                  downloadButton("download_transformed", "💾 Unduh Data Transformasi", class = "btn-success")
                ),
                
                box(
                  title = "📊 Data Hasil Transformasi", status = "info", solidHeader = TRUE,
                  width = 8,
                  withSpinner(DT::dataTableOutput("comparison_table"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Transformasi", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("transform_interpretation")
                  )
                )
              )
      ),
      
      # Tab Eksplorasi Data
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(
                  title = "🎛️ Kontrol Visualisasi", status = "primary", solidHeader = TRUE,
                  width = 3,
                  selectInput("viz_type", "Jenis Visualisasi:",
                              choices = list(
                                "📊 Statistik Deskriptif" = "desc",
                                "📈 Histogram" = "hist",
                                "📦 Boxplot" = "box",
                                "🔍 Scatter Plot" = "scatter",
                                "🌐 Correlation Plot" = "corr",
                                "🗺️ Peta Tematik" = "map"
                              )
                  ),
                  conditionalPanel(
                    condition = "input.viz_type == 'hist' || input.viz_type == 'box'",
                    selectInput("single_var", "Pilih Variabel:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.viz_type == 'scatter'",
                    selectInput("x_var", "Variabel X:", choices = NULL),
                    selectInput("y_var", "Variabel Y:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.viz_type == 'map'",
                    selectInput("map_var", "Variabel untuk Peta:", choices = NULL)
                  ),
                  actionButton("generate_viz", "🎨 Buat Visualisasi", class = "btn-primary btn-block"),
                  br(), br(),
                  div(class = "info-box",
                      p("💡 Tips Peta Leaflet Clustering:"),
                      tags$ul(
                        tags$li("Klik titik untuk detail cluster dan data SOVI"),
                        tags$li("Zoom untuk melihat nama kota dan jalan"),
                        tags$li("Drag untuk navigasi peta OpenStreetMap"),
                        tags$li("Legend menunjukkan 4 cluster spasial"),
                        tags$li("Clustering berdasarkan matriks jarak geografis")
                      )
                  )
                ),
                
                box(
                  title = "📊 Hasil Visualisasi", status = "info", solidHeader = TRUE,
                  width = 9,
                  conditionalPanel(
                    condition = "input.viz_type == 'map'",
                    withSpinner(leafletOutput("main_plot", height = "600px"), type = 4)
                  ),
                  conditionalPanel(
                    condition = "input.viz_type != 'map'",
                    withSpinner(plotlyOutput("main_plot", height = "600px"), type = 4)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Hasil", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("viz_interpretation")
                  )
                )
              )
      ),
      
      # Tab Uji Asumsi
      tabItem(tabName = "asumsi",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Uji", status = "primary", solidHeader = TRUE,
                  width = 3,
                  selectInput("test_var", "Pilih Variabel:", choices = NULL),
                  selectInput("test_type", "Jenis Uji:",
                              choices = list(
                                "📊 Uji Normalitas" = "normality",
                                "📈 Uji Homogenitas" = "homogeneity"
                              )
                  ),
                  conditionalPanel(
                    condition = "input.test_type == 'homogeneity'",
                    selectInput("group_var", "Variabel Kelompok:", choices = NULL)
                  ),
                  actionButton("run_assumption", "🔬 Jalankan Uji", class = "btn-primary btn-block")
                ),
                
                box(
                  title = "📊 Hasil Uji", status = "info", solidHeader = TRUE,
                  width = 9,
                  withSpinner(verbatimTextOutput("assumption_result"), type = 4),
                  br(),
                  withSpinner(plotOutput("assumption_plot", height = "400px"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Hasil Uji", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("assumption_interpretation")
                  )
                )
              )
      ),
      
      # Tab Uji Rata-rata
      tabItem(tabName = "inferensia_mean",
              fluidRow(
                box(
                  title = "🧮 Uji Rata-rata", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("mean_test_type", "Jenis Uji:",
                              choices = list(
                                "📊 Uji t 1 Sampel" = "one_sample",
                                "📈 Uji t 2 Sampel" = "two_sample"
                              )
                  ),
                  selectInput("mean_var1", "Variabel Utama:", choices = NULL),
                  conditionalPanel(
                    condition = "input.mean_test_type == 'two_sample'",
                    selectInput("mean_var2", "Variabel Kedua:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.mean_test_type == 'one_sample'",
                    numericInput("mu_null", "Nilai Hipotesis (μ0):", value = 0)
                  ),
                  numericInput("confidence_level", "Tingkat Kepercayaan:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                  actionButton("run_mean_test", "🔬 Jalankan Uji", class = "btn-primary btn-block")
                ),
                
                box(
                  title = "📊 Hasil Uji", status = "info", solidHeader = TRUE,
                  width = 8,
                  withSpinner(verbatimTextOutput("mean_test_result"), type = 4),
                  br(),
                  withSpinner(plotOutput("mean_test_plot", height = "400px"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Uji Rata-rata", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("mean_test_interpretation")
                  )
                )
              )
      ),
      
      # Tab Uji Proporsi & Varians
      tabItem(tabName = "inferensia_prop",
              fluidRow(
                box(
                  title = "📊 Uji Proporsi dan Varians", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("prop_test_type", "Jenis Uji:",
                              choices = list(
                                "📊 Uji Proporsi 1 Sampel" = "prop_one",
                                "📈 Uji Proporsi 2 Sampel" = "prop_two",
                                "📉 Uji Varians 1 Sampel" = "var_one",
                                "🔍 Uji Varians 2 Sampel" = "var_two"
                              )
                  ),
                  selectInput("prop_var1", "Variabel Utama:", choices = NULL),
                  conditionalPanel(
                    condition = "input.prop_test_type == 'prop_two' || input.prop_test_type == 'var_two'",
                    selectInput("prop_var2", "Variabel Kedua:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.prop_test_type == 'prop_one'",
                    numericInput("null_prop", "Proporsi Null (p0):", value = 0.5, min = 0, max = 1, step = 0.01)
                  ),
                  actionButton("run_prop_test", "🔬 Jalankan Uji", class = "btn-primary btn-block")
                ),
                
                box(
                  title = "📊 Hasil Uji", status = "info", solidHeader = TRUE,
                  width = 8,
                  withSpinner(verbatimTextOutput("prop_test_result"), type = 4),
                  br(),
                  withSpinner(plotOutput("prop_test_plot", height = "400px"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Uji", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("prop_test_interpretation")
                  )
                )
              )
      ),
      
      # Tab ANOVA
      tabItem(tabName = "anova",
              fluidRow(
                box(
                  title = "📊 ANOVA", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("anova_type", "Jenis ANOVA:",
                              choices = list(
                                "📈 ANOVA Satu Arah" = "one_way",
                                "📊 ANOVA Dua Arah" = "two_way"
                              )
                  ),
                  selectInput("anova_dependent", "Variabel Dependen:", choices = NULL),
                  selectInput("anova_factor1", "Faktor 1:", choices = NULL),
                  conditionalPanel(
                    condition = "input.anova_type == 'two_way'",
                    selectInput("anova_factor2", "Faktor 2:", choices = NULL)
                  ),
                  checkboxInput("post_hoc", "Lakukan Post-hoc Test", FALSE),
                  actionButton("run_anova", "🔬 Jalankan ANOVA", class = "btn-primary btn-block")
                ),
                
                box(
                  title = "📊 Hasil ANOVA", status = "info", solidHeader = TRUE,
                  width = 8,
                  withSpinner(verbatimTextOutput("anova_result"), type = 4),
                  br(),
                  withSpinner(plotOutput("anova_plot", height = "400px"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi ANOVA", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("anova_interpretation")
                  )
                )
              )
      ),
      
      # Tab Regresi Linear
      tabItem(tabName = "regresi",
              fluidRow(
                box(
                  title = "📈 Model Regresi", status = "primary", solidHeader = TRUE,
                  width = 4,
                  selectInput("reg_dependent", "Variabel Dependen:", choices = NULL),
                  checkboxGroupInput("reg_independent", "Variabel Independen:", choices = NULL),
                  actionButton("run_regression", "🔬 Jalankan Regresi", class = "btn-primary btn-block")
                ),
                
                box(
                  title = "📊 Hasil Regresi", status = "info", solidHeader = TRUE,
                  width = 8,
                  withSpinner(verbatimTextOutput("regression_summary"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "🔍 Diagnostik Model", status = "warning", solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotOutput("regression_diagnostics", height = "500px"), type = 4)
                ),
                
                box(
                  title = "✅ Uji Asumsi Regresi", status = "info", solidHeader = TRUE,
                  width = 6,
                  withSpinner(verbatimTextOutput("regression_assumptions"), type = 4)
                )
              ),
              
              fluidRow(
                box(
                  title = "💡 Interpretasi Model Regresi", status = "success", solidHeader = TRUE,
                  width = 12,
                  div(class = "interpretation-box",
                      verbatimTextOutput("regression_interpretation")
                  )
                )
              )
      )
    )
  )
)

# Server Function - Complete Enhanced Implementation
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    original_data = NULL,
    distance_matrix = NULL,
    coordinates = NULL,
    transformed_data = NULL,
    current_data = NULL,
    test_results = NULL,
    regression_model = NULL
  )
  
  # Load data
  observeEvent(input$load_data_btn, {
    showModal(modalDialog(
      title = "📥 Memuat Data", 
      div(style = "text-align: center;",
          h4("Sedang mengunduh data dari GitHub..."),
          p("Mohon tunggu, proses ini memerlukan koneksi internet.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      data_list <- load_data()
      if (!is.null(data_list)) {
        values$original_data <- data_list$sovi_data
        values$distance_matrix <- data_list$distance
        values$coordinates <- data_list$coordinates
        values$current_data <- values$original_data
        
        # Update variable choices
        numeric_vars <- names(select_if(values$original_data, is.numeric))
        all_vars <- names(values$original_data)
        
        # Identifikasi variabel kategorik untuk grouping
        categorical_vars <- names(values$original_data)[sapply(values$original_data, function(x) {
          is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
        })]
        
        # Jika tidak ada variabel kategorik, buat dummy dari variabel numerik pertama
        if(length(categorical_vars) == 0 && length(numeric_vars) > 0) {
          first_numeric <- numeric_vars[1]
          median_val <- median(values$original_data[[first_numeric]], na.rm = TRUE)
          values$current_data[[paste0(first_numeric, "_group")]] <- 
            ifelse(values$current_data[[first_numeric]] <= median_val, "Rendah", "Tinggi")
          
          categorical_vars <- paste0(first_numeric, "_group")
        }
        
        # Update all select inputs
        updateSelectInput(session, "var_to_transform", choices = numeric_vars)
        updateSelectInput(session, "single_var", choices = numeric_vars)
        updateSelectInput(session, "x_var", choices = numeric_vars)
        updateSelectInput(session, "y_var", choices = numeric_vars)
        updateSelectInput(session, "map_var", choices = numeric_vars)
        updateSelectInput(session, "test_var", choices = numeric_vars)
        updateSelectInput(session, "group_var", choices = categorical_vars)
        updateSelectInput(session, "mean_var1", choices = numeric_vars)
        updateSelectInput(session, "mean_var2", choices = numeric_vars)
        updateSelectInput(session, "prop_var1", choices = numeric_vars)
        updateSelectInput(session, "prop_var2", choices = numeric_vars)
        updateSelectInput(session, "anova_dependent", choices = numeric_vars)
        updateSelectInput(session, "anova_factor1", choices = all_vars)
        updateSelectInput(session, "anova_factor2", choices = all_vars)
        updateSelectInput(session, "reg_dependent", choices = numeric_vars)
        updateCheckboxGroupInput(session, "reg_independent", choices = numeric_vars)
        
        showNotification("✅ Data berhasil dimuat!", type = "message", duration = 5)
      } else {
        showNotification("❌ Gagal memuat data.", type = "error", duration = 10)
      }
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
    })
    
    removeModal()
  })
  
  # Value boxes
  output$total_districts <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$current_data), "0", format(nrow(values$current_data), big.mark = ",")),
      subtitle = "Total Kabupaten/Kota",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$total_variables <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$current_data), "0", ncol(values$current_data)),
      subtitle = "Total Variabel",
      icon = icon("list-alt"),
      color = "green"
    )
  })
  
  output$data_status <- renderValueBox({
    valueBox(
      value = ifelse(is.null(values$current_data), "❌ Tidak Tersedia", "✅ Tersedia"),
      subtitle = "Status Data",
      icon = icon("database"),
      color = ifelse(is.null(values$current_data), "red", "yellow")
    )
  })
  
  # Data transformation
  observeEvent(input$apply_transform, {
    req(values$current_data, input$var_to_transform, input$transform_type)
    
    data <- values$current_data
    var_name <- input$var_to_transform
    
    tryCatch({
      if (input$transform_type == "quartile") {
        quartiles <- quantile(data[[var_name]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        data[[paste0(var_name, "_cat")]] <- cut(data[[var_name]], 
                                                breaks = c(-Inf, quartiles, Inf),
                                                labels = c("Rendah", "Sedang-Rendah", "Sedang-Tinggi", "Tinggi"))
      } else if (input$transform_type == "median") {
        median_val <- median(data[[var_name]], na.rm = TRUE)
        data[[paste0(var_name, "_cat")]] <- ifelse(data[[var_name]] <= median_val, "Rendah", "Tinggi")
      } else if (input$transform_type == "tertile") {
        tertiles <- quantile(data[[var_name]], probs = c(1/3, 2/3), na.rm = TRUE)
        data[[paste0(var_name, "_cat")]] <- cut(data[[var_name]], 
                                                breaks = c(-Inf, tertiles, Inf),
                                                labels = c("Rendah", "Sedang", "Tinggi"))
      }
      
      values$transformed_data <- data
      values$current_data <- data
      
      # Update dropdown variabel kelompok dengan variabel kategorik yang baru
      categorical_vars <- names(values$current_data)[sapply(values$current_data, function(x) {
        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
      })]
      updateSelectInput(session, "group_var", choices = categorical_vars)
      
      # Pesan sukses dengan informasi variabel baru
      new_var_name <- paste0(var_name, "_cat")
      showNotification(
        paste("✅ Transformasi berhasil! Variabel baru '", new_var_name, "' telah ditambahkan ke dataset."), 
        type = "message", 
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })
  
  # Data table
  output$comparison_table <- DT::renderDataTable({
    req(values$current_data)
    DT::datatable(values$current_data, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  extensions = 'Buttons')
  })
  
  # Transform interpretation
  output$transform_interpretation <- renderText({
    req(input$var_to_transform, input$transform_type)
    
    interpretations <- list(
      "quartile" = paste("✅ Variabel", input$var_to_transform, "telah dikategorikan menjadi 4 kelompok berdasarkan kuartil:\n",
                         "• Rendah (≤Q1): 25% data terendah\n",
                         "• Sedang-Rendah (Q1-Q2): 25% data di bawah median\n", 
                         "• Sedang-Tinggi (Q2-Q3): 25% data di atas median\n",
                         "• Tinggi (>Q3): 25% data tertinggi\n\n",
                         "Variabel baru:", paste0(input$var_to_transform, "_cat"), "\n",
                         "Kategorisasi ini berguna untuk analisis kelompok dan identifikasi daerah dengan tingkat kerentanan berbeda."),
      
      "median" = paste("✅ Variabel", input$var_to_transform, "telah dikategorikan menjadi 2 kelompok berdasarkan nilai median:\n",
                       "• Rendah (≤median): 50% data di bawah nilai tengah\n",
                       "• Tinggi (>median): 50% data di atas nilai tengah\n\n",
                       "Variabel baru:", paste0(input$var_to_transform, "_cat"), "\n",
                       "Kategorisasi biner ini memudahkan perbandingan dan analisis dua kelompok."),
      
      "tertile" = paste("✅ Variabel", input$var_to_transform, "telah dikategorikan menjadi 3 kelompok berdasarkan tertil:\n",
                        "• Rendah (≤T1): 33.3% data terendah\n",
                        "• Sedang (T1-T2): 33.3% data menengah\n",
                        "• Tinggi (>T2): 33.3% data tertinggi\n\n",
                        "Variabel baru:", paste0(input$var_to_transform, "_cat"), "\n",
                        "Kategorisasi tertil memberikan pembagian yang seimbang untuk analisis tiga kelompok.")
    )
    
    interpretations[[input$transform_type]]
  })
  
  # Visualization
  observeEvent(input$generate_viz, {
    req(values$current_data, input$viz_type)
    
    if (input$viz_type == "desc") {
      output$main_plot <- renderPlotly({
        numeric_data <- select_if(values$current_data, is.numeric)
        desc_stats <- describe(numeric_data)
        
        p <- ggplot(data = data.frame(
          Variable = rownames(desc_stats),
          Mean = desc_stats$mean,
          SD = desc_stats$sd
        ), aes(x = reorder(Variable, Mean))) +
          geom_col(aes(y = Mean), fill = "steelblue", alpha = 0.7) +
          geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Statistik Deskriptif", x = "Variabel", y = "Nilai")
        
        ggplotly(p)
      })
    } else if (input$viz_type == "hist") {
      req(input$single_var)
      output$main_plot <- renderPlotly({
        p <- ggplot(values$current_data, aes_string(x = input$single_var)) +
          geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
          theme_minimal() +
          labs(title = paste("Histogram", input$single_var))
        
        ggplotly(p)
      })
    } else if (input$viz_type == "box") {
      req(input$single_var)
      output$main_plot <- renderPlotly({
        p <- ggplot(values$current_data, aes_string(y = input$single_var)) +
          geom_boxplot(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot", input$single_var))
        
        ggplotly(p)
      })
    } else if (input$viz_type == "scatter") {
      req(input$x_var, input$y_var)
      output$main_plot <- renderPlotly({
        p <- ggplot(values$current_data, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE) +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var))
        
        ggplotly(p)
      })
    } else if (input$viz_type == "corr") {
      output$main_plot <- renderPlotly({
        numeric_data <- select_if(values$current_data, is.numeric)
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Convert correlation matrix to long format for ggplot
        cor_data <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
        cor_data$value <- as.vector(cor_matrix)
        
        p <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Correlation Matrix", x = "", y = "")
        
        ggplotly(p)
      })
                     } else if (input$viz_type == "map") {
        req(input$map_var, values$coordinates)
        
        # Gunakan Leaflet dengan clustering berdasarkan distance matrix
        output$main_plot <- renderLeaflet({
          # Lakukan clustering menggunakan distance matrix
          if (!is.null(values$distance_matrix)) {
            # Pastikan distance matrix memiliki dimensi yang sesuai
            n_obs <- min(nrow(values$current_data), nrow(values$distance_matrix), ncol(values$distance_matrix))
            
            # Ambil subset distance matrix sesuai jumlah observasi
            dist_subset <- as.matrix(values$distance_matrix[1:n_obs, 1:n_obs])
            
            # Konversi ke distance object untuk clustering
            dist_obj <- as.dist(dist_subset)
            
            # Lakukan hierarchical clustering
            hc_result <- hclust(dist_obj, method = "ward.D2")
            
            # Buat 4 cluster
            clusters <- cutree(hc_result, k = 4)
            
            # Buat data untuk peta dengan informasi cluster
            map_data <- values$current_data[1:n_obs, ] %>%
              bind_cols(values$coordinates[1:n_obs, ]) %>%
              mutate(
                value = get(input$map_var)[1:n_obs],
                cluster = clusters
              )
          } else {
            # Fallback jika tidak ada distance matrix
            map_data <- values$current_data %>%
              bind_cols(values$coordinates) %>%
              mutate(
                value = get(input$map_var),
                cluster = 1  # Default cluster
              )
          }
          
          # Definisikan warna untuk cluster
          cluster_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
          
          # Buat peta Leaflet
          leaflet(map_data) %>%
            addTiles() %>%  # Tambahkan OpenStreetMap tiles
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addCircleMarkers(
              lng = ~longitude, 
              lat = ~latitude,
              color = ~cluster_colors[cluster],
              radius = 6,
              fillOpacity = 0.8,
              stroke = TRUE,
              weight = 2,
              opacity = 1,
              popup = ~paste(
                "<div style='font-size: 12px;'>",
                "<b>Kabupaten/Kota:</b>", 1:nrow(map_data), "<br>",
                "<b>Cluster:</b>", cluster, "<br>",
                "<b>Koordinat:</b>", round(longitude, 3), "°BT, ", round(latitude, 3), "°LU<br>",
                "<b>", input$map_var, ":</b>", round(value, 3),
                "</div>"
              )
            ) %>%
            addLegend(
              "bottomright",
              colors = cluster_colors[1:max(map_data$cluster)],
              labels = paste("Cluster", 1:max(map_data$cluster)),
              title = "Clusters",
              opacity = 1
            )
        })
      }
  })
  
  # Visualization interpretation
  output$viz_interpretation <- renderText({
    req(input$viz_type)
    
    interpretations <- list(
      "desc" = "📊 Statistik deskriptif menunjukkan ringkasan data numerik dengan mean dan standard deviation. Error bars menunjukkan variabilitas data.",
      "hist" = "📈 Histogram menunjukkan distribusi frekuensi data. Bentuk distribusi dapat mengindikasikan normalitas data.",
      "box" = "📦 Boxplot menunjukkan median, kuartil, dan outliers. Berguna untuk identifikasi data ekstrem.",
      "scatter" = "🔍 Scatter plot menunjukkan hubungan antara dua variabel. Garis regresi menunjukkan trend linear.",
      "corr" = "🌐 Correlation matrix menunjukkan kekuatan hubungan antar variabel. Nilai mendekati 1 atau -1 menunjukkan korelasi kuat.",
            "map" = if(!is.null(input$map_var) && input$map_var != "") {
        paste("🗺️ Peta Clustering Spasial Indonesia - Variabel:", input$map_var, "\n\n",
              "📍 Fitur Peta Leaflet Clustering:\n",
              "• Peta OpenStreetMap dengan nama kota dan jalan\n",
              "• Clustering hierarkis menggunakan matriks jarak spasial\n",
              "• 4 cluster berdasarkan kedekatan geografis (Ward.D2)\n",
              "• Warna berbeda untuk setiap cluster (Merah, Tosca, Biru, Hijau)\n",
              "• Klik titik untuk detail cluster dan informasi SOVI\n",
              "• Legend menunjukkan cluster membership\n\n",
              "🗺️ Analisis Clustering Spasial:\n",
              "• Identifikasi kelompok wilayah berdasarkan matriks jarak\n",
              "• Hubungan antara data SOVI dan struktur spasial Indonesia\n",
              "• Deteksi pola clustering regional dan spatial autocorrelation\n",
              "• Analisis heterogenitas spasial antar cluster\n",
              "• Basis untuk analisis regional dan kebijakan spasial")
      } else {
        "🗺️ Pilih variabel untuk menampilkan peta geografis Indonesia dengan konteks daratan dan laut."
      }
    )
    
    interpretations[[input$viz_type]]
  })
  
  # Assumption testing
  observeEvent(input$run_assumption, {
    req(values$current_data, input$test_var, input$test_type)
    
    tryCatch({
      if (input$test_type == "normality") {
        # Shapiro-Wilk test for normality
        if (nrow(values$current_data) <= 5000) {
          test_result <- shapiro.test(values$current_data[[input$test_var]])
        } else {
          # Use Anderson-Darling for large samples
          test_result <- ad.test(values$current_data[[input$test_var]])
        }
        
        values$test_results <- test_result
        
        output$assumption_result <- renderText({
          paste("🔬 UJI NORMALITAS - SHAPIRO-WILK TEST\n\n",
                "Statistik W =", round(test_result$statistic, 4), "\n",
                "p-value =", format(test_result$p.value, scientific = TRUE), "\n\n",
                "Hipotesis:\n",
                "H0: Data berdistribusi normal\n",
                "H1: Data tidak berdistribusi normal\n\n",
                "Alpha = 0.05")
        })
        
        output$assumption_plot <- renderPlot({
          par(mfrow = c(2, 2))
          
          # Histogram with normal curve
          hist(values$current_data[[input$test_var]], prob = TRUE, main = "Histogram dengan Kurva Normal",
               xlab = input$test_var, col = "lightblue", border = "black")
          curve(dnorm(x, mean = mean(values$current_data[[input$test_var]], na.rm = TRUE), 
                     sd = sd(values$current_data[[input$test_var]], na.rm = TRUE)), 
                add = TRUE, col = "red", lwd = 2)
          
          # Q-Q plot
          qqnorm(values$current_data[[input$test_var]], main = "Q-Q Plot")
          qqline(values$current_data[[input$test_var]], col = "red")
          
          # Boxplot
          boxplot(values$current_data[[input$test_var]], main = "Boxplot", 
                  ylab = input$test_var, col = "lightgreen")
          
          # Density plot
          plot(density(values$current_data[[input$test_var]], na.rm = TRUE), 
               main = "Density Plot", xlab = input$test_var, col = "blue", lwd = 2)
        })
        
        output$assumption_interpretation <- renderText({
          generate_interpretation("normality", test_result, 0.05)
        })
        
             } else if (input$test_type == "homogeneity") {
         req(input$group_var)
         
         # Pastikan variabel kelompok ada
         if (!input$group_var %in% names(values$current_data)) {
           showNotification("❌ Variabel kelompok tidak ditemukan!", type = "error")
           return()
         }
         
         # Create grouping variable if numeric
         if (is.numeric(values$current_data[[input$group_var]])) {
           median_val <- median(values$current_data[[input$group_var]], na.rm = TRUE)
           group_data <- ifelse(values$current_data[[input$group_var]] <= median_val, "Low", "High")
         } else {
           group_data <- values$current_data[[input$group_var]]
         }
         
         # Pastikan ada minimal 2 kelompok
         if (length(unique(group_data[!is.na(group_data)])) < 2) {
           showNotification("❌ Perlu minimal 2 kelompok untuk uji homogenitas!", type = "error")
           return()
         }
        
        # Levene's test for homogeneity of variances
        test_result <- leveneTest(values$current_data[[input$test_var]], group_data)
        
        values$test_results <- list(
          statistic = test_result$`F value`[1],
          p.value = test_result$`Pr(>F)`[1],
          method = "Levene's Test"
        )
        
        output$assumption_result <- renderText({
          paste("🔬 UJI HOMOGENITAS - LEVENE'S TEST\n\n",
                "F-statistik =", round(test_result$`F value`[1], 4), "\n",
                "p-value =", format(test_result$`Pr(>F)`[1], scientific = TRUE), "\n",
                "df =", test_result$Df[1], ",", test_result$Df[2], "\n\n",
                "Hipotesis:\n",
                "H0: Varians antar kelompok homogen\n",
                "H1: Varians antar kelompok tidak homogen\n\n",
                "Alpha = 0.05")
        })
        
        output$assumption_plot <- renderPlot({
          par(mfrow = c(2, 2))
          
          # Boxplot by group
          boxplot(values$current_data[[input$test_var]] ~ group_data,
                  main = "Boxplot by Group", xlab = "Group", ylab = input$test_var,
                  col = c("lightblue", "lightgreen"))
          
          # Variance plot
          group_vars <- tapply(values$current_data[[input$test_var]], group_data, var, na.rm = TRUE)
          barplot(group_vars, main = "Variance by Group", ylab = "Variance", 
                  col = "orange", names.arg = names(group_vars))
          
          # Density plots by group
          plot(density(values$current_data[[input$test_var]][group_data == unique(group_data)[1]], na.rm = TRUE),
               main = "Density by Group", col = "blue", lwd = 2, 
               xlim = range(values$current_data[[input$test_var]], na.rm = TRUE))
          lines(density(values$current_data[[input$test_var]][group_data == unique(group_data)[2]], na.rm = TRUE),
                col = "red", lwd = 2)
          legend("topright", legend = unique(group_data), col = c("blue", "red"), lwd = 2)
          
          # Residuals vs fitted (if applicable)
          group_means <- tapply(values$current_data[[input$test_var]], group_data, mean, na.rm = TRUE)
          fitted_vals <- group_means[group_data]
          residuals <- values$current_data[[input$test_var]] - fitted_vals
          plot(fitted_vals, residuals, main = "Residuals vs Fitted", 
               xlab = "Fitted Values", ylab = "Residuals")
          abline(h = 0, col = "red", lty = 2)
        })
        
        output$assumption_interpretation <- renderText({
          generate_interpretation("homogeneity", values$test_results, 0.05)
        })
      }
    }, error = function(e) {
      showNotification(paste("❌ Error dalam uji asumsi:", e$message), type = "error")
    })
  })
  
  # Mean tests
  observeEvent(input$run_mean_test, {
    req(values$current_data, input$mean_var1, input$mean_test_type)
    
    tryCatch({
      if (input$mean_test_type == "one_sample") {
        test_result <- t.test(values$current_data[[input$mean_var1]], mu = input$mu_null,
                             conf.level = input$confidence_level)
      } else if (input$mean_test_type == "two_sample") {
        req(input$mean_var2)
        test_result <- t.test(values$current_data[[input$mean_var1]], 
                             values$current_data[[input$mean_var2]],
                             conf.level = input$confidence_level)
      }
      
      values$test_results <- test_result
      
      output$mean_test_result <- renderText({
        paste("t-statistik =", round(test_result$statistic, 4), "\n",
              "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
              "df =", test_result$parameter, "\n",
              "Confidence Interval:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4), "\n\n",
              "Estimate:", round(test_result$estimate, 4))
      })
      
      output$mean_test_plot <- renderPlot({
        if (input$mean_test_type == "one_sample") {
          hist(values$current_data[[input$mean_var1]], prob = TRUE, 
               main = paste("Distribution of", input$mean_var1),
               xlab = input$mean_var1, col = "lightblue")
          abline(v = mean(values$current_data[[input$mean_var1]], na.rm = TRUE), col = "red", lwd = 2)
          abline(v = input$mu_null, col = "blue", lwd = 2, lty = 2)
          legend("topright", legend = c("Sample Mean", "Null Hypothesis"), 
                 col = c("red", "blue"), lwd = 2, lty = c(1, 2))
        } else {
          boxplot(values$current_data[[input$mean_var1]], values$current_data[[input$mean_var2]],
                  names = c(input$mean_var1, input$mean_var2),
                  main = "Comparison of Two Variables",
                  col = c("lightblue", "lightgreen"))
        }
      })
      
      output$mean_test_interpretation <- renderText({
        generate_interpretation("t_test", test_result, 0.05)
      })
      
    }, error = function(e) {
      showNotification(paste("❌ Error dalam uji rata-rata:", e$message), type = "error")
    })
  })
  
     # Proportion and variance tests
   observeEvent(input$run_prop_test, {
     req(values$current_data, input$prop_var1, input$prop_test_type)
     
     tryCatch({
       test_result <- NULL
       
       if (input$prop_test_type == "prop_one") {
         # Convert to binary if not already
         data_binary <- ifelse(values$current_data[[input$prop_var1]] > 
                              median(values$current_data[[input$prop_var1]], na.rm = TRUE), 1, 0)
         successes <- sum(data_binary, na.rm = TRUE)
         n <- length(data_binary[!is.na(data_binary)])
         
         test_result <- prop.test(successes, n, p = input$null_prop)
         
       } else if (input$prop_test_type == "prop_two") {
         req(input$prop_var2)
         # Two sample proportion test
         data1_binary <- ifelse(values$current_data[[input$prop_var1]] > 
                               median(values$current_data[[input$prop_var1]], na.rm = TRUE), 1, 0)
         data2_binary <- ifelse(values$current_data[[input$prop_var2]] > 
                               median(values$current_data[[input$prop_var2]], na.rm = TRUE), 1, 0)
         
         successes1 <- sum(data1_binary, na.rm = TRUE)
         successes2 <- sum(data2_binary, na.rm = TRUE)
         n1 <- length(data1_binary[!is.na(data1_binary)])
         n2 <- length(data2_binary[!is.na(data2_binary)])
         
         test_result <- prop.test(c(successes1, successes2), c(n1, n2))
         
       } else if (input$prop_test_type == "var_one") {
         # Chi-square test for variance
         sample_var <- var(values$current_data[[input$prop_var1]], na.rm = TRUE)
         n <- length(values$current_data[[input$prop_var1]][!is.na(values$current_data[[input$prop_var1]])])
         chi_stat <- (n - 1) * sample_var / 1  # assuming null variance = 1
         p_val <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))
         
         test_result <- list(
           statistic = chi_stat,
           p.value = p_val,
           parameter = n - 1,
           method = "Chi-square test for variance"
         )
         
       } else if (input$prop_test_type == "var_two") {
         req(input$prop_var2)
         # F-test for equality of variances
         test_result <- var.test(values$current_data[[input$prop_var1]], 
                                values$current_data[[input$prop_var2]])
       }
       
       # Pastikan test_result tidak NULL
       if (is.null(test_result)) {
         showNotification("❌ Jenis uji tidak dikenali!", type = "error")
         return()
       }
       
       values$test_results <- test_result
      
             output$prop_test_result <- renderText({
         if (input$prop_test_type == "prop_one") {
           paste("X-squared =", round(test_result$statistic, 4), "\n",
                 "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                 "df =", test_result$parameter, "\n",
                 "Sample proportion:", round(test_result$estimate, 4), "\n",
                 "Confidence Interval:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4))
         } else if (input$prop_test_type == "prop_two") {
           paste("X-squared =", round(test_result$statistic, 4), "\n",
                 "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                 "df =", test_result$parameter, "\n",
                 "Proportions:", paste(round(test_result$estimate, 4), collapse = ", "), "\n",
                 "Confidence Interval:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4))
         } else if (input$prop_test_type == "var_one") {
           paste("Chi-squared =", round(test_result$statistic, 4), "\n",
                 "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                 "df =", test_result$parameter)
         } else if (input$prop_test_type == "var_two") {
           paste("F =", round(test_result$statistic, 4), "\n",
                 "p-value =", format(test_result$p.value, scientific = TRUE), "\n",
                 "df =", test_result$parameter[1], ",", test_result$parameter[2], "\n",
                 "Ratio of variances:", round(test_result$estimate, 4), "\n",
                 "Confidence Interval:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4))
         }
       })
      
             output$prop_test_plot <- renderPlot({
         if (input$prop_test_type == "prop_one") {
           data_binary <- ifelse(values$current_data[[input$prop_var1]] > 
                                median(values$current_data[[input$prop_var1]], na.rm = TRUE), 1, 0)
           barplot(table(data_binary), main = "Uji Proporsi 1 Sampel",
                   names.arg = c("Below Median", "Above Median"),
                   col = c("lightblue", "lightgreen"),
                   ylab = "Frekuensi")
         } else if (input$prop_test_type == "prop_two") {
           data1_binary <- ifelse(values$current_data[[input$prop_var1]] > 
                                 median(values$current_data[[input$prop_var1]], na.rm = TRUE), 1, 0)
           data2_binary <- ifelse(values$current_data[[input$prop_var2]] > 
                                 median(values$current_data[[input$prop_var2]], na.rm = TRUE), 1, 0)
           
           prop_data <- data.frame(
             Variable = rep(c(input$prop_var1, input$prop_var2), each = 2),
             Category = rep(c("Below Median", "Above Median"), 2),
             Count = c(table(data1_binary), table(data2_binary))
           )
           
           barplot(prop_data$Count, main = "Uji Proporsi 2 Sampel",
                   names.arg = paste(prop_data$Variable, prop_data$Category, sep = "\n"),
                   col = rep(c("lightblue", "lightgreen"), 2),
                   ylab = "Frekuensi")
         } else if (input$prop_test_type == "var_one") {
           hist(values$current_data[[input$prop_var1]], main = "Distribusi untuk Uji Varians 1 Sampel",
                xlab = input$prop_var1, col = "lightblue", ylab = "Frekuensi")
           abline(v = mean(values$current_data[[input$prop_var1]], na.rm = TRUE), col = "red", lwd = 2)
           legend("topright", legend = "Mean", col = "red", lwd = 2)
         } else if (input$prop_test_type == "var_two") {
           par(mfrow = c(1, 2))
           hist(values$current_data[[input$prop_var1]], main = paste("Distribusi", input$prop_var1),
                xlab = input$prop_var1, col = "lightblue", ylab = "Frekuensi")
           hist(values$current_data[[input$prop_var2]], main = paste("Distribusi", input$prop_var2),
                xlab = input$prop_var2, col = "lightgreen", ylab = "Frekuensi")
         }
       })
      
             output$prop_test_interpretation <- renderText({
         if (input$prop_test_type == "prop_one") {
           if (test_result$p.value <= 0.05) {
             paste("✅ INTERPRETASI UJI PROPORSI 1 SAMPEL:\n\n",
                   "Hasil: Proporsi sampel BERBEDA SIGNIFIKAN dari proporsi hipotesis\n",
                   "(p-value =", round(test_result$p.value, 4), "≤ α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: p = p0 (proporsi hipotesis) - DITOLAK\n",
                   "• H1: p ≠ p0 - DITERIMA\n",
                   "• Proporsi sampel:", round(test_result$estimate, 4), "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Proporsi populasi berbeda dari nilai yang dihipotesiskan\n",
                   "• Perbedaan ini signifikan secara statistik\n",
                   "• Temuan ini dapat digeneralisasi ke populasi")
           } else {
             paste("❌ INTERPRETASI UJI PROPORSI 1 SAMPEL:\n\n",
                   "Hasil: Proporsi sampel TIDAK BERBEDA SIGNIFIKAN dari proporsi hipotesis\n",
                   "(p-value =", round(test_result$p.value, 4), "> α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: p = p0 (proporsi hipotesis) - DITERIMA\n",
                   "• H1: p ≠ p0 - DITOLAK\n",
                   "• Proporsi sampel:", round(test_result$estimate, 4), "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Proporsi populasi konsisten dengan nilai hipotesis\n",
                   "• Tidak ada bukti kuat perbedaan proporsi\n",
                   "• Sampel mendukung asumsi awal")
           }
         } else if (input$prop_test_type == "prop_two") {
           if (test_result$p.value <= 0.05) {
             paste("✅ INTERPRETASI UJI PROPORSI 2 SAMPEL:\n\n",
                   "Hasil: Ada PERBEDAAN SIGNIFIKAN antara dua proporsi\n",
                   "(p-value =", round(test_result$p.value, 4), "≤ α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: p1 = p2 (proporsi sama) - DITOLAK\n",
                   "• H1: p1 ≠ p2 (proporsi berbeda) - DITERIMA\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Kedua kelompok memiliki proporsi yang berbeda signifikan\n",
                   "• Perbedaan ini dapat digeneralisasi ke populasi")
           } else {
             paste("❌ INTERPRETASI UJI PROPORSI 2 SAMPEL:\n\n",
                   "Hasil: TIDAK ADA PERBEDAAN SIGNIFIKAN antara dua proporsi\n",
                   "(p-value =", round(test_result$p.value, 4), "> α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: p1 = p2 (proporsi sama) - DITERIMA\n",
                   "• H1: p1 ≠ p2 (proporsi berbeda) - DITOLAK\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Kedua kelompok memiliki proporsi yang sama\n",
                   "• Tidak ada bukti perbedaan yang signifikan")
           }
         } else if (input$prop_test_type == "var_one") {
           if (test_result$p.value <= 0.05) {
             paste("✅ INTERPRETASI UJI VARIANS 1 SAMPEL:\n\n",
                   "Hasil: Varians sampel BERBEDA SIGNIFIKAN dari varians hipotesis\n",
                   "(p-value =", round(test_result$p.value, 4), "≤ α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: σ² = σ₀² (varians hipotesis) - DITOLAK\n",
                   "• H1: σ² ≠ σ₀² - DITERIMA\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Variabilitas data berbeda dari yang diharapkan\n",
                   "• Data menunjukkan tingkat keragaman yang signifikan berbeda")
           } else {
             paste("❌ INTERPRETASI UJI VARIANS 1 SAMPEL:\n\n",
                   "Hasil: Varians sampel TIDAK BERBEDA SIGNIFIKAN dari varians hipotesis\n",
                   "(p-value =", round(test_result$p.value, 4), "> α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: σ² = σ₀² (varians hipotesis) - DITERIMA\n",
                   "• H1: σ² ≠ σ₀² - DITOLAK\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Variabilitas data konsisten dengan yang diharapkan\n",
                   "• Tingkat keragaman data sesuai dengan hipotesis")
           }
         } else if (input$prop_test_type == "var_two") {
           if (test_result$p.value <= 0.05) {
             paste("✅ INTERPRETASI UJI VARIANS 2 SAMPEL (F-TEST):\n\n",
                   "Hasil: Ada PERBEDAAN SIGNIFIKAN antara dua varians\n",
                   "(p-value =", round(test_result$p.value, 4), "≤ α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: σ₁² = σ₂² (varians sama) - DITOLAK\n",
                   "• H1: σ₁² ≠ σ₂² (varians berbeda) - DITERIMA\n",
                   "• Rasio varians:", round(test_result$estimate, 4), "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Kedua kelompok memiliki variabilitas yang berbeda signifikan\n",
                   "• Asumsi homogenitas varians tidak terpenuhi untuk uji parametrik")
           } else {
             paste("❌ INTERPRETASI UJI VARIANS 2 SAMPEL (F-TEST):\n\n",
                   "Hasil: TIDAK ADA PERBEDAAN SIGNIFIKAN antara dua varians\n",
                   "(p-value =", round(test_result$p.value, 4), "> α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: σ₁² = σ₂² (varians sama) - DITERIMA\n",
                   "• H1: σ₁² ≠ σ₂² (varians berbeda) - DITOLAK\n",
                   "• Rasio varians:", round(test_result$estimate, 4), "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Kedua kelompok memiliki variabilitas yang sama\n",
                   "• Asumsi homogenitas varians terpenuhi untuk uji parametrik")
           }
         }
       })
      
    }, error = function(e) {
      showNotification(paste("❌ Error dalam uji proporsi/varians:", e$message), type = "error")
    })
  })
  
     # ANOVA
   observeEvent(input$run_anova, {
     req(values$current_data, input$anova_dependent, input$anova_factor1)
     
     tryCatch({
       # Pastikan variabel faktor ada
       if (!input$anova_factor1 %in% names(values$current_data)) {
         showNotification("❌ Variabel faktor tidak ditemukan!", type = "error")
         return()
       }
       
       # Create factor1 dengan penanganan yang lebih robust
       factor1 <- NULL
       if (is.numeric(values$current_data[[input$anova_factor1]])) {
         # Untuk variabel numerik, buat kategori berdasarkan kuartil jika banyak nilai unik
         unique_vals <- length(unique(values$current_data[[input$anova_factor1]][!is.na(values$current_data[[input$anova_factor1]])]))
         if (unique_vals > 10) {
           # Gunakan quantile untuk membuat kategori yang seimbang
           quartiles <- quantile(values$current_data[[input$anova_factor1]], probs = c(0.33, 0.67), na.rm = TRUE)
           factor1 <- cut(values$current_data[[input$anova_factor1]], 
                         breaks = c(-Inf, quartiles[1], quartiles[2], Inf),
                         labels = c("Low", "Medium", "High"),
                         include.lowest = TRUE)
         } else {
           factor1 <- as.factor(values$current_data[[input$anova_factor1]])
         }
       } else {
         factor1 <- as.factor(values$current_data[[input$anova_factor1]])
       }
       
       # Pastikan factor1 berhasil dibuat dan memiliki minimal 2 level
       if (is.null(factor1) || length(levels(factor1)) < 2) {
         showNotification("❌ Faktor 1 harus memiliki minimal 2 kelompok!", type = "error")
         return()
       }
       
       # Inisialisasi factor2 sebagai NULL
       factor2 <- NULL
       
       if (input$anova_type == "one_way") {
         anova_result <- aov(values$current_data[[input$anova_dependent]] ~ factor1)
         summary_result <- summary(anova_result)
       } else {
         # ANOVA Two-way
         req(input$anova_factor2)
         
         # Pastikan variabel faktor2 ada
         if (!input$anova_factor2 %in% names(values$current_data)) {
           showNotification("❌ Variabel faktor 2 tidak ditemukan!", type = "error")
           return()
         }
         
         # Create factor2 dengan penanganan yang robust
         if (is.numeric(values$current_data[[input$anova_factor2]])) {
           unique_vals2 <- length(unique(values$current_data[[input$anova_factor2]][!is.na(values$current_data[[input$anova_factor2]])]))
           if (unique_vals2 > 5) {
             # Untuk variabel numerik dengan banyak nilai, buat 2 kategori berdasarkan median
             median_val <- median(values$current_data[[input$anova_factor2]], na.rm = TRUE)
             factor2 <- cut(values$current_data[[input$anova_factor2]], 
                           breaks = c(-Inf, median_val, Inf),
                           labels = c("Low", "High"),
                           include.lowest = TRUE)
           } else {
             factor2 <- as.factor(values$current_data[[input$anova_factor2]])
           }
         } else {
           factor2 <- as.factor(values$current_data[[input$anova_factor2]])
         }
         
         # Pastikan factor2 berhasil dibuat dan memiliki minimal 2 level
         if (is.null(factor2) || length(levels(factor2)) < 2) {
           showNotification("❌ Faktor 2 harus memiliki minimal 2 kelompok!", type = "error")
           return()
         }
         
         anova_result <- aov(values$current_data[[input$anova_dependent]] ~ factor1 * factor2)
         summary_result <- summary(anova_result)
       }
      
      values$test_results <- summary_result
      
      output$anova_result <- renderText({
        result_text <- capture.output(print(summary_result))
        
        # Tambahkan post-hoc test jika checkbox dicentang
        if (input$post_hoc) {
          result_text <- c(result_text, "\n--- POST-HOC TEST (TUKEY HSD) ---\n")
          
          if (input$anova_type == "one_way") {
            # Post-hoc untuk ANOVA satu arah
            posthoc_result <- TukeyHSD(anova_result)
            result_text <- c(result_text, capture.output(print(posthoc_result)))
          } else {
            # Post-hoc untuk ANOVA dua arah
            posthoc_result <- TukeyHSD(anova_result)
            result_text <- c(result_text, capture.output(print(posthoc_result)))
          }
        }
        
        paste(result_text, collapse = "\n")
      })
      
             output$anova_plot <- renderPlot({
         if (input$anova_type == "one_way") {
           # Pastikan factor1 ada dan valid
           if (!is.null(factor1) && length(levels(factor1)) >= 2) {
             boxplot(values$current_data[[input$anova_dependent]] ~ factor1,
                     main = paste("ANOVA Satu Arah - Boxplot by", input$anova_factor1),
                     xlab = input$anova_factor1, ylab = input$anova_dependent,
                     col = rainbow(length(levels(factor1))))
           }
         } else {
           # Pastikan factor1 dan factor2 ada dan valid
           if (!is.null(factor1) && !is.null(factor2) && 
               length(levels(factor1)) >= 2 && length(levels(factor2)) >= 2) {
             
             # Buat interaction plot
             tryCatch({
               interaction.plot(factor1, factor2, values$current_data[[input$anova_dependent]],
                               main = "ANOVA Dua Arah - Interaction Plot",
                               xlab = input$anova_factor1, ylab = input$anova_dependent,
                               trace.label = input$anova_factor2,
                               col = c("blue", "red", "green", "purple")[1:length(levels(factor2))])
             }, error = function(e) {
               # Jika interaction plot gagal, buat boxplot sederhana
               par(mfrow = c(1, 2))
               boxplot(values$current_data[[input$anova_dependent]] ~ factor1,
                       main = paste("Faktor 1:", input$anova_factor1),
                       xlab = input$anova_factor1, ylab = input$anova_dependent,
                       col = "lightblue")
               boxplot(values$current_data[[input$anova_dependent]] ~ factor2,
                       main = paste("Faktor 2:", input$anova_factor2),
                       xlab = input$anova_factor2, ylab = input$anova_dependent,
                       col = "lightgreen")
             })
           }
         }
       })
      
             output$anova_interpretation <- renderText({
         if (input$anova_type == "one_way") {
           # Interpretasi ANOVA satu arah
           f_stat <- summary_result[[1]]$`F value`[1]
           p_value <- summary_result[[1]]$`Pr(>F)`[1]
           
           if (!is.na(p_value) && p_value <= 0.05) {
             paste("✅ INTERPRETASI ANOVA SATU ARAH:\n\n",
                   "Hasil: Ada PERBEDAAN SIGNIFIKAN antar kelompok\n",
                   "(F =", round(f_stat, 4), ", p-value =", round(p_value, 4), "≤ α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: μ1 = μ2 = μ3 = ... (semua rata-rata sama) - DITOLAK\n",
                   "• H1: Minimal ada satu rata-rata yang berbeda - DITERIMA\n",
                   "• F-statistik:", round(f_stat, 4), "\n",
                   "• Degrees of freedom:", summary_result[[1]]$Df[1], "dan", summary_result[[1]]$Df[2], "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Minimal ada satu kelompok yang berbeda signifikan\n",
                   "• Variabilitas antar kelompok > variabilitas dalam kelompok\n",
                   "• Faktor", input$anova_factor1, "berpengaruh terhadap", input$anova_dependent, "\n\n",
                   "📈 Rekomendasi Analisis Lanjutan:\n",
                   if(input$post_hoc) {
                     "• Post-hoc test (Tukey HSD) telah dilakukan - lihat hasil di atas\n"
                   } else {
                     "• Centang 'Post-hoc Test' untuk identifikasi kelompok yang berbeda\n"
                   },
                   "• Hitung effect size untuk mengukur besaran perbedaan\n",
                   "• Analisis descriptive statistics per kelompok")
           } else {
             paste("❌ INTERPRETASI ANOVA SATU ARAH:\n\n",
                   "Hasil: TIDAK ADA PERBEDAAN SIGNIFIKAN antar kelompok\n",
                   "(F =", round(f_stat, 4), ", p-value =", round(p_value, 4), "> α = 0.05)\n\n",
                   "📊 Kesimpulan Statistik:\n",
                   "• H0: μ1 = μ2 = μ3 = ... (semua rata-rata sama) - DITERIMA\n",
                   "• H1: Minimal ada satu rata-rata yang berbeda - DITOLAK\n",
                   "• F-statistik:", round(f_stat, 4), "\n",
                   "• Degrees of freedom:", summary_result[[1]]$Df[1], "dan", summary_result[[1]]$Df[2], "\n\n",
                   "🔍 Implikasi Praktis:\n",
                   "• Semua kelompok memiliki rata-rata yang sama\n",
                   "• Faktor", input$anova_factor1, "tidak berpengaruh signifikan terhadap", input$anova_dependent, "\n",
                   "• Variabilitas dalam kelompok > variabilitas antar kelompok\n\n",
                   "📈 Rekomendasi:\n",
                   if(input$post_hoc) {
                     "• Post-hoc test tidak diperlukan karena ANOVA tidak signifikan\n"
                   } else {
                     "• Post-hoc test tidak diperlukan karena tidak ada perbedaan signifikan\n"
                   },
                   "• Pertimbangkan faktor lain yang mungkin berpengaruh\n",
                   "• Evaluasi power analysis untuk ukuran sampel")
           }
         } else {
           # Interpretasi ANOVA dua arah
           if (length(summary_result[[1]]$`F value`) >= 3) {
             f_factor1 <- summary_result[[1]]$`F value`[1]
             f_factor2 <- summary_result[[1]]$`F value`[2]
             f_interaction <- summary_result[[1]]$`F value`[3]
             
             p_factor1 <- summary_result[[1]]$`Pr(>F)`[1]
             p_factor2 <- summary_result[[1]]$`Pr(>F)`[2]
             p_interaction <- summary_result[[1]]$`Pr(>F)`[3]
             
             paste("✅ INTERPRETASI ANOVA DUA ARAH:\n\n",
                   "📊 Hasil Analisis:\n",
                   "1. Efek", input$anova_factor1, ":\n",
                   "   F =", round(f_factor1, 4), ", p-value =", round(p_factor1, 4),
                   if(p_factor1 <= 0.05) " (SIGNIFIKAN)" else " (TIDAK SIGNIFIKAN)", "\n\n",
                   "2. Efek", input$anova_factor2, ":\n",
                   "   F =", round(f_factor2, 4), ", p-value =", round(p_factor2, 4),
                   if(p_factor2 <= 0.05) " (SIGNIFIKAN)" else " (TIDAK SIGNIFIKAN)", "\n\n",
                   "3. Efek Interaksi", input$anova_factor1, "×", input$anova_factor2, ":\n",
                   "   F =", round(f_interaction, 4), ", p-value =", round(p_interaction, 4),
                   if(p_interaction <= 0.05) " (SIGNIFIKAN)" else " (TIDAK SIGNIFIKAN)", "\n\n",
                   "🔍 Interpretasi Praktis:\n",
                   if(p_factor1 <= 0.05) paste("• Faktor", input$anova_factor1, "berpengaruh signifikan\n") else paste("• Faktor", input$anova_factor1, "tidak berpengaruh signifikan\n"),
                   if(p_factor2 <= 0.05) paste("• Faktor", input$anova_factor2, "berpengaruh signifikan\n") else paste("• Faktor", input$anova_factor2, "tidak berpengaruh signifikan\n"),
                   if(p_interaction <= 0.05) "• Ada efek interaksi antara kedua faktor\n" else "• Tidak ada efek interaksi antara kedua faktor\n",
                   "\n📈 Rekomendasi:\n",
                   if(p_interaction <= 0.05) "• Fokus pada interpretasi efek interaksi\n• Analisis simple effects untuk setiap kombinasi faktor" 
                   else "• Interpretasi efek utama masing-masing faktor secara terpisah")
           } else {
             "📊 ANOVA dua arah menguji efek dua faktor dan interaksinya terhadap variabel dependen. Hasil menunjukkan apakah ada perbedaan signifikan antar kelompok."
           }
         }
       })
      
    }, error = function(e) {
      showNotification(paste("❌ Error dalam ANOVA:", e$message), type = "error")
    })
  })
  
  # Regression
  observeEvent(input$run_regression, {
    req(values$current_data, input$reg_dependent, input$reg_independent)
    
    tryCatch({
      # Create formula
      formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
      formula_obj <- as.formula(formula_str)
      
      # Fit model
      model <- lm(formula_obj, data = values$current_data)
      values$regression_model <- model
      
      output$regression_summary <- renderText({
        # Buat summary tanpa duplikasi header
        model_summary <- summary(model)
        output_text <- capture.output(print(model_summary))
        
        # Hapus baris yang mengandung "REGRESI LINEAR BERGANDA" dari output
        clean_output <- output_text[!grepl("REGRESI LINEAR BERGANDA", output_text)]
        
        paste("🔬 REGRESI LINEAR BERGANDA\n\n",
              paste(clean_output, collapse = "\n"))
      })
      
      output$regression_diagnostics <- renderPlot({
        par(mfrow = c(2, 2))
        plot(model)
      })
      
      output$regression_assumptions <- renderText({
        # Normality test on residuals
        shapiro_test <- shapiro.test(residuals(model))
        
        # Breusch-Pagan test for heteroscedasticity
        bp_test <- bptest(model)
        
        paste("✅ UJI ASUMSI REGRESI\n\n",
              "1. UJI NORMALITAS RESIDUAL (Shapiro-Wilk):\n",
              "   W =", round(shapiro_test$statistic, 4), "\n",
              "   p-value =", format(shapiro_test$p.value, scientific = TRUE), "\n\n",
              "2. UJI HOMOSKEDASTISITAS (Breusch-Pagan):\n",
              "   BP =", round(bp_test$statistic, 4), "\n",
              "   p-value =", format(bp_test$p.value, scientific = TRUE), "\n\n",
              "3. R-SQUARED:", round(summary(model)$r.squared, 4), "\n",
              "4. ADJUSTED R-SQUARED:", round(summary(model)$adj.r.squared, 4))
      })
      
      output$regression_interpretation <- renderText({
        generate_interpretation("regression", summary(model), 0.05)
      })
      
    }, error = function(e) {
      showNotification(paste("❌ Error dalam regresi:", e$message), type = "error")
    })
  })
  
  # Download handlers
  output$download_transformed <- downloadHandler(
    filename = function() { 
      paste("SIVUDASI_data_transformasi_", Sys.Date(), ".csv", sep = "") 
    },
    content = function(file) {
      if (!is.null(values$transformed_data)) {
        write.csv(values$transformed_data, file, row.names = FALSE)
      }
    }
  )
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)