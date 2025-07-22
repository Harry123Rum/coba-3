# SIVANA Dashboard Startup Script
# This script will install required packages and run the dashboard

cat("========================================\n")
cat("SIVANA Dashboard Startup\n")
cat("Social Index Vulnerability Analysis Navigator Application\n")
cat("========================================\n\n")

# Create user library if it doesn't exist
user_lib <- file.path(Sys.getenv('HOME'), 'R', 'library')
if (!dir.exists(user_lib)) {
  cat("Creating user library directory...\n")
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# Set library paths
.libPaths(c(user_lib, .libPaths()))

# Define required packages
required_packages <- c(
  "shiny", "shinydashboard", "DT", "ggplot2", "plotly", 
  "leaflet", "sf", "dplyr", "corrplot", "psych", "car", 
  "nortest", "flextable", "officer", "shinyWidgets", 
  "shinycssloaders", "htmlwidgets", "RColorBrewer", 
  "viridis", "reshape2", "broom", "gridExtra"
)

# Function to install missing packages
install_missing_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())
  missing_packages <- packages[!(packages %in% installed_packages)]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("This may take a few minutes...\n\n")
    
    for (pkg in missing_packages) {
      cat("Installing", pkg, "...\n")
      tryCatch({
        install.packages(pkg, repos = "https://cran.rstudio.com/", 
                        lib = user_lib, dependencies = TRUE, quiet = TRUE)
        cat("✓", pkg, "installed successfully\n")
      }, error = function(e) {
        cat("✗ Failed to install", pkg, ":", e$message, "\n")
      })
    }
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Check which packages are available
cat("\nChecking package availability...\n")
available_packages <- required_packages[required_packages %in% rownames(installed.packages())]
missing_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]

cat("Available packages:", length(available_packages), "of", length(required_packages), "\n")
if (length(missing_packages) > 0) {
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
}

# Check if data files exist
data_files <- c("sovi_data.csv", "sovi_administrasi_kabupaten.shp")
missing_data <- data_files[!file.exists(data_files)]

if (length(missing_data) > 0) {
  cat("\n⚠️  WARNING: Missing data files:\n")
  for (file in missing_data) {
    cat("   -", file, "\n")
  }
  cat("\nPlease ensure all data files are in the current directory.\n")
} else {
  cat("\n✓ All required data files found.\n")
}

# Try to run the dashboard
cat("\nStarting SIVANA Dashboard...\n")
cat("========================================\n")

if (file.exists("SIVANA_Dashboard.R")) {
  tryCatch({
    source("SIVANA_Dashboard.R")
  }, error = function(e) {
    cat("Error starting dashboard:", e$message, "\n")
    cat("\nTrying alternative approach...\n")
    
    # Try with minimal packages
    if ("shiny" %in% available_packages) {
      library(shiny)
      if (file.exists("SIVANA_Dashboard.R")) {
        runApp("SIVANA_Dashboard.R")
      }
    } else {
      cat("Cannot start dashboard: Shiny package not available.\n")
      cat("Please install required packages manually:\n")
      cat("install.packages(c('shiny', 'shinydashboard', 'DT', 'ggplot2'))\n")
    }
  })
} else {
  cat("Error: SIVANA_Dashboard.R not found in current directory.\n")
}