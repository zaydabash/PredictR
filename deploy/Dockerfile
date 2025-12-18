FROM rocker/shiny-verse:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install R packages with explicit versions where possible or just robustly
RUN R -e "install.packages(c('RSQLite', 'caret', 'pROC', 'DT', 'e1071', 'tidymodels', 'ranger', 'plotly', 'testthat', 'bslib', 'rmarkdown', 'glmnet', 'rprojroot', 'lubridate', 'here'), repos='https://cloud.r-project.org/')"

# Set up app directory
WORKDIR /app
COPY . /app

# Ensure directory is writable by the shiny user
RUN chown -R shiny:shiny /app

# Switch to non-root user
USER shiny

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/app/app', host = '0.0.0.0', port = 3838)"]
