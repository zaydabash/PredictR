FROM rocker/shiny-verse:latest

# Install system dependencies (if any needed for R packages)
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('RSQLite', 'caret', 'pROC', 'DT', 'e1071', 'tidymodels', 'ranger', 'plotly', 'testthat', 'bslib', 'rmarkdown', 'glmnet'), repos='https://cloud.r-project.org/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/app
RUN mkdir -p /srv/shiny-server/R
RUN mkdir -p /srv/shiny-server/data
RUN mkdir -p /srv/shiny-server/sql

# Copy code
COPY app/ /srv/shiny-server/app/
COPY R/ /srv/shiny-server/R/
COPY sql/ /srv/shiny-server/sql/
COPY data/ /srv/shiny-server/data/

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port = 3838)"]
