FROM rstudio/plumber

# Install required R packages
RUN R -e "install.packages('readxl')"

# Copy your files into the container
COPY . /app
WORKDIR /app

# Run your API
CMD ["Rscript", "app.R"]