# Use the rocker/shiny base image
FROM rocker/shiny:4.3.1

# Install required R packages (replace with your actual package dependencies)
RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr'), repos='http://cran.rstudio.com/')"

# Copy the app to the container
COPY . /srv/shiny-server/

# Expose port 3838 for Shiny
EXPOSE 3838

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
