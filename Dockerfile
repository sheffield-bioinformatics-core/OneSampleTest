FROM crukcibioinformatics/shiny-base:0.1

RUN R -e 'install.packages("RcmdrMisc", repos = c("CRAN" = "http://cran.ma.imperial.ac.uk"))'

RUN mkdir -p /srv/shiny-server/OneSampleTest/www

# modified shiny server configuration file
COPY shiny-server.conf /etc/shiny-server/

COPY www/*.jpg /srv/shiny-server/OneSampleTest/www/
COPY *.R /srv/shiny-server/OneSampleTest/

