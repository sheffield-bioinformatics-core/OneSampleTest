FROM crukcibioinformatics/shiny-server:latest

RUN R -e "install.packages(c('ggplot2', 'reshape2', 'gridExtra', 'pastecs', 'tidyr', 'RcmdrMisc'), repos = c('CRAN' = 'http://cran.ma.imperial.ac.uk'))"

RUN mkdir -p /srv/shiny-server/OneSampleTest/www

# modified shiny server configuration file
COPY shiny-server.conf /etc/shiny-server/

COPY www/*.jpg /srv/shiny-server/OneSampleTest/www/
COPY *.R /srv/shiny-server/OneSampleTest/

