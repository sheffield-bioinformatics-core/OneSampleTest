FROM crukcibioinformatics/shiny-base

RUN R -e 'install.packages("RcmdrMisc", repos = "https://cloud.r-project.org")'

RUN mkdir -p /srv/shiny-server/OneSampleTest/www

COPY www/*.jpg /srv/shiny-server/OneSampleTest/www/
COPY *.R /srv/shiny-server/OneSampleTest/

