FROM rocker/tidyverse

RUN install2.r --error ggExtra \
                       cowplot \
                       ggrepel \
                       timetk

RUN rm -rf /tmp/downloaded_packages /tmp/*.rds
