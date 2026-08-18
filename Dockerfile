FROM rocker/tidyverse:latest

WORKDIR /code

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
DT \
shiny \
plotly \
tidyverse \
httr

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', 7860)))"]
