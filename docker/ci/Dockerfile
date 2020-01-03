FROM rohanshahcsiro/magiccalling-jags:latest

RUN R -e "install.packages(c('testthat', 'qtl', 'fastcluster', 'pryr', 'nnls', 'jsonlite'))"

RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)){install.packages('BiocManager')}; BiocManager::install('Heatplus')"

RUN git clone https://github.com/rohan-shah/mpMap2.git \
 && cd mpMap2 \
 && R CMD INSTALL . \
 && rm -rf /mpMap2

# Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini

# Configure container startup
ENTRYPOINT ["tini", "--"]

# Overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]
