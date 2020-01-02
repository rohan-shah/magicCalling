FROM rohanshahcsiro/magiccalling-jags:latest

RUN export LD_LIBRARY_PATH=/usr/local/lib/:/usr/local/lib/JAGS/modules-5/ \
 && git clone https://github.com/rohan-shah/magicCalling.git \
 && cd magicCalling \
 && R CMD INSTALL . \
 && rm -rf /magicCalling

# Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini

# Configure container startup
ENTRYPOINT ["tini", "--"]

# Overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]
