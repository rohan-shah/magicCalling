FROM rohanshahcsiro/magiccalling-base:latest

RUN git clone https://github.com/rohan-shah/mcmc-jags.git jags \
 && cd jags \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release ..

RUN cd /jags/build \
 &&  make && make install

RUN git clone https://github.com/rohan-shah/rjags.git \
 && cd rjags \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -Djags_DIR=/jags/build -Dbugs_DIR=/jags/build -Dbase_DIR=/jags/build .. \
 && make && make install

RUN rm -rf /jags \
 && rm -rf /rjags

ENV BASHRC_FILE=/root/.bashrc
ENV LD_LIBRARY_PATH=/usr/local/lib/:/usr/local/lib/JAGS/modules-5/

RUN echo "" >> $BASHRC_FILE \
 && echo "export LD_LIBRARY_PATH=/usr/local/lib/:/usr/local/lib/JAGS/modules-5/" >> $BASHRC_FILE \
 && echo "" >> $BASHRC_FILE

# Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini

# Configure container startup
ENTRYPOINT ["tini", "--"]

# Overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]

