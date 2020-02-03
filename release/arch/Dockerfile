FROM archlinux/base

RUN pacman -Suy --noconfirm
RUN pacman -S --noconfirm curl base-devel
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl https://raw.githubusercontent.com/haskell/ghcup/master/bootstrap-haskell -sSf | sh
ENV PATH="/root/.cabal/bin/:/root/.ghcup/bin:${PATH}"
RUN cabal new-install cabal-plan
RUN mkdir /build/

WORKDIR /build/
CMD cp -r /home/hledger-iadd/* /build/ && cabal new-update && cabal new-build && \
	cp `cabal-plan list-bin hledger-iadd` /home/hledger-iadd/ && \
	strip /home/hledger-iadd/hledger-iadd
