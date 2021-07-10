FROM archlinux

RUN pacman -Suy --noconfirm
RUN pacman -S --noconfirm curl base-devel
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.cabal/bin/:/root/.ghcup/bin:${PATH}"

RUN mkdir /build/

WORKDIR /build/
CMD cp -r /home/hledger-iadd/* /build/ && cabal new-update && cabal new-build && \
	cp `cabal list-bin exe:ghledger-iadd` /home/hledger-iadd/ && \
	strip /home/hledger-iadd/hledger-iadd
