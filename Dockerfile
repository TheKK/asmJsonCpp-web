FROM nixos/nix:2.3.6

RUN echo "binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org" >> /etc/nix/nix.conf
RUN echo "binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" >> /etc/nix/nix.conf
RUN echo "binary-caches-parallel-connections = 40" >> /etc/nix/nix.conf

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command

RUN cd /tmp/ && mkdir foo && cd foo && ob init && nix-build -A exe
