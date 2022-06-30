# add system user for file ownership and systemd user, if not exists
useradd --system --home-dir /opt/miner --create-home helium || true
