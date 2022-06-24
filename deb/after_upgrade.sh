# if upgrading from old version with different file location, move blockchain_node date files to the new location
if [ -e /var/helium/blockchain_node/data/blockchain_node/swarm_key ] && [ ! -e /opt/blockchain_node/data/blockchain_node/swarm_key ]; then
    echo "Found existing swarm_key, moving data to /opt/blockchain_node/"
    mv /var/helium/blockchain_node/data /opt/blockchain_node/data
    chown -R helium:helium /opt/blockchain_node/data
elif [ -e /var/data/blockchain_node/blockchain_node/swarm_key ] && [ ! -e /opt/blockchain_node/data/blockchain_node/swarm_key ]; then
    echo "Found existing swarm_key, moving data to /opt/blockchain_node/"
    mv /var/data/blockchain_node /opt/blockchain_node/data
    chown -R helium:helium /opt/blockchain_node/data
fi

# add blockchain_node to /usr/local/bin so it appears in path, if it does not already exist
ln -s /opt/blockchain_node/bin/blockchain_node /usr/local/bin/blockchain_node || true
