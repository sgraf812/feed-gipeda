# Changelog

## 0.3

- In order to enable easy SSH tunneling, the `distributed-process` backend
  migrated to `distributed-process-p2p`, using a simple star topology instead
- `distributed-process-p2p` requires us to pass the master node to slave nodes,
  so the command line interface changed (`--master` and `--slave` specifically)
- The acceptance test suite was fixed (nasty name resolution, IPv4 vs IPv6
  stuff), so this mostly works, although `stack` seems to have problems finding
  `hsc2hs` at the time of writing for `lts-2` and `lts-6`, so they are allowed
  to fail for the time being
