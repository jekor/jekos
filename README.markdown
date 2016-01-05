# The JekOS Operating System

Currently JekOS isn't as much an operating system as it is a way to bootstrap an install of NixOS to AWS in a browser. This bootstrapped "kernel" instance is planned to then takeover provisioning and monitoring of further servers/resources.

The installer is available at [jekos.org](https://jekos.org/) but the AMI it references is not generally available (although the code has the necessary Nix expression for building your own). It probably won't be released until I can get jekos.net whitelisted by [Let's Encrypt](https://letsencrypt.org/) or I would quickly exhaust the 5-certificates-per-top-level-domain-per-7-days limit.
