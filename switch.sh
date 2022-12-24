#!/usr/bin/env bash

nix build .#homeManagerConfigurations.kaptch@laptop.activationPackage
result/activate
