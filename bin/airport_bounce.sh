#!/bin/bash -e
networksetup -setairportpower en0 off
sleep 1
dscacheutil -flushcache
networksetup -setairportpower en0 on
