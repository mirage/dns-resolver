#!/bin/bash

sudo ip link add name service type bridge
sudo ip addr add 10.0.0.1/24 dev service
sudo ip tuntap add name tap0 mode tap
sudo ip link set tap0 master service
sudo ip link set service up
sudo ip link set tap0 up
sudo iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -o wlan0 -j MASQUERADE
sudo iptables -A FORWARD -i service -o wlan0 -j ACCEPT
sudo iptables -A FORWARD -i wlan0 -o service -m state --state RELATED,ESTABLISHED -j ACCEPT
