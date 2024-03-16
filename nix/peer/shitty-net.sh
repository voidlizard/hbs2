NIC=ve-hbs2-test

sudo tc qdisc del dev $NIC root
sudo tc qdisc add dev $NIC root netem delay 200ms 40ms loss 1%
sudo tc -s qdisc ls dev $NIC

