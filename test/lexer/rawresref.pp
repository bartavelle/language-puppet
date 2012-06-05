class test {
    file {
        "/etc/udp2log/${name}":
            require => Package[udplog];
    }
}
