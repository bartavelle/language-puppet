
node 'www1.example.com', 'www2.example.com', 'www3.example.com' {
    include common
    include apache, squid
}


node /^www\d+$/ {
    include common
}

node 'www1.example.com' inherits 'common' {
    include ntp
    include apache
    include squid
}
