class bind::master ($testcheck = '', $domains = { "${::domain}" => {} } , $slaves = [], $query_nets = ['10.2.1.0/24'], $forwarders = '')
{
    validate_hash($domains)
    validate_array($slaves)
    validate_array($query_nets)

    include bind
    include bind::statichosts

    create_resources('bind::zonefile', $bind::master::domains)
    bind::zonefile { 'zone.rev': ; }

    file {
        '/etc/bind/named.conf.local':
            content => template("bind/master.named.conf.local.erb"),
            owner   => 'root',
            group   => 'root',
            mode    => '644',
            require => Package['bind9'],
            notify  => Service['bind9'];
        '/etc/bind/named.conf.options':
            content => template("bind/named.conf.options.erb"),
            owner   => 'root',
            group   => 'root',
            mode    => '644',
            require => Package['bind9'],
            notify  => Service['bind9'];
    }

    Zone_record<<|  |>>
}

