define create_pkcs12( $certname="$name", $cert_alias="", $password="", $user="root", $group="ssl-cert", $location="/etc/ssl/private" ) {

    if ( $cert_alias == "" ) {
        $certalias = $certname
    } else {
        $certalias = $cert_alias
    }

    if ( $password == "" ) {
        $defaultpassword = $passwords::certs::certs_default_pass
    } else {
        $defaultpassword = $password
    }

    exec {
        # pkcs12 file, used by things like opendj, nss, and tomcat
        "${name}_create_pkcs12":
            creates => "${location}/${certname}.p12",
            command => "/usr/bin/openssl pkcs12 -export -name \"${certalias}\" -passout pass:${defaultpassword} -in /etc/ssl/certs/${certname}.pem -inkey /etc/ssl/private/${certname}.key -out ${location}/${certname}.p12",
            require => [Package["openssl"], File["/etc/ssl/private/${certname}.key", "/etc/ssl/certs/${certname}.pem"]];
    }

    file {
        # Fix permissions on the p12 file, and make it available as
        # a puppet resource
        "${location}/${certname}.p12":
            mode => 0440,
            owner => $user,
            group => $group,
            require => Exec["${name}_create_pkcs12"],
            ensure => file;
    }
}
