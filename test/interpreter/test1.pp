
$toplevelvar = "/lapin"

node 'test' {
    file {$toplevelvar: ensure => present;}
}
