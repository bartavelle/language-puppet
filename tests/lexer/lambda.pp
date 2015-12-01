$binaries = ["facter", "hiera", "mco", "puppet", "puppetserver"]

$binaries.each |$binary| {
  file {"/usr/bin/$binary":
    ensure => link,
    target => "/opt/puppetlabs/bin/$binary",
  }
}
