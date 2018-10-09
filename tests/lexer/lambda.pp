$binaries = ["facter", "hiera", "mco", "puppet", "puppetserver"]

$binaries.each | $binary | {
  file {"/usr/bin/$binary":
    ensure => link,
    target => "/opt/puppetlabs/bin/$binary",
  }
}

each($binaries) |$binary| {
  file {"/usr/bin/$binary":
    ensure => link,
    target => "/opt/puppetlabs/bin/$binary",
  }
}

$entries = {}
$entries.each | String $e_name, Hash $e_params | {
  limits::limits { $e_name:
    * => $e_params,
  }
}
