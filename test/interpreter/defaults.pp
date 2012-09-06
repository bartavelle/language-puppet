
node 'test' {
    Mount { fstype => 'nfs' }
    mount { '/tmp':
        ensure => mounted,
        device => '/dev/test';
    }
}
