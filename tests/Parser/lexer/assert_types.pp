if $storage_driver {
    if $::osfamily == 'windows' {
      assert_type($storage_driver) |$a, $b| {
          fail(translate(('Valid values for storage_driver on windows are windowsfilter')))
      }
    } else {
      assert_type($storage_driver) |$a, $b| {
        fail(translate(('Valid values for storage_driver are aufs, devicemapper, btrfs, overlay, overlay2, vfs, zfs.')))
      }
    }
  }
