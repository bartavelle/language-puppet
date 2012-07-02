node 'test' {
    $samplestring = 'DSQdqsdq653d'
    $sha1 = sha1($samplestring)
    $md5 = md5($samplestring)
    $regsubst = regsubst($samplestring, '(qsd)q', '\1')
    $mysql_password = mysql_password($samplestring)
    $split = split($samplestring, d)

    if($md5  != '81f03b05b831274fda4cfe485b19722c'        )             { fail("md5  $md5") }
    if($sha1 != '090477d0550af168b60979362cb1137a54798b0b')             { fail("sha1 $sha1") }
    if($regsubst != 'DSQdqsd653d')                                      { fail("regsubst $regsubst") }
    if($mysql_password != '*090477d0550af168b60979362cb1137a54798b0b')  { fail("mysql_password $mysql_password") }
    if($split != ['DSQ','qs','q653',''])                                { fail("split $split") }

}
