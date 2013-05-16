
node 'test' {
    if( fqdn_rand(60000) != 35380 )
    {
        fail("test 0")
    }
    if( fqdn_rand(60,1) != 40 )
    {
        fail("test 1")
    }
    if( fqdn_rand(60,2) != 47 )
    {
        fail("test 2")
    }
    if( fqdn_rand(60,3) != 51 )
    {
        fail("test 3")
    }
    if( fqdn_rand(60,4) != 18 )
    {
        fail("test 4")
    }
}
