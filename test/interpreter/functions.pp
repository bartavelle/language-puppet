
node 'test' {
    /*
    $incr = incrementer(4)
    if($r != 5.0)
    {
        fail("Returned $r instead of 5")
    }
    $hash2list = hashtolist( { 'a' => 'b', 'c' => 5 } )
    */
    $luamap = luamap( [7,1,2,3,4,5] )
    fail($luamap)
}
