
node 'test' {
    $num1 = 50
    $num2 = 12
    $asc1 = "qsdqsd"
    $asc2 = 'asdqdd'

    if($num1 < $num2)
    {
        fail('failed')
    }
    if($num1 == $num2)
    {
        fail('failed')
    }
    if($asc1 < $asc2)
    {
        fail('failed')
    }
    if($asc1 == $asc2)
    {
        fail('failed')
    }
}
