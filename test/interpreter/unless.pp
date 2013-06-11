$a = 12

node 'test' {
    unless ($a == 12) {
        fail "unless"
    }
}
