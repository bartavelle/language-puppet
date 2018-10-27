node 'test' {
    ifupdown_route { 'other': vlans => ['8','11']; }
}
