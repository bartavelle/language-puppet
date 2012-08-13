node 'test' {
    validate_re('dsqdqs', '^[a-z]+$')
    validate_re('site123', '^\w+\d+$')
    if(!is_domain_name('test.domain.com'))
    {
        fail('This should be a valid domain name')
    }
    if(is_domain_name('1sdq.sdq.sdq'))
    {
        fail('This should be an invalid domain name')
    }
    if(split('a.B.C.D', '\.') != ['a','B','C','D'])
    {
        fail("bad split")
    }
}
