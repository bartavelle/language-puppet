$a_number = Integer("0xFF", 16)  # results in 255
$a_number = Numeric("010")       # results in 8
$a_number = Numeric("010", 10)   # results in 10
$a_number = Integer(true)        # results in 1
$a_number = Numeric("0x10", 10)  # this is an error. Prefix and radix does not match.

