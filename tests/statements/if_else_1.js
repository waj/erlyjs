// Mandatory. Return here a description of the test case.
function test_description() {
    return "If statement";
}

// Mandatory. Return here an array of arguments the testsuite will use 
// to invoke the test() function. For no arguments return an empty array.
function test_args() {
    return [];
}

// Mandatory. Return here the expected test result. 
function test_ok() {
    return 3;
}

// Optional. Provide here any global code.


// Mandatory. The actual test. 
// Testsuite invokes this function with the arguments from test_args()
// and compares the return value with the expected result from test_ok().
function test() {
    var a=2, b=3, c=4;
    if (true)  
        a = b;
    else
        a = c;
    return a;
}
