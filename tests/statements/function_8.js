// Mandatory. Return here a description of the test case.
function test_description() {
    return "more lambdas";
}

// Mandatory. Return here an array of arguments the testsuite will use
// to invoke the test() function. For no arguments return an empty array.
function test_args() {
    return [];
}

// Mandatory. Return here the expected test result.
function test_ok() {
    return 64;
}

// Optional. Provide here any global code.


// Mandatory. The actual test.
// Testsuite invokes this function with the arguments from test_args()
// and compares the return value with the expected result from test_ok().
function test() {
  var square = function(n) {
    return n * n;
  };
  var cube = function(n) {
    return n * n * n;
  };
  function f_of_g(f, g) {
    return function(args) { return f(g(args)) };
  }
  return f_of_g(square, cube)(2);
}
