exports.onKeypressImpl = function (input, output, terminal, f) {
  const readline = require("readline");
  const rl = readline.createInterface({
    input: input,
    output: output,
    prompt: "",
    terminal: terminal
  });
  return function() {
    process.stdin.on('keypress', (c, k) => {
      f(k)()
    });
  }
}

exports.onResizeImpl = function (f) {
  return function () {
    process.stdout.on('resize', function() {
      f(process.stdout.columns)(process.stdout.rows)()
    })
  }
}

exports.getColumns = process.stdout.columns
exports.getRows = process.stdout.rows


