export let clearConsole = function () {
  process.stdout.write('\x1Bc');
};
