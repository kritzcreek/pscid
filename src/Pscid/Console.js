exports.clearConsole = function() {
    process.stdout.write('\033c');
};
