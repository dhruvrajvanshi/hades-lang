"use strict";
exports.__esModule = true;
exports.deactivate = exports.activate = void 0;
var path = require("path");
var vscode_1 = require("vscode");
var vscode_languageclient_1 = require("vscode-languageclient");
var client;
function activate(context) {
    // The server is implemented in node
    var serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
    // The debug options for the server
    // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
    var debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    var serverOptions = {
        command: "hades",
        args: ["lsp"]
    };
    var clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'hades' }],
        synchronize: {
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/*.hds')
        }
    };
    client = new vscode_languageclient_1.LanguageClient('vscode-hades', 'Hades', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
