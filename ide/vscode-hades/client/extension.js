"use strict";
exports.__esModule = true;
exports.deactivate = exports.activate = void 0;
var vscode_1 = require("vscode");
var vscode_languageclient_1 = require("vscode-languageclient");
var client;
function activate(context) {
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
