"use strict";
exports.__esModule = true;
exports.deactivate = exports.activate = void 0;
var vscode_1 = require("vscode");
var vscode_languageclient_1 = require("vscode-languageclient");
var client;
function activate(context) {
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    var debugOpts = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005";
    var serverOptions = {
        command: "/home/dhruv/Projects/hades-lang/language-server/target/pack/bin/hades-language-server",
        options: {
            env: {
                // "JAVA_OPTS": debugOpts,
                "JAVA_HOME": "/home/dhruv/.jdks/adopt-openjdk-11.0.8"
            }
        }
    };
    // Options to control the language client
    var clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'hades' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // Create the language client and start the client.
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
