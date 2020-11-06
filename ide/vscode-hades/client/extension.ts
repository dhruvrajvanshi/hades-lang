import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used

  let debugOpts = "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005"
  let serverOptions: ServerOptions = {
      command: `/home/dhruv/Projects/hades-lang/language-server/target/pack/bin/hades-language-server`,
      options: {
        env: {
          "JAVA_OPTS": debugOpts,
          "JAVA_HOME": "/home/dhruv/.jdks/adopt-openjdk-11.0.8"
        }
      }
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'hades' }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'vscode-hades',
    'Hades',
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

