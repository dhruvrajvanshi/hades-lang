import { fileURLToPath } from 'url';
import { workspace, ExtensionContext, window } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient';
import { promises as fs } from 'fs';

let client: LanguageClient;
const SERVER_PATH_CONFIG_KEY = 'hades.languageServerPath';

export async function activate(context: ExtensionContext) {


  const config = workspace.getConfiguration()
  const path = config.get(SERVER_PATH_CONFIG_KEY);
  if (!config.has(SERVER_PATH_CONFIG_KEY) || path === '') {
    await window.showWarningMessage(`${SERVER_PATH_CONFIG_KEY} configuration must be set`);
    return;
  }

  if (typeof(path) !== 'string') {
    await window.showWarningMessage(`${SERVER_PATH_CONFIG_KEY} must be a string path`);
    return;
  }

  try {
    await fs.stat(path);
  } catch (e: unknown) {
    window.showWarningMessage(`Couldn't start Hades language server: ${path} is not a file`);
    return;
  }

  const serverOptions: ServerOptions = {
    command: path,
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'hades' }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher('**/.hadesmodule')
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

