const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

function activate(context) {
  const configuration = vscode.workspace.getConfiguration('mylang');
  const command = configuration.get('languageServerPath') || 'mylang-lsp';

  const serverOptions = {
    command,
    args: ['--stdio'],
  };

  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'mlang' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.mlang'),
    },
  };

  client = new LanguageClient(
    'mylang',
    'mylang Language Server',
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client.start());
}

function deactivate() {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

module.exports = {
  activate,
  deactivate,
};
