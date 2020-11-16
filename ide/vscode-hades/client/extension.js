"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
exports.__esModule = true;
exports.deactivate = exports.activate = void 0;
var vscode_1 = require("vscode");
var vscode_languageclient_1 = require("vscode-languageclient");
var fs_1 = require("fs");
var client;
var SERVER_PATH_CONFIG_KEY = 'hades.languageServerPath';
function activate(context) {
    return __awaiter(this, void 0, void 0, function () {
        var config, path, e_1, serverOptions, clientOptions;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    config = vscode_1.workspace.getConfiguration();
                    path = config.get(SERVER_PATH_CONFIG_KEY);
                    if (!(!config.has(SERVER_PATH_CONFIG_KEY) || path === '')) return [3 /*break*/, 2];
                    return [4 /*yield*/, vscode_1.window.showWarningMessage(SERVER_PATH_CONFIG_KEY + " configuration must be set")];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
                case 2:
                    if (!(typeof (path) !== 'string')) return [3 /*break*/, 4];
                    return [4 /*yield*/, vscode_1.window.showWarningMessage(SERVER_PATH_CONFIG_KEY + " must be a string path")];
                case 3:
                    _a.sent();
                    return [2 /*return*/];
                case 4:
                    _a.trys.push([4, 6, , 7]);
                    return [4 /*yield*/, fs_1.promises.stat(path)];
                case 5:
                    _a.sent();
                    return [3 /*break*/, 7];
                case 6:
                    e_1 = _a.sent();
                    vscode_1.window.showWarningMessage("Couldn't start Hades language server: " + path + " is not a file");
                    return [2 /*return*/];
                case 7:
                    serverOptions = {
                        command: path
                    };
                    clientOptions = {
                        // Register the server for plain text documents
                        documentSelector: [{ scheme: 'file', language: 'hades' }],
                        synchronize: {
                            // Notify the server about file changes to '.clientrc files contained in the workspace
                            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.hadesmodule')
                        }
                    };
                    // Create the language client and start the client.
                    client = new vscode_languageclient_1.LanguageClient('vscode-hades', 'Hades', serverOptions, clientOptions);
                    // Start the client. This will also launch the server
                    client.start();
                    return [2 /*return*/];
            }
        });
    });
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
