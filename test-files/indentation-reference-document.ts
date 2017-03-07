/// <reference types="node" />
/// <reference path="shared.ts" />
/// <reference path="session.ts" />
// used in fs.writeSync
/* tslint:disable:no-null-keyword */

/*
 * this file is a butchered copy of TypeScript's tsserver.ts file
 * made to contain the most important syntactical elements
 * of TypeScript to verify indentation-code.
 *
 * It will not build, and that's NOT a problem!
 */

// namespaces indent.
namespace ts.server {

    const net: {
        connect(options: { port: number }, onConnect?: () => void): NodeSocket
    } = require("net");

    // functions indent.
    function getGlobalTypingsCacheLocation() {
        // We know switch/case is indented incorrectly.
        // TODO: FIX!

        // switch (process.platform) {
        //     case "win32": {
        //         const basePath = process.env.LOCALAPPDATA ||
        //             process.env.APPDATA;
        //         return combinePaths(normalizeSlashes(basePath), "Microsoft/TypeScript");
        //     }
        //     case "darwin":
        //     case "linux":
        //     case "android": {
        //         const cacheLocation = getNonWindowsCacheLocation(process.platform === "darwin");
        //         return combinePaths(cacheLocation, "typescript");
        //     }
        //     default:
        //         Debug.fail(`unsupported platform '${process.platform}'`);
        //         return;
        // }
    }

    // interfaces and classes indent.
    interface NodeChildProcess {
        send(message: any, sendHandle?: any): void;
        on(message: "message" | "exit", f: (m: any) => void): void;
        kill(): void;
        pid: number;
    }

    class Logger implements ts.server.Logger {
        private firstInGroup = true;

        // parameter-lists are currently not indented like tsserver wants it to...
        // constructor(private readonly logFilename: string,
        //     private readonly traceToConsole: boolean,
        //     private readonly level: LogLevel) {
        // }

        // function-typed class-members indent.
        constructor(private readonly logFilename: string) {
            console.log("yes");
        }

        static padStringRight(str: string, padding: string) {
            return (str + padding).slice(0, padding.length);
        }

        close() {
            if (this.fd >= 0) {
                fs.close(this.fd);
            }
        }
    }

    // object initialization/parameter-lists indent.
    const ioSession = new IOSession(
        sys,
        cancellationToken,
        eventPort,
        /*canUseEvents*/ eventPort === undefined,
        useSingleInferredProject,
        disableAutomaticTypingAcquisition,
        getGlobalTypingsCacheLocation(),
        telemetryEnabled,
        logger);
    process.on("uncaughtException", function (err: Error) {
        ioSession.logError(err, "unknown");
    });
}
