function indentTest(): any {
    const obj = {
        case: 1,
        default: 2
    };

    // This function was specifically added to test for a reversion in
    // the code that indents switch statements.
    function turnip(): void {
    }

    switch (process.platform) {
        case "moo":
            break;
        case "win32": {
            const basePath = process.env.LOCALAPPDATA ||
                process.env.APPDATA;
            return combinePaths(normalizeSlashes(basePath), "Microsoft/TypeScript");
        }
        case "darwin":
            const objCase = {
                case: 1,
                default: 2
            };
        case "linux":
        case "android": {
            const cacheLocation = getNonWindowsCacheLocation(process.platform === "darwin");
            return combinePaths(cacheLocation, "typescript");
        }
        default:
            const objDefault = {
                case: 1,
                default: 2
            };
            Debug.fail(`unsupported platform '${process.platform}'`);
            return;
    }
}
