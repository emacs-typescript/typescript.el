function getGlobalTypingsCacheLocation() {
    const obj = {
        case: 1,
        default: 2
    };
    switch (process.platform) {
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
