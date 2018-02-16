function indentTest(): any {
    switch (process.platform) {
    case "win32": {
        const basePath = process.env.LOCALAPPDATA ||
            process.env.APPDATA;
        return combinePaths(normalizeSlashes(basePath), "Microsoft/TypeScript");
    }
    case "darwin":
    case "linux":
    case "android": {
        const cacheLocation = getNonWindowsCacheLocation(process.platform === "darwin");
        return combinePaths(cacheLocation, "typescript");
    }
    default:
        Debug.fail(`unsupported platform '${process.platform}'`);
        return;
    }
}
