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
        const obj = {
            case: 1,
            default: 2
        };
    }

    // interfaces and classes indent.
    interface NodeChildProcess {
        send(message: any, sendHandle?: any): void;
        on(message: "message" | "exit", f: (m: any) => void): void;
        kill(): void;
        pid: number;
    }

    export type SomeType = string | number;

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

    // Generators as methods.
    class WithAGeneratorFirst {
        *blah() {
        }
    }

    class WithAGeneratorAfterAProperty {
        public foo: string = "1";

        *blah() {
        }
    }

    class WithAGeneratorAfterAnotherMethod {
        foo() {
        }

        *blah() {
        }
    }

    class WithSpaceAfterAsterisk  {
        bar() {
        }

        * oops() {
        }
    }


    class WithSpaceAfterParens  {
        bar() {
        }

        *oops () {
        }
    }

    class WithArguments  {
        bar() {
        }

        *oops(foo: number, bar: string) {
        }
    }

    // Some continued expressions
    {
        const a = 1 *
            2 /
            3 +
            4 -
            5 %
            6;

        const b = 1 >
            2;

        const c = 1 <
            2;

        const d = 1 &
            2 |
            3;

        const e = b ?
            2 :
            3;

        const e2 = b ?
            { a: 1 } :
            3;

        const f = window
            .document;

        const g = f
            instanceof Object;

        const h = "q"
            in [1, 2];

    }

    {
        // Object with fields that are keyword names.
        const a = {
            in: 1,
            IN: 1,
            instanceof: 1,
            instanceOf: 1,
        };

        // Objects with methods that are keyword names.  At the top of
        // the object declaration, and after a function declaration.
        class One {
            instanceOf(): void {
            }

            in(): void {}
        }

        // After a field declaration.
        class Two {
            foo: boolean = true;

            instanceOf(): void {
            }
        }
    }

    // Spread syntax
    {
        const a = { a: 1, b: 2 };
        const b = {
            ...a,
            a: 3,
        };
        const c = [1, 2];
        const d = [
            "a",
            ...c
        ];

        function foo(a: string,
                     b: number,
                     ...rest: any[]) {
        }
    }

    {
        // Regular expressions in lists.

        // List objects...
        const a = [
            /abc/,
            /def/
        ];

        const z =
            /abcd/;

        // Argument lists...
        function foo(a: RegExp, b: RegExp): void {
        }

        foo(
            /abc/,
            /def/);
    }
}

// Tests for return value annotations.

// Unannotated.
function moo(x: any,
             f: string) {
    return null;
}

// No parens around return type.
function moo2(x: any,
              f: string): (a: number) => void {
    return null;
}

// Parens around return type.
function foo(x: any,
             f: string): ((a: number) => void) {
    return null;
}

class Moo {}

// Type guard.
function foo2(x: any,
              f: string): x is Moo {
    return x.something === 1;
}

// Usage of generic in return type.
function foo3(a: number,
              b: number): Array<Array<(a: number) => void>> {
    return [];
}

// Curly brackets in return type.
function bar(a: number,
             b: number): { z(): string } {
    return {
        z() { return "a"; }
    };
}

// The sequence ): in the return type.
function bif(a: number,
             b: number): "abc):d" {
    return "abc):d";
}

// Generic and union in return type. This case was constructed from
// a specific bug in the indentation code.
function bif2(a: number,
              b: number): Array<number> | number {
    return 1;
}

// Array shorthand.
function bif3(a: number,
              b: number): number[] {
    return [1];
}

// Array shorthand in union.
function bif4(a: number,
              b: number): number[] | number {
    return [1];
}

// Array shorthand in union, with spaces.
function bif5(a: number,
              b: number): number[   ] | number {
    return [1];
}

// Comment where the return type would appear.
function gogo(a: number,
              b: number) /* foo */ {
}

// Function call in the list of arguments.
function foo5(x: any,
              f: string = bif(1, 2)): ((a: number) => void) {
    return null;
}

// Dotted name in return type.
function foo6(x: any,
              f: number): ts.server.SomeType {
    return "string";
}

// First parameter has function signature.
function foo7(x: (a: string, b: string) => Array<number>,
              b: number): void {
}

// Second parameter has function signature.
function foo7b(a: string,
               x: (a: number, b: number) => Array<number>): void {
}

function foo8(): void {
    // Arrow function in first parameter.
    foo7((a: string): Array<number> => {
        return [1];
    },
         1);

    // Arrow function in first parameter, line break in parameters.
    foo7((a: string,
          b: string): Array<number> => {
        return [1];
    },
         1);

    // Arrow function in second parameter.
    foo7b("1",
          (a: number, b: number): Array<number> => {
              return [1];
          });

    // Arrow function in second parameter, line break in parameters.
    foo7b("1",
          (a: number,
           b: number): Array<number> => {
              return [1];
          });
}

// Arrow function assignment, line break in parameters.
const foo9 = (a: string,
              b: string): Array<number> => {
    return [1];
}

// Arrow function assignment.
const foo10 = (a: string, b: string): Array<number> => {
    return [1];
}

// Arrow function assignment, parenthesized.
const foo11 = ((a: string, b: string): Array<number> => {
    return [1];
});

function foo12(): void {
    // Function in first parameter.
    foo7(function (a: string): Array<number> {
        return [1];
    },
         1);

    // Function in first parameter, line break in parameters.
    foo7(function (a: string,
                   b: string): Array<number> {
        return [1];
    },
         1);

    // Arrow function in second parameter.
    foo7b("1",
          function (a: number, b: number): Array<number> {
              return [1];
          });

    // Arrow function in second parameter, line break in parameters.
    foo7b("1",
          function (a: number,
                    b: number): Array<number> {
              return [1];
          });

    // Same cases as above but named.

    // Function in first parameter.
    foo7(function _mip(a: string): Array<number> {
        return [1];
    },
         1);

    // Function in first parameter, line break in parameters.
    foo7(function _mip(a: string,
                       b: string): Array<number> {
        return [1];
    },
         1);

    // Arrow function in second parameter.
    foo7b("1",
          function _mip(a: number, b: number): Array<number> {
              return [1];
          });

    // Arrow function in second parameter, line break in parameters.
    foo7b("1",
          function _mip(a: number,
                        b: number): Array<number> {
              return [1];
          });

    // Same cases as above but generators.

    // Function in first parameter.
    foo7(function *(a: string): Array<number> {
        return [1];
    },
         1);

    // Function in first parameter, line break in parameters.
    foo7(function *(a: string,
                    b: string): Array<number> {
        return [1];
    },
         1);

    // Arrow function in second parameter.
    foo7b("1",
          function *(a: number, b: number): Array<number> {
              return [1];
          });

    // Arrow function in second parameter, line break in parameters.
    foo7b("1",
          function *(a: number,
                     b: number): Array<number> {
              return [1];
          });

    // Check that JavaScript objects are still handled right. Whether
    // in the 1st or subsequent position of a call.
    function smurf(a: {}, b: {}) {}
    smurf({
        a: {},
    },
          {
        a: {},
    });

    smurf({
        a: {},
    }, {
        a: {},
    });
}

// Number literals in the return type annotation.
function foo13(something: string,
               somethingElse: string): 0b1 | 0 | -1 | 0o2 | 0x3f {
    return 0;
}

// No spaces between numbers and type union symbols. (Also changed the
// notation to uppercase where possible.)
function foo14(something: string,
               somethingElse: string): 0B1|0|-1|0O2|0X3F {
    return 0;
}

const a =
    1; // Blah
const b = 2;

function blipblop(): void {
    {
        const q = 1;
    }
}
