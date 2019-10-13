function foo(bar, baz, faz) {
    //
}

function foo2(
    bar, baz, faz
) {
    //
}

function bar(bar: string, baz: Bar, faz) {
    //
}

function barz(
    bar: string, baz: Bar, faz) {
    //
}

function baz<T>(bar: string, baz: Bar, faz: T) {
    //
}

class Foo<T> {
    bar: Map<string, Array<T>>;

    constructor() {
        this.foo();
        this.bar = 'test';
    }

    foo(foo): Promise<T> {
        console.log('test me');
    }

    baz(
        foo, bar, baz
    ) {
        //
    }

    baz2(foo, bar, baz) {
        this.baz(foo, bar, baz);
    }
}

interface Bar {
    bar: Map<string, Array<T>>;

    foo();
    bar(): Promise<F>;
    baz(foo: Foo, bar, baz): Promise<F>;
}
