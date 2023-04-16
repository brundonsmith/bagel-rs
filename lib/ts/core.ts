function ___log<T>(x: T): T {
    console.log(x);
    return x;
}

const ___LOADING = Symbol('LOADING')

type ___Plan<T> = () => Promise<T>

function ___planned<T>(plan: ___Plan<T>): T | typeof ___LOADING {
    return ___observe(___getRemote(plan), 'current')
}

const ___getRemote = ___memo(<T,>(plan: ___Plan<T>) => new ___Remote(plan))

class ___Remote<T> {
    constructor(
        plan: ___Plan<T>
    ) {
        ___autorun(async () => {
            const thisRequestId = this.latestRequestId = String(Math.random())

            ___invalidate(this, 'current', ___LOADING);

            await plan()
                .then(res => {
                    if (thisRequestId === this.latestRequestId) {
                        ___invalidate(this, 'current', res);
                    }
                })
        })
    }

    private latestRequestId: string | undefined;
    public current: T | typeof ___LOADING = ___LOADING;
}


// Errors
type ___Error<T> = { kind: typeof ___ERROR_SYM, value: T }
const ___ERROR_SYM = Symbol('ERROR_SYM')

// Iterables
function* ___range(start: number, end: number): Generator<number> {
    for (let i = start; i < end; i++) {
        yield i;
    }
}

function* ___repeat<T>(val: T, count: number): Generator<T> {
    for (let i = 0; i < count; i++) {
        yield val
    }
}

function* ___entries<V>(obj: { [key: string]: V }): Generator<[string, V]> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield [key, obj[key]];
    }
}

function* ___keys<V>(obj: { [key: string]: V }): Generator<string> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield key
    }
}

function* ___values<V>(obj: { [key: string]: V }): Generator<V> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield obj[key]
    }
}

function ___observeIterable<T>(iter: Iterable<T>) {
    if (iter[Symbol.iterator]) {
        ___observe(iter, ___WHOLE_OBJECT)
    }
}

function* ___slice<T>(iter: Iterable<T>, start: number | undefined, end: number | undefined): Generator<T> {
    ___observeIterable(iter)

    let index = 0;
    for (const el of iter) {
        if ((start == null || index >= start) && (end == null || index < end)) {
            yield el;
        }
        index++;
    }
}

function* ___takeWhile<T>(iter: Iterable<T>, fn: (el: T) => boolean): Generator<T> {
    ___observeIterable(iter)

    for (const el of iter) {
        if (fn(el)) {
            yield el;
        } else {
            return
        }
    }
}

function ___sorted<T>(iter: Iterable<T>, fn: (a: T, b: T) => number): Iterable<T> {
    ___observeIterable(iter)

    return Array.from(iter).sort(fn)
}

function ___every<T>(iter: Iterable<T>, fn: (a: T) => boolean): boolean {
    ___observeIterable(iter)

    for (const el of iter) {
        if (!fn(el)) {
            return false
        }
    }

    return true
}

function ___some<T>(iter: Iterable<T>, fn: (a: T) => boolean): boolean {
    ___observeIterable(iter)

    for (const el of iter) {
        if (!fn(el)) {
            return true
        }
    }

    return false
}

function ___count<T>(iter: Iterable<T>): number {
    ___observeIterable(iter)

    let count = 0

    for (const _ of iter) {
        count++
    }

    return count
}

function ___first<T>(iter: Iterable<T>): T | null | undefined {
    ___observeIterable(iter)

    for (const el of iter) {
        return el
    }
}

function* ___map<T, R>(iter: Iterable<T>, fn: (el: T) => R): Generator<R> {
    ___observeIterable(iter)

    for (const el of iter) {
        yield fn(el);
    }
}

function ___reduce<T, R>(iter: Iterable<T>, init: R, fn: (acc: R, el: T) => R): R {
    ___observeIterable(iter)

    let acc = init
    for (const el of iter) {
        acc = fn(acc, el)
    }
    return acc
}

function* ___filter<T>(iter: Iterable<T>, fn: (el: T) => boolean): Generator<T> {
    ___observeIterable(iter)

    for (const el of iter) {
        if (fn(el)) {
            yield el;
        }
    }
}

function* ___concat<T>(iter1: Iterable<T>, iter2: Iterable<T>): Generator<T> {
    ___observeIterable(iter1)
    ___observeIterable(iter2)

    for (const el of iter1) {
        yield el;
    }

    for (const el of iter2) {
        yield el;
    }
}

function* ___zip<T, R>(iter1: Iterable<T>, iter2: Iterable<R>): Generator<[T | null | undefined, R | null | undefined]> {
    ___observeIterable(iter1)
    ___observeIterable(iter2)

    const a = iter1[Symbol.iterator]();
    const b = iter2[Symbol.iterator]();

    let nextA = a.next();
    let nextB = b.next();

    while (!nextA.done || !nextB.done) {
        yield [nextA.value, nextB.value];

        nextA = a.next();
        nextB = b.next();
    }
}

function* ___indexed<T>(iter: Iterable<T>): Generator<[T, number]> {
    ___observeIterable(iter)

    let index = 0;
    for (const el of iter) {
        yield [el, index];
        index++
    }
}

function ___collectArray<T>(iter: Iterable<T>): T[] {
    ___observeIterable(iter)

    return Array.from(iter)
}

function ___collectObject<T>(iter: Iterable<T>): { [key: string]: unknown } {
    ___observeIterable(iter)

    const obj = {} as { [key: string]: unknown }

    for (const entry of iter) {
        if (Array.isArray(entry)) {
            const [key, value] = entry

            obj[key] = value
        }
    }

    return obj
}

function ___join(iter: Iterable<string>, delimiter: string | null | undefined): string {
    ___observeIterable(iter)

    let str = "";
    let first = true;

    for (const el of iter) {
        if (first) {
            first = false;
        } else {
            str += delimiter;
        }

        str += el;
    }

    return str
}

// ___Plans
function ___concurrent<P extends unknown[], R>(fn: (...params: P) => R, ...params: P): ___Plan<R> {
    return () => ___asWorker(fn)(...params)
}

function ___asWorker<P extends unknown[], R>(fn: (...params: P) => R): (...params: P) => Promise<R> {
    const code = `
        const fn = (${fn.toString()})

        onmessage = function(event) {
            const args = JSON.parse(event.data);
            const result = fn(...args)
            postMessage(JSON.stringify(result))
        }
    `

    const worker = new Worker(`data:text/javascript;base64,${btoa(code)}`, { type: "module" });

    return (...params) => new Promise(res => {
        worker.onmessage = function (event) {
            res(JSON.parse(event.data))
            worker.terminate()
        }

        worker.postMessage(JSON.stringify(params))
    })
}


// Runtime type checking
const ___RT_UNKNOWN = Symbol('RT_UNKNOWN')
const ___RT_NIL = Symbol('RT_NIL')
const ___RT_BOOLEAN = Symbol('RT_BOOLEAN')
const ___RT_NUMBER = Symbol('RT_NUMBER')
const ___RT_STRING = Symbol('RT_STRING')
const ___RT_LITERAL = Symbol('RT_LITERAL')
const ___RT_ITERATOR = Symbol('RT_ITERATOR')
const ___RT_PLAN = Symbol('RT_PLAN')
const ___RT_REMOTE = Symbol('RT_REMOTE')
const ___RT_ERROR = Symbol('RT_ERROR')
const ___RT_NOMINAL = Symbol('RT_NOMINAL')
const ___RT_ARRAY = Symbol('RT_ARRAY')
const ___RT_RECORD = Symbol('RT_RECORD')
const ___RT_OBJECT = Symbol('RT_OBJECT')

type ___RuntimeType =
    | typeof ___RT_UNKNOWN
    | typeof ___RT_NIL
    | typeof ___RT_BOOLEAN
    | typeof ___RT_NUMBER
    | typeof ___RT_STRING
    | { kind: typeof ___RT_LITERAL, value: string | number | boolean }
    | { kind: typeof ___RT_NOMINAL, nominal: symbol }
    | {
        kind: typeof ___RT_ITERATOR | typeof ___RT_PLAN | typeof ___RT_REMOTE | typeof ___RT_ARRAY,
        inner: ___RuntimeType
    }
    | ___RuntimeType[] // union
    | { kind: typeof ___RT_RECORD, key: ___RuntimeType, value: ___RuntimeType }
    | { kind: typeof ___RT_OBJECT, entries: { key: string, value: ___RuntimeType, optional: boolean }[] }
    | { kind: typeof ___RT_ERROR, inner: ___RuntimeType }


function ___instanceOf(val: any, type: ___RuntimeType): boolean {
    switch (type) {
        case ___RT_UNKNOWN: return true;
        case ___RT_NIL: return val == null;
        case ___RT_BOOLEAN: return typeof val === 'boolean';
        case ___RT_NUMBER: return typeof val === 'number';
        case ___RT_STRING: return typeof val === 'string';
        default: {
            if (Array.isArray(type)) {
                return type.some(member => ___instanceOf(val, member))
            }

            switch (type.kind) {
                case ___RT_LITERAL: return val === type.value;
                case ___RT_ARRAY: return Array.isArray(val) && val.every(member => ___instanceOf(member, type.inner));
                case ___RT_NOMINAL: return typeof val === 'object' && val != null && val.kind === type.nominal
                case ___RT_RECORD: {
                    if (typeof val !== 'object' || val == null) {
                        return false
                    } else {
                        for (const key in val) {
                            if (!___instanceOf(key, type.key)) {
                                return false
                            }
                            if (!___instanceOf(val[key], type.value)) {
                                return false
                            }
                        }

                        return true
                    }
                }
                case ___RT_OBJECT: {
                    if (typeof val !== 'object' || val == null) {
                        return false
                    } else {
                        for (const entry of type.entries) {
                            let found = false

                            for (const key in val) {
                                if (key === entry.key) {
                                    found = true

                                    if (!___instanceOf(val[key], entry.value)) {
                                        // found and doesn't match type
                                        return false
                                    }
                                }
                            }

                            if (!found && !entry.optional) {
                                // not found
                                return false
                            }
                        }

                        return true
                    }
                }
                case ___RT_ERROR: return typeof val === 'object' && val != null && val.kind === ___ERROR_SYM && ___instanceOf(val.value, type.inner)

                // TODO:
                // case ___RT_ITERATOR: return val instanceof ___Iter;
                // case ___RT_PLAN: return false
                // case ___RT_REMOTE: return val instanceof Remote;
            }
        }
    }

    throw Error('Received invalid runtime type')
}
// | MaybeType
// | GenericParamType
// | ProcType
// | FuncType
// | GenericType
// | BoundGenericType
// | ElementType
// | TupleType
// | NominalType
// | AnyType
// | JavascriptEscapeType

function* ___exec(exp: RegExp, s: string) {
    const expr = new RegExp(exp)
    let result

    while (result = expr.exec(s)) {
        const [match, ...groups] = result
        yield { match, groups }

        if (!exp.flags.includes('g')) {
            return
        }
    }
}
