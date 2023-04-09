function ___log<T>(x: T): T {
    console.log(x);
    return x;
}

const ___LOADING = Symbol('LOADING')

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

// ___Iterators
function* ___rangeInner(start: number, end: number): Generator<number> {
    for (let i = start; i < end; i++) {
        yield i;
    }
}
function ___range(start: number, end: number): ___Iter<number> {
    return new ___Iter(() => ___rangeInner(start, end))
}

function* ___repeatInner<T>(val: T, count: number): Generator<T> {
    for (let i = 0; i < count; i++) {
        yield val
    }
}
function ___repeat<T>(val: T, count: number): ___Iter<T> {
    return new ___Iter(___repeatInner(val, count))
}

function* ___entriesInner<V>(obj: { [key: string]: V }): Generator<[string, V]> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield [key, obj[key]];
    }
}

function ___entries<V>(obj: { [key: string]: V }): ___Iter<[string, V]> {
    return new ___Iter(() => ___entriesInner(obj))
}

function* ___keysInner<V>(obj: { [key: string]: V }): Generator<string> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield key
    }
}

function ___keys<V>(obj: { [key: string]: V }): ___Iter<string> {
    return new ___Iter(() => ___keysInner(obj))
}

function* ___valuesInner<V>(obj: { [key: string]: V }): Generator<V> {
    ___observe(obj, ___WHOLE_OBJECT)
    for (const key in obj) {
        yield obj[key]
    }
}

function ___values<V>(obj: { [key: string]: V }): ___Iter<V> {
    return new ___Iter(() => ___valuesInner(obj))
}

const ___INNER_ITER = Symbol('INNER_ITER')

type ___RawIter<T> = Iterable<T> | (() => Generator<T>)

function ___iter<T>(inner: ___RawIter<T>): ___Iter<T> {
    return new ___Iter(inner)
}

class ___Iter<T> {

    [___INNER_ITER]: ___RawIter<T>

    constructor(inner: ___RawIter<T>) {
        this[___INNER_ITER] = inner
    }

    get inner() {
        const inner = this[___INNER_ITER]
        return typeof inner === 'function' ? inner() : inner
    }

    map<R>(fn: (el: T) => R): ___Iter<R> {
        return new ___Iter(___map(fn, this[___INNER_ITER]))
    }

    reduce<R>(init: R, fn: (acc: R, el: T) => R): R {
        let acc = init
        for (const el of this.inner) {
            acc = fn(acc, el)
        }
        return acc
    }

    filter(fn: (el: T) => boolean): ___Iter<T> {
        return new ___Iter(___filter(fn, this[___INNER_ITER]))
    }

    slice(start: number, end?: number): ___Iter<T> {
        return new ___Iter(___slice(start, end, this[___INNER_ITER]))
    }

    takeWhile(fn: (el: T) => boolean): ___Iter<T> {
        return new ___Iter(___takeWhile(this[___INNER_ITER], fn))
    }

    sorted(fn: (a: T, b: T) => number): ___Iter<T> {
        return new ___Iter(Array.from(this.inner).sort(fn))
    }

    every(fn: (a: T) => boolean): boolean {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        for (const el of inner) {
            if (!fn(el)) {
                return false
            }
        }

        return true
    }

    some(fn: (a: T) => boolean): boolean {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        for (const el of inner) {
            if (fn(el)) {
                return true
            }
        }

        return false
    }

    count(): number {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        let count = 0;

        for (const _ of inner) {
            count++;
        }

        return count;
    }

    first(): T | undefined {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        for (const item of inner) {
            return item
        }
    }

    concat(other: ___Iter<T>): ___Iter<T> {
        return new ___Iter(___concat(this[___INNER_ITER], other[___INNER_ITER] ?? other))
    }

    zip<R>(other: ___Iter<R>): ___Iter<[T | null | undefined, R | null | undefined]> {
        return new ___Iter(___zip(this[___INNER_ITER], other[___INNER_ITER] ?? other))
    }

    indexed(): ___Iter<[T, number]> {
        return new ___Iter(___indexed(this[___INNER_ITER]))
    }

    collectArray(): T[] {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        return Array.from(inner)
    }

    collectObject(): { [key: string]: unknown } {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        const obj = {} as { [key: string]: unknown }
        for (const entry of inner) {
            if (Array.isArray(entry)) {
                const [key, value] = entry

                obj[key] = value
            }
        }

        return obj
    }

    set(): Set<T> {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }
        return new Set(inner)
    }

    // Bagel will ensure this is an ___Iter<string>
    join(delimiter: string): string {
        const inner = this.inner

        if (inner[Symbol.iterator]) {
            ___observe(inner, ___WHOLE_OBJECT)
        }

        let str = "";
        let first = true;

        for (const el of inner) {
            if (first) {
                first = false;
            } else {
                str += delimiter;
            }

            str += el;
        }

        return str;
    }
}

function* ___slice<T>(start: number | undefined, end: number | undefined, iter: ___RawIter<T>): Generator<T> {
    const inner = typeof iter === 'function' ? iter() : iter

    if (inner[Symbol.iterator]) {
        ___observe(inner, ___WHOLE_OBJECT)
    }

    let index = 0;
    for (const el of inner) {
        if ((start == null || index >= start) && (end == null || index < end)) {
            yield el;
        }
        index++;
    }
}

function* ___takeWhile<T>(iter: ___RawIter<T>, fn: (el: T) => boolean): Generator<T> {
    const inner = typeof iter === 'function' ? iter() : iter

    if (inner[Symbol.iterator]) {
        ___observe(inner, ___WHOLE_OBJECT)
    }

    for (const el of inner) {
        if (fn(el)) {
            yield el;
        } else {
            return
        }
    }
}

function* ___map<T, R>(fn: (el: T) => R, iter: ___RawIter<T>): Generator<R> {
    const inner = typeof iter === 'function' ? iter() : iter

    if (inner[Symbol.iterator]) {
        ___observe(inner, ___WHOLE_OBJECT)
    }

    for (const el of inner) {
        yield fn(el);
    }
}

function* ___filter<T>(fn: (el: T) => boolean, iter: ___RawIter<T>): Generator<T> {
    const inner = typeof iter === 'function' ? iter() : iter

    if (inner[Symbol.iterator]) {
        ___observe(inner, ___WHOLE_OBJECT)
    }

    for (const el of inner) {
        if (fn(el)) {
            yield el;
        }
    }
}

function* ___concat<T>(iter1: ___RawIter<T>, iter2: ___RawIter<T>): Generator<T> {
    const inner1 = typeof iter1 === 'function' ? iter1() : iter1
    const inner2 = typeof iter2 === 'function' ? iter2() : iter2

    if (inner1[Symbol.iterator]) {
        ___observe(inner1, ___WHOLE_OBJECT)
    }
    if (inner2[Symbol.iterator]) {
        ___observe(inner2, ___WHOLE_OBJECT)
    }

    for (const el of inner1) {
        yield el;
    }

    for (const el of inner2) {
        yield el;
    }
}

function* ___zip<T, R>(iter1: ___RawIter<T>, iter2: ___RawIter<R>): Generator<[T | null | undefined, R | null | undefined]> {
    const inner1 = typeof iter1 === 'function' ? iter1() : iter1
    const inner2 = typeof iter2 === 'function' ? iter2() : iter2

    if (inner1[Symbol.iterator]) {
        ___observe(inner1, ___WHOLE_OBJECT)
    }
    if (inner2[Symbol.iterator]) {
        ___observe(inner2, ___WHOLE_OBJECT)
    }


    const a = inner1[Symbol.iterator]();
    const b = inner2[Symbol.iterator]();

    let nextA = a.next();
    let nextB = b.next();

    while (!nextA.done || !nextB.done) {
        yield [nextA.value, nextB.value];

        nextA = a.next();
        nextB = b.next();
    }
}

function* ___indexed<T>(iter: ___RawIter<T>): Generator<[T, number]> {
    const inner = typeof iter === 'function' ? iter() : iter

    if (inner[Symbol.iterator]) {
        ___observe(inner, ___WHOLE_OBJECT)
    }

    let index = 0;
    for (const el of inner) {
        yield [el, index];
        index++
    }
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
