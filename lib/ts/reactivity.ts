
// --- Utilities ---

const ___WHOLE_OBJECT = Symbol('WHOLE_OBJECT')

type ___Plan<T> = () => Promise<T>

class ___IterableWeakMap<K extends object, V extends object> {
    private weakMap = new WeakMap<K, V>()
    private map = new Map<string | number | boolean, V>()
    private refSet = new Set<WeakRef<K>>()

    set(key: K, value: V) {
        if (typeof key === 'object') {
            if (!this.weakMap.has(key)) {
                this.refSet.add(new WeakRef(key))
            }

            this.weakMap.set(key, value)
        } else {
            this.map.set(key, value)
        }
    }

    has(key: K) {
        return this.weakMap.has(key) || this.map.has(key)
    }

    get(key: K) {
        return this.weakMap.get(key) ?? this.map.get(key)
    }

    delete(key: K) {
        if (this.weakMap.has(key)) {
            this.weakMap.delete(key)
            this.map.delete(key)
            // this.refSet = new Set([...this.refSet].filter(el => el.deref() !== undefined && el.deref() !== key))
            for (const el of this.refSet) {
                if (el.deref() === undefined || el.deref() === key) {
                    this.refSet.delete(el)
                }
            }
        }
    }

    clear() {
        this.weakMap = new WeakMap<K, V>()
        this.map = new Map<string | number | boolean, V>()
        this.refSet = new Set<WeakRef<K>>()
    }

    *[Symbol.iterator]() {
        for (const keyRef of this.refSet) {
            const key = keyRef.deref()

            if (key !== undefined) {
                const value = this.weakMap.get(key)

                if (value !== undefined) {
                    yield [key, value] as const
                }
            }
        }

        for (const [key, value] of this.map) {
            yield [key, value] as const
        }
    }
}

const ___VALUE_ITSELF = Symbol('VALUE_ITSELF')

class ___WeakTupleSet<TKeys extends readonly any[]> {
    private readonly baseMap = new ___IterableWeakMap<any, any>()

    add(keys: TKeys) {
        let currentMap = this.baseMap

        for (const key of keys) {
            if (!currentMap.has(key)) {
                currentMap.set(key, new ___IterableWeakMap())
            }

            currentMap = currentMap.get(key)
        }

        currentMap[___VALUE_ITSELF] = true
    }

    has(keys: TKeys): boolean {
        for (const _ of this.get(keys)) {
            return true // has at least one
        }

        return false
    }

    delete(keys: readonly any[]) {
        let current = this.baseMap

        for (const key of keys) {
            if (current instanceof ___IterableWeakMap) {
                current = current.get(key)
            } else {
                return
            }
        }

        if (current instanceof ___IterableWeakMap) {
            delete current[___VALUE_ITSELF]
            current.clear()
        }
    }

    * get(keys: readonly any[] = []) {
        let currentMap = this.baseMap

        for (const key of keys) {
            if (!currentMap.has(key)) {
                return
            }

            currentMap = currentMap.get(key)
        }

        if (currentMap instanceof ___IterableWeakMap) {
            yield* ___iterateWeakMapRecursive(currentMap)
        }
    }
}

function* ___iterateWeakMapRecursive(map: ___IterableWeakMap<any, any>): Generator<any[]> {
    for (const [key, value] of map) {
        if (value[___VALUE_ITSELF]) {
            yield [key]
        }

        if (value instanceof ___IterableWeakMap) {
            for (const rest of ___iterateWeakMapRecursive(value)) {
                yield [key, ...rest]
            }
        }
    }
}

// // --- Core state ---

type ___Observable = object
type ___PropertyName = string
type ___Reaction = () => void | Promise<void>;
type ___Computation = (...args: unknown[]) => unknown;

let ___reportObservableAccessed: (<TObservable extends ___Observable, TPropertyName extends string & keyof TObservable>(obj: TObservable, prop: TPropertyName) => void) | undefined;

const ___reactionsToObservables = new ___WeakTupleSet<[___Reaction, ___Observable, ___PropertyName]>();
const ___observablesToReactions = new ___WeakTupleSet<[___Observable, ___PropertyName, ___Reaction]>();

const ___computationsToObservables = new ___WeakTupleSet<[___Computation, ...unknown[], ___Observable, ___PropertyName]>();
const ___computationsToCaches = new ___WeakTupleSet<[___Computation, ...unknown[], unknown]>();
const ___observablesToComputations = new ___WeakTupleSet<[___Observable, ___PropertyName, ___Computation, ...unknown[]]>();

// --- Core functionality ---

function ___observe<
    TObservable extends ___Observable,
    TPropertyName extends ___PropertyName & keyof TObservable
>(
    obj: TObservable,
    prop: TPropertyName
): TObservable[TPropertyName] {
    const value = obj[prop]
    ___reportObservableAccessed?.(obj, prop)
    return value
}

function ___invalidate<
    TObservable extends ___Observable,
    TPropertyName extends ___PropertyName & keyof TObservable
>(
    obj: TObservable,
    prop: TPropertyName,
    val: TObservable[TPropertyName]
) {
    obj[prop] = val

    for (const [reaction] of ___observablesToReactions.get([obj, prop])) {
        reaction()
    }

    for (const [computation] of ___observablesToComputations.get([obj, prop])) {
        ___computationsToCaches.delete([computation])
    }
}

function ___autorun(reaction: ___Reaction) {

    function dispose() {
        for (const [obj, prop] of ___reactionsToObservables.get([reaction])) {
            ___observablesToReactions.delete([obj, prop, reaction])
        }
        ___reactionsToObservables.delete([reaction])
    }

    const previous = ___reportObservableAccessed
    ___reportObservableAccessed = (obj, prop) => {
        dispose()
        ___reactionsToObservables.add([reaction, obj, prop])
        ___observablesToReactions.add([obj, prop, reaction])
    }

    reaction()

    ___reportObservableAccessed = previous

    return dispose
}

function ___memo<F extends ___Computation>(fn: F): F {
    return ((...args) => {

        function dispose() {
            for (const entry of ___computationsToObservables.get([fn])) {
                const obj = entry[entry.length - 2]
                const prop = entry[entry.length - 1]

                ___observablesToComputations.delete([obj, prop, fn])
            }
            ___computationsToCaches.delete([fn])
            ___computationsToObservables.delete([fn])
        }

        if (!___computationsToCaches.has([fn, ...args])) {
            const previous = ___reportObservableAccessed
            ___reportObservableAccessed = (obj, prop) => {
                dispose()
                ___computationsToObservables.add([fn, ...args, obj, prop])
                ___observablesToComputations.add([obj, prop, fn, ...args])
            }

            const result = fn(...args)

            ___reportObservableAccessed = previous

            ___computationsToCaches.add([fn, ...args, result])
        }

        for (const [res] of ___computationsToCaches.get([fn, ...args])) {
            return res
        }
    }) as F
}


// --- Declare missing TS types ---

// interface WeakRef<T extends object> {
//     readonly [Symbol.toStringTag]: "WeakRef";

//     /**
//      * Returns the WeakRef instance's target object, or undefined if the target object has been
//      * reclaimed.
//      */
//     deref(): T | undefined;
// }

// interface WeakRefConstructor {
//     readonly prototype: WeakRef<any>;

//     /**
//      * Creates a WeakRef instance for the given target object.
//      * @param target The target object for the WeakRef instance.
//      */
//     new <T extends object>(target: T): WeakRef<T>;
// }

// declare var WeakRef: WeakRefConstructor;

// interface FinalizationRegistry<T> {
//     readonly [Symbol.toStringTag]: "FinalizationRegistry";

//     /**
//      * Registers an object with the registry.
//      * @param target The target object to register.
//      * @param heldValue The value to pass to the finalizer for this object. This cannot be the
//      * target object.
//      * @param unregisterToken The token to pass to the unregister method to unregister the target
//      * object. If provided (and not undefined), this must be an object. If not provided, the target
//      * cannot be unregistered.
//      */
//     register(target: object, heldValue: T, unregisterToken?: object): void;

//     /**
//      * Unregisters an object from the registry.
//      * @param unregisterToken The token that was used as the unregisterToken argument when calling
//      * register to register the target object.
//      */
//     unregister(unregisterToken: object): void;
// }

// interface FinalizationRegistryConstructor {
//     readonly prototype: FinalizationRegistry<any>;

//     /**
//      * Creates a finalization registry with an associated cleanup callback
//      * @param cleanupCallback The callback to call after an object in the registry has been reclaimed.
//      */
//     new <T>(cleanupCallback: (heldValue: T) => void): FinalizationRegistry<T>;
// }

// declare var FinalizationRegistry: FinalizationRegistryConstructor;
