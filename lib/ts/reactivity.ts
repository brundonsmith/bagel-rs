
// --- Utilities ---

class IterableWeakMap<K extends object, V extends object> {
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

const VALUE_ITSELF = Symbol('VALUE_ITSELF')

class WeakTupleSet<TKeys extends readonly any[]> {
    private readonly baseMap = new IterableWeakMap<any, any>()

    add(keys: TKeys) {
        let currentMap = this.baseMap

        for (const key of keys) {
            if (!currentMap.has(key)) {
                currentMap.set(key, new IterableWeakMap())
            }

            currentMap = currentMap.get(key)
        }

        currentMap[VALUE_ITSELF] = true
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
            if (current instanceof IterableWeakMap) {
                current = current.get(key)
            } else {
                return
            }
        }

        if (current instanceof IterableWeakMap) {
            delete current[VALUE_ITSELF]
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

        if (currentMap instanceof IterableWeakMap) {
            yield* iterateWeakMapRecursive(currentMap)
        }
    }
}

function* iterateWeakMapRecursive(map: IterableWeakMap<any, any>): Generator<any[]> {
    for (const [key, value] of map) {
        if (value[VALUE_ITSELF]) {
            yield [key]
        }

        if (value instanceof IterableWeakMap) {
            for (const rest of iterateWeakMapRecursive(value)) {
                yield [key, ...rest]
            }
        }
    }
}

// // --- Core state ---

type Observable = object
type PropertyName = string
type Reaction = () => void | Promise<void>;
type Computation = (...args: unknown[]) => unknown;

let reportObservableAccessed: (<TObservable extends Observable, TPropertyName extends string & keyof TObservable>(obj: TObservable, prop: TPropertyName) => void) | undefined;

const reactionsToObservables = new WeakTupleSet<[Reaction, Observable, PropertyName]>();
const observablesToReactions = new WeakTupleSet<[Observable, PropertyName, Reaction]>();

const computationsToObservables = new WeakTupleSet<[Computation, ...unknown[], Observable, PropertyName]>();
const computationsToCaches = new WeakTupleSet<[Computation, ...unknown[], unknown]>();
const observablesToComputations = new WeakTupleSet<[Observable, PropertyName, Computation, ...unknown[]]>();

// --- Core functionality ---

function observe<
    TObservable extends Observable,
    TPropertyName extends PropertyName & keyof TObservable
>(
    obj: TObservable,
    prop: TPropertyName
): TObservable[TPropertyName] {
    const value = obj[prop]
    reportObservableAccessed?.(obj, prop)
    return value
}

function invalidate<
    TObservable extends Observable,
    TPropertyName extends PropertyName & keyof TObservable
>(
    obj: TObservable,
    prop: TPropertyName,
    val: TObservable[TPropertyName]
) {
    obj[prop] = val

    for (const [reaction] of observablesToReactions.get([obj, prop])) {
        reaction()
    }

    for (const [computation] of observablesToComputations.get([obj, prop])) {
        computationsToCaches.delete([computation])
    }
}

function autorun(reaction: Reaction) {

    function dispose() {
        for (const [obj, prop] of reactionsToObservables.get([reaction])) {
            observablesToReactions.delete([obj, prop, reaction])
        }
        reactionsToObservables.delete([reaction])
    }

    const previous = reportObservableAccessed
    reportObservableAccessed = (obj, prop) => {
        dispose()
        reactionsToObservables.add([reaction, obj, prop])
        observablesToReactions.add([obj, prop, reaction])
    }

    reaction()

    reportObservableAccessed = previous

    return dispose
}

function memo<F extends Computation>(fn: F): F {
    return ((...args) => {

        function dispose() {
            for (const entry of computationsToObservables.get([fn])) {
                const obj = entry[entry.length - 2]
                const prop = entry[entry.length - 1]

                observablesToComputations.delete([obj, prop, fn])
            }
            computationsToCaches.delete([fn])
            computationsToObservables.delete([fn])
        }

        if (!computationsToCaches.has([fn, ...args])) {
            const previous = reportObservableAccessed
            reportObservableAccessed = (obj, prop) => {
                dispose()
                computationsToObservables.add([fn, ...args, obj, prop])
                observablesToComputations.add([obj, prop, fn, ...args])
            }

            const result = fn(...args)

            reportObservableAccessed = previous

            computationsToCaches.add([fn, ...args, result])
        }

        for (const [res] of computationsToCaches.get([fn, ...args])) {
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
