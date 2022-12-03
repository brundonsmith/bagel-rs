use std::{collections::HashMap, ops::Add, rc::Rc};

use crate::{gather_errors, model::module::ModuleID, passes::parse::parse, print_errors};

#[test]
fn Basic_constant() {
    test_check("const x: string = 'foo'", false);
}

#[test]
fn Basic_constant_fail() {
    test_check("const x: number = 'foo'", true);
}

#[test]
fn Binary_operators_1() {
    test_check("const x: boolean = 2 + 2 == 4", false);
}

#[test]
fn Binary_operators_2() {
    test_check("const x: string = 2 + 'foo' + 'bar'", false);
}

#[test]
fn Binary_operators_3() {
    test_check("const x: string = 2 * 3 + 'foo' + 'bar'", false);
}

#[test]
fn Binary_operators_4() {
    test_check("const x: number = 2 * 3 / 12", false);
}

#[test]
fn Binary_operators_5() {
    test_check("const x = 'foo' == 12", true);
}

#[test]
fn Function_as_argument_inference_1() {
    test_check(
        "
      func foo(fn: (val: number) => boolean) => nil
      const x = foo(n => n)",
        true,
    );
}

#[test]
fn Function_as_argument_inference_2() {
    test_check(
        "
      func foo(fn: (val: number) => boolean) => nil
      const x = foo(n => n > 0)",
        false,
    );
}

#[test]
fn Function_as_argument_inference_3() {
    test_check(
        "
      func foo(fn: (val: number) => boolean) => nil
      const x = foo((n: number) => n > 0)",
        false,
    );
}
#[test]
fn Function_as_argument_inference_4() {
    test_check(
        "
      func foo(fn: (val: number) => boolean) => nil
      const x = foo((n: string) => n == 'foo')",
        true,
    );
}

#[test]
fn Function_as_argument_inference_5() {
    test_check(
        "
    pure js func iter<T>(arr: T[]): Iterator<T> => {# #}
    func find<T>(iter: Iterator<T>, fn: (el: T) => boolean): T? => nil

    const x = [2, 4, 6, 8].iter().find(n => n > 5)",
        false,
    );
}

#[test]
fn Function_as_argument_inference_6() {
    test_check(
        "
    js func map<T,R>(iter: Iterator<T>, fn: (el: T) => R): Iterator<R> => {# #}

    func foo(i: Iterator<number>): Iterator<string> => i.map(n => n + 'a')",
        false,
    );
}

#[test]
fn Function_and_proc_in_declaration_inference_pass() {
    test_check(
        "
    type MyFn = (n: number) => number
    type MyProc = (n: number) { }

    const x: MyFn = n => n * 2
    const y: MyProc = (n) {
      const a = n * 2;
    }",
        false,
    );
}

#[test]
fn Function_in_declaration_inference_fail() {
    test_check(
        "
    type MyFn = (n: number) => string
    const x: MyFn = n => n * 2",
        true,
    );
}

#[test]
fn Function_in_object_declaration_inference_pass() {
    test_check(
        "
    type MyObj = {
      myFn: (n: number) => number
    }
    const x: MyObj = {
      myFn: n => n * 2
    }",
        false,
    );
}

#[test]
fn Function_in_object_declaration_inference_fail() {
    test_check(
        "
    type MyObj = {
      myFn: (n: number) => string
    }
    const x: MyObj = {
      myFn: n => n * 2
    }",
        true,
    );
}

#[test]
fn Function_in_array_declaration_inference_pass() {
    test_check(
        "
    type MyArr = ((n: number) => number)[]
    const x: MyArr = [
      n => n * 2,
      n => n + 3
    ]",
        false,
    );
}

#[test]
fn Function_in_array_declaration_inference_fail() {
    test_check(
        "
    type MyArr = ((n: number) => string)[]
    const x: MyArr = [
      n => n * 2,
      n => n + 'foo'
    ]",
        true,
    );
}

#[test]
fn Function_returned_inference_pass() {
    test_check(
        "
    type MyFn = (n: number) => number
    func foo(): MyFn => (
      const factor = 3,
      n => n * 3
    )",
        false,
    );
}

#[test]
fn Function_returned_inference_fail() {
    test_check(
        "
    type MyFn = (n: number) => string
    func foo(): MyFn => (
      const factor = 3,
      n => n * 3
    )",
        true,
    );
}

#[test]
fn Basic_constant_inference_pass() {
    test_check("const x = 'foo'\nconst y: string = x", false);
}

#[test]
fn Basic_constant_inference_fail() {
    test_check("const x = 'foo'\nconst y: number = x", true);
}

#[test]
fn Basic_function_return() {
    test_check("func fn(a: string): string => 'foo'", false);
}

#[test]
fn Basic_function_return_fail() {
    test_check("func fn(): number => 'foo'", true);
}

#[test]
fn Basic_function_fail_2() {
    test_check("func fn(a: string, a: string) => 'foo'", true);
}

#[test]
fn Basic_function_return_inference() {
    test_check(
        "
      func fn(_: string) => 'foo'
      const y: string = fn('z')",
        false,
    );
}

#[test]
fn Basic_function_return_inference_fail() {
    test_check(
        "
      func fn(_: string) => 'foo'
      const y: number = fn('z')",
        true,
    );
}

#[test]
fn Object_literal_with_spread_pass() {
    test_check(
        "
      const a = { foo: 'stuff' }
      const b: { foo: string, bar: string } = { ...a, bar: 'other' }",
        false,
    );
}

#[test]
fn Object_literal_with_spread_fail() {
    test_check(
        "
      const a = { foo: 'stuff' }
      const b: { foo: number, bar: string } = { ...a, bar: 'other' }",
        true,
    );
}

#[test]
fn Array_literal_with_spread_pass_1() {
    test_check(
        "
      const a = [1, 2, 3]
      const b: number[] = [...a, 4]",
        false,
    );
}

#[test]
fn Array_literal_with_spread_pass_2() {
    test_check(
        "
      const a = [1, 2, 3]
      const b: number = [...a, '4'][2]",
        false,
    );
}

#[test]
fn Function_return_type_inference_with_generic() {
    test_check(
        "
      func getThird<T extends string[]>(arr: T) => arr[2]
      const third: string|nil = getThird(['one', 'two', 'three'])",
        false,
    );
}

#[test]
fn Array_literal_with_spread_fail_1() {
    test_check(
        "
      const a = 12
      const b = [...a, 4]",
        true,
    );
}

#[test]
fn Array_literal_with_spread_fail_2() {
    test_check(
        "
      const a = ['1', '2', '3']
      const b: number[] = [...a, 4]",
        true,
    );
}

#[test]
fn Object_type_pass() {
    test_check(
        "type MyObj = {
            foo: string,
            bar: {
                other: number|nil
            }
        }
        
        const obj: MyObj = {
            foo: 'stuff',
            bar: {
                other: 12
            }
        }",
        false,
    );
}

#[test]
fn Object_type_fail_1() {
    test_check(
        "type MyObj = {
            foo: string,
            bar: {
                other: number|nil
            }
        }
        
        const obj: MyObj = {
            foo: 'stuff',
            bar: {
                other: 'foo'
            }
        }",
        true,
    );
}

#[test]
fn Object_type_fail_2() {
    test_check(
        "type MyObj = {
            foo: string,
        }
        
        const obj: MyObj = {
            foo: 'stuff',
            bar: 12
        }",
        true,
    );
}

#[test]
fn Object_type_fail_3() {
    test_check(
        "type MyObj = {
            foo: string,
        }
        
        const obj: MyObj = {
            foo: 12
        }",
        true,
    );
}

#[test]
fn Interface_type_pass() {
    test_check(
        "type MyInterface = interface {
            foo: string
        }
        
        const obj: MyInterface = {
            foo: 'stuff'
        }
        
        
        type MyObj = {
          foo: string
        }

        func foo(arg: MyObj): MyInterface => arg",
        false,
    );
}

#[test]
fn Interface_type_fail_1() {
    test_check(
        "type MyInterface = interface {
          foo: string
        }
        type MyObj = {
          foo: string
        }

        func foo(arg: MyInterface): MyObj => arg",
        true,
    );
}

#[test]
fn Callback_argument_type_inference() {
    test_check(
        "
      func foo(fn: pure (n: number) => number) => fn(12)
      const bar = foo(n => 2 * n)",
        false,
    );
}

#[test]
fn Basic_explicit_generic() {
    test_check(
        "
      func other<T>(a: T): T => a
      const c: number = other<number>(12)",
        false,
    );
}

#[test]
fn Basic_explicit_generic_fail_argument() {
    test_check(
        "
      func other<T>(a: T): T => a
      const c: number = other<number>('foo')",
        true,
    );
}

#[test]
fn Basic_explicit_generic_with_extends() {
    test_check(
        "
      func other<T extends { foo: number }>(a: T): number => a.foo
      const c: number = other<{ foo: number, bar: string }>({ foo: 12, bar: 'stuff' })",
        false,
    );
}

#[test]
fn Basic_explicit_generic_with_outside_extends_fail() {
    test_check(
        "
      func other<T extends { foo: number }>(a: T): number => a.foo
      const c: number = other<{ foo: string, bar: string }>({ foo: 'stuff', bar: 13 })",
        true,
    );
}

#[test]
fn Basic_explicit_generic_with_inside_extends_fail() {
    test_check(
        "
      func other<T extends { foo: number }>(a: T): number => a
      const c: number = other<{ foo: number, bar: string }>({ foo: 12, bar: 'stuff' })",
        true,
    );
}

#[test]
fn Basic_explicit_generic_fail_return() {
    test_check(
        "
      func other<T>(a: T): T => a
      const c: string = other<number>(12)",
        true,
    );
}

#[test]
fn Basic_generic_with_return_inference() {
    test_check(
        "
      func other<T>(a: T) => a
      const c: number = other<number>(12)",
        false,
    );
}

#[test]
fn Nested_generic_calls() {
    test_check(
        "
      func fnA<R>(a: R) => a
      func fnB<T>(b: T): T => fnA<T>(b)
      const c: number = fnB<number>(12)",
        false,
    );
}

#[test]
fn Nested_generic_calls_fail() {
    test_check(
        "
      func fnA<T>(a: R) => a
      func fnB<T>(b: T): T => fnA<T>(12)
      const c: number = fnB<number>(12)",
        true,
    );
}

#[test]
fn Nested_generic_calls_with_return_inference() {
    test_check(
        "
      func fnA<R>(a: R): R => a
      func fnB<T>(b: T) => fnA<T>(b)
      const c: number = fnB<number>(12)",
        false,
    );
}

#[test]
fn Nested_generic_calls_with_return_inference_fail() {
    test_check(
        "
      func fnA<T>(a: R): R => a
      func fnB<T>(b: T) => fnA<T>(b)
      const c: string = fnB<number>(12)",
        true,
    );
}

#[test]
fn Nested_generic_calls_with_same_param_names() {
    test_check(
        "
      func fnA<T>(a: T): T => a
      func fnB<T>(b: T): T => fnA<T>(b)
      const c: number = fnB<number>(12)",
        false,
    );
}

#[test]
fn Basic_generic_param_inference() {
    test_check(
        "
      func other<T>(a: T): T => a
      const c: number = other(12)",
        false,
    );
}

#[test]
fn Basic_generic_param_inference_fail() {
    test_check(
        "
      func other<T>(a: T): T => a
      const c: number = other('foo')",
        true,
    );
}

#[test]
fn Union_generic_param_inference_pass() {
    test_check(
        "
      func other<T>(a: T|nil): T|nil => a
      const c: number|nil = other(12)",
        false,
    );
}

#[test]
fn Union_generic_param_inference_fail() {
    test_check(
        "
      func other<T>(a: T|nil): T|nil => a
      const c: number = other(12)",
        true,
    );
}

#[test]
fn Method_chain_generic_param_inference_pass() {
    test_check("
      js func iter<T>(x: readonly T[]): Iterator<T> => {# #}
      js func map<T,R>(iter: Iterator<T>, fn: (el: T) => R): Iterator<R> => {# #}

      func foo(arr: readonly number[]): Iterator<string> => arr.iter().map((n: number) => 'foo' + n)", false);
}

#[test]
fn Method_chain_generic_param_inference_fail() {
    test_check("
      js func iter<T>(x: readonly T[]): Iterator<T> => {# #}
      js func map<T,R>(iter: Iterator<T>, fn: (el: T) => R): Iterator<R> => {# #}

      func foo(arr: readonly number[]): Iterator<number> => arr.iter().map((n: number) => 'foo' + n)", true);
}

#[test]
fn Iterator_generic_param_inference_pass() {
    test_check("
      // copied from lib/bgl
      export pure js func iter<T>(x: readonly T[]): Iterator<T> => {#
        return ___iter(x)
      #}
      export pure js func filter<T>(iter: Iterator<T>, fn: (el: T) => boolean): Iterator<T> =>     {# return iter.filter(fn) #}
      export pure js func first<T>(iter: Iterator<T>): T? =>                                       {# return iter.first() #} 
      export pure js func concat<T,R>(iter: Iterator<T>, other: Iterator<R>): Iterator<T|R> =>         {# return iter.concat(other) #}
      export pure js func collectArray<T>(iter: Iterator<T>): T[] =>                               {# return iter.collectArray() #}

      const i: Iterator<number> = [1, 2, 3].iter()
      const foo: number? = i.filter((n: number) => n > 2).first()

      func find<T>(iter: Iterator<T>, fn: (el: T) => boolean): T? =>
        iter.filter(fn).first()

      const x: number[] = concat([2, 4].iter(), [6, 8].iter()).collectArray()
      ", false);
}

#[test]
fn Complex_generic() {
    test_check(
        "
      func foo<T>(val: { prop: T }) => [val.prop]
      const x: number[] = foo<number>({ prop: 12 })",
        false,
    );
}

#[test]
fn Complex_generic_fail() {
    test_check(
        "
      func foo<T>(val: { prop: T }) => [val.prop]
      const x: string[] = foo<number>({ prop: 12 })",
        true,
    );
}

#[test]
fn Complex_generic_param_inference() {
    test_check(
        "
      func given<T,R>(val: T|nil, fn: (val: T) => R): R|nil =>
        if val != nil {
          fn(val)
        }

      func double(n: number|nil): number|nil =>
        given(n, (x: number) => x * 2)",
        false,
    );
}

#[test]
fn Generic_extends_clause_fail() {
    test_check(
        "
    func foo<T extends string>(x: T) => x

    const z = foo<number>(12)
    ",
        true,
    );
}

#[test]
fn Function_consts_out_of_order() {
    test_check(
        "
      func foo(_: number) =>
        const a = b + 2,
        const b = 12,
        2 * a",
        true,
    );
}

#[test]
fn Function_consts_in_order() {
    test_check(
        "
      func foo(_: number) =>
        const b = 12,
        const a = b + 2,
        2 * a",
        false,
    );
}

#[test]
fn Const_declarations_in_order() {
    test_check(
        "
      const b = 12
      const a = b + 2",
        false,
    );
}

#[test]
fn Const_declarations_out_of_order() {
    test_check(
        "
      const a = b + 2
      const b = 12",
        true,
    );
}

#[test]
fn Const_declaration_referencing_self() {
    test_check(
        "
      const a: number = a + 2",
        true,
    );
}

#[test]
fn Initializing_a_const_from_a_let() {
    test_check(
        "
      let a = 12
      const b = a",
        true,
    );
}

#[test]
fn Initializing_a_const_from_impure_function() {
    test_check(
        "
      let a = 12
      func foo() => a
      const b = foo()",
        true,
    );
}

#[test]
fn Let_declarations_out_of_order() {
    test_check(
        "
      proc foo() {
        let a = b;
        let b = 12;
      }",
        true,
    );
}

#[test]
fn Let_declarations_in_order() {
    test_check(
        "
      proc foo() {
        let b = 12;
        let a = b;
      }",
        false,
    );
}

#[test]
fn Duplicate_declaration_name_1() {
    test_check(
        "
      proc foo() {
      }
      
      const foo = 12",
        true,
    );
}

// Deno.test({
//   name: "Duplicate declaration name 2",
//   fn() {
//     testMultiModuleTypecheck({
//         'a.bgl': `export const a = 12`,
//         'b.bgl': `
//         from 'a.bgl' import { a }

//         func a() => nil`
//       },
//       true,
//     );
//   },
// });

#[test]
fn Basic_type_refinement() {
    test_check(
        "
      const a: number|nil = 12
      const b = if a != nil { a + 12 }",
        false,
    );
}

#[test]
fn Basic_type_refinement_control() {
    test_check(
        "
      const a: number|nil = 12
      const b = a + 12",
        true,
    );
}

#[test]
fn Deep_refinement() {
    test_check(
        "
      func foo(x: { bar: number|nil }): number|nil =>
        if x.bar != nil {
          x.bar - 12
        }",
        false,
    );
}

#[test]
fn Deep_refinement_control() {
    test_check(
        "
      func foo(x: { bar: number|nil }): number|nil =>
        x.bar - 12",
        true,
    );
}

#[test]
fn Chained_if_else_refinement_pass() {
    test_check(
        "
      func getOutcome(val: number | Error<string>): string =>
        if val instanceof number {
          'nothing wrong!'
        } else {
          val.value
        }",
        false,
    );
}

#[test]
fn Chained_if_else_refinement_fail_1() {
    test_check(
        "
      func getOutcome(val: number | Error<string>): number =>
        if val instanceof number {
          'nothing wrong!'
        } else {
          val.value
        }",
        true,
    );
}

#[test]
fn Chained_if_else_refinement_fail_2() {
    test_check(
        "
      func getOutcome(val: number | Error<string>): string =>
        if val instanceof Error<string> {
          'nothing wrong!'
        } else {
          val.value
        }",
        true,
    );
}

#[test]
fn Boolean_refinement_pass() {
    test_check(
        "
      func foo(val: boolean|string): true|string =>
        if val {
          val
        } else {
          'stuff'
        }",
        false,
    );
}

#[test]
fn Boolean_refinement_fail() {
    test_check(
        "
      func foo(val: boolean|string): true|string =>
        val",
        true,
    );
}

#[test]
fn Comparison_refinement_pass() {
    test_check(
        "
      func foo(x: 'a' | 'b'): 'a' | nil =>
        if x == 'a' {
          x
        } else {
          nil
        }

      func bar(x: 'a' | 'b'): 'a' | nil =>
        if x != 'b' {
          x
        } else {
          nil
        }

      func blah(x: 'a' | 'b'): 'a' | nil =>
        if x == 'b' {
          nil
        } else {
          x
        }

      func stuff(x: 'a' | 'b'): 'a' | nil =>
        if x != 'a' {
          nil
        } else {
          x
        }",
        false,
    );
}

#[test]
fn Comparison_refinement_fail() {
    test_check(
        "
      func foo(x: 'a' | 'b'): 'a' | nil =>
        x",
        true,
    );
}

#[test]
fn Comparison_types_pass() {
    test_check(
        "
    const a: true = 1 < 2
    const b: false = 2 < 1
    
    const c: true = 2 > 1
    const d: false = 1 > 1
    
    const e: true = 2 >= 2
    const f: false = 1 >= 2
    
    const g: true = 3 <= 4
    const h: false = 5 <= 1
    ",
        false,
    );
}

#[test]
fn Refinement_invalidation_pass() {
    test_check(
        "
    func foo(x: { prop: number }): ((n: number) => number) =>
      (n: number) => n * x.prop
    
    proc log(x: unknown) { }
    proc bar() {
      let obj: { prop: number|string } = { prop: 14 }
      if obj.prop instanceof number {
        log(obj.prop * 2);
      }
    }
    ",
        false,
    );
}

#[test]
fn Refinement_invalidation_fail_1() {
    test_check(
        "
    func foo(x: { prop: number|string }): ((n: number) => number) =>
      if x.prop instanceof number {
        (n: number) => n * x.prop
      } else {
        (n: number) => n + 2
      }",
        true,
    );
}

#[test]
fn Refinement_invalidation_fail_2() {
    test_check(
        "
    proc log(x: unknown) { }

    proc bar() {
      let obj: { prop: number|string } = { prop: 14 }
      if obj.prop instanceof number {
        obj.prop = 'foo';
        log(obj.prop * 2);
      }
    }",
        true,
    );
}

#[test]
fn Complex_narrowing() {
    test_check(
        "
    func trim(s: true|string) =>
      if s instanceof string {
        s
      } else {
        'foo'
      }

    func foo(val: boolean|string): false|string =>
      val && val.trim()
    ",
        false,
    );
}

#[test]
fn Complex_narrowing_2() {
    test_check(
        "
    func trim(s: true|string) =>
      if s instanceof string {
        s
      } else {
        'foo'
      }

    func foo(val: boolean): true|string =>
      val || 'stuff'
    ",
        false,
    );
}

#[test]
fn Object_type_spread_1() {
    test_check(
        "
      type Base = {
        foo: number
      }
      
      type Other = {
        ...Base,
        bar: string
      }
      
      const thing: Other = {
        foo: 12,
        bar: 'fgsdg'
      }",
        false,
    );
}

#[test]
fn Object_type_spread_2() {
    test_check(
        "
      type Base = {
        foo: number
      }
      
      type Other = {
        ...Base,
        bar: string
      }
      
      const thing: Other = {
        bar: 'fgsdg'
      }",
        true,
    );
}

#[test]
fn Property_access_pass() {
    test_check(
        "
      const obj = {
        foo: {
          bar: 12
        }
      }

      const val: number = obj.foo.bar

      func foo(obj: { a: number, b: string } | { a: string }): number|string => obj.a
      ",
        false,
    );
}

#[test]
fn Property_access_fail_1() {
    test_check(
        "
      const obj = {
        foo: {
          bar: 12
        }
      }

      const val: number = obj.foo.other
      ",
        true,
    );
}

#[test]
fn Property_access_fail_2() {
    test_check(
        "
      func foo(obj: { a: number, b: string } | { a: string }) => obj.b
      ",
        true,
    );
}

#[test]
fn Property_access_named_type_pass() {
    test_check(
        "
      type Obj = {
        foo: {
          bar: number
        }
      }

      func fn(obj: Obj): number =>
        obj.foo.bar
      ",
        false,
    );
}

#[test]
fn Destructure_pass() {
    test_check(
        "
      type Obj = {
        foo: {
          bar: number
        }
      }

      func fn(obj: Obj): number =>
        const { foo } = obj,
        foo.bar
      ",
        false,
    );
}

#[test]
fn Destructure_fail_1() {
    test_check(
        "
      type Obj = {
        foo: {
          bar: number
        }
      }

      func fn(obj: Obj): string =>
        const { foo } = obj,
        foo.bar
      ",
        true,
    );
}

#[test]
fn Destructure_fail_2() {
    test_check(
        "
      type Obj = {
        foo: {
          bar: number
        }
      }

      func fn(obj: Obj) =>
        const [foo] = obj,
        foo
      ",
        true,
    );
}

#[test]
fn Optional_chain_pass() {
    test_check(
        "
      type Obj = {
        foo: nil | {
          bar: number
        }
      }
      
      func fn(obj: Obj): number|nil =>
        obj.foo?.bar
        
      func foo(tupleArr: ([number | nil, number | nil] | nil)[]): number? =>
        tupleArr[0]?.[1]",
        false,
    );
}

#[test]
fn Optional_chain_fail() {
    test_check(
        "
      type Obj = {
        foo: nil | {
          bar: number
        }
      }
      
      func fn(obj: Obj): number|nil =>
        obj.foo.bar",
        true,
    );
}

#[test]
fn Optional_chain_indexer_pass() {
    test_check(
        "
      type Obj = {
        foo: nil | {
          bar: number
        }
      }
      
      func fn(obj: Obj): number|nil =>
        obj.foo?.['bar']",
        false,
    );
}

#[test]
fn Optional_chain_indexer_fail() {
    test_check(
        "
      type Obj = {
        foo: nil | {
          bar: number
        }
      }
      
      func fn(obj: Obj): number|nil =>
        obj.foo['bar']",
        true,
    );
}

#[test]
fn Optional_arguments_pass() {
    test_check(
        "
      func foo(a: number, b?: string) => a + (b ?? 'foo')
      
      const b = foo(12)
      const c = foo(12, 'stuff')",
        false,
    );
}

#[test]
fn Optional_arguments_fail_1() {
    test_check(
        "
      func foo(a: number, b?: string) => a + b",
        true,
    );
}

#[test]
fn Optional_arguments_fail_2() {
    test_check(
        "
      func foo(a: number, b?: string) => a + (b ?? 'foo')
      
      const b = foo(12)
      const c = foo(12, 13)",
        true,
    );
}

#[test]
fn Optional_arguments_fail_3() {
    test_check(
        "
      func foo(a: number, b?: string) => a + (b ?? 'foo')
      
      const b = foo(12)
      const c = foo(12, 13, 14)",
        true,
    );
}

#[test]
fn Optional_arguments_fail_4() {
    test_check(
        "
      func foo(a: number, b?: string, c: boolean) => a + (b ?? 'foo')",
        true,
    );
}

#[test]
fn Optional_property_access_pass() {
    test_check(
        "
    type Obj = { foo?: string }
    proc foo(obj: Obj) {
      const str: string? = obj.foo;
    }",
        false,
    );
}

#[test]
fn Optional_property_access_fail() {
    test_check(
        "
    type Obj = { foo?: string }
    proc foo(obj: Obj) {
      const str: string = obj.foo;
    }",
        true,
    );
}

// Deno.test({
//   name: "Fail to import module",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       export func foo(b: number) => b * 2`,
//       "module-2.bgl": `
//       from 'module-3.bgl' import { foo }`
//     }, true)
//   }
// })

// Deno.test({
//   name: "Import type across modules pass",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       export type Foo = {
//         method: () {}
//       }`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { Foo }
//       proc bar(foo: Foo) {
//         foo.method();
//       }`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Import type across modules fail",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       export type Foo = {
//         method: () {}
//       }`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { Foo }
//       proc bar(foo: Foo) {
//         foo.other();
//       }`
//     }, true)
//   }
// })

// Deno.test({
//   name: "Inferred type across modules",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       export func foo(b: number) => b * 2`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { foo }
//       const stuff: number = foo(12)`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Inferred type across module with name resolution",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       func foo(a: number) => a * 2
//       export func bar(b: number) => foo(b) * 2`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { bar }
//       const stuff: number = bar(12)`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Expose access in module",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       expose let foo: number = 12`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { foo }
//       proc bar() {
//         let a: number = 0;

//         a = foo;
//       }`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Expose assignment",
//   fn() {
//     testMultiModuleTypecheck({
//       "module-1.bgl": `
//       expose let foo: number = 12`,

//       "module-2.bgl": `
//       from 'module-1.bgl' import { foo }
//       proc bar() {
//         foo = 13;
//       }`
//     }, true)
//   }
// })

#[test]
fn Assignment_ops_pass() {
    test_check(
        "
    proc foo() {
      let n = 0;

      n += 1;
      n -= 2;
      n *= 3;
      n /= 4;

      let s = 'foo';
      s += ' other';
      s += 12;
    }
    ",
        false,
    );
}

#[test]
fn Assignment_ops_fail_1() {
    test_check(
        "
    proc foo() {
      let n = 0;

      n += '1';
    }
    ",
        true,
    );
}

#[test]
fn Assignment_ops_fail_2() {
    test_check(
        "
    proc foo() {
      let s = 'foo';
      s -= 12;
    }
    ",
        true,
    );
}

#[test]
fn Negation() {
    test_check(
        "
    const a: boolean = true
    const b: boolean = false
    const foo = !(a && b)
    ",
        false,
    );
}

#[test]
fn Negation_fail() {
    test_check(
        "
    const a: number = 12
    const foo = !a
    ",
        true,
    );
}

#[test]
fn Literal_type() {
    test_check(
        "
      const foo: 'bar' = 'bar'",
        false,
    );
}

#[test]
fn String_insertion_type() {
    test_check(
        "
      const a = 'a'
      const one = 1
      const t = true

      const foo: 'bar' = 'b${a}r'
      const bar: 'b1r1' = 'b${one}r${one}'
      const stuff: 'it\\'s true!' = 'it\\'s ${t}!'",
        false,
    );
}

#[test]
fn As_casting_pass() {
    test_check(
        "
    func foo(val: number): number|string => val as number|string",
        false,
    );
}

#[test]
fn As_casting_fail() {
    test_check(
        "
    func foo(val: number|string): number => val as number",
        true,
    );
}

#[test]
fn Immutability_test_1() {
    test_check(
        "
    const obj = { foo: 'stuff' }

    proc foo() {
      obj.foo = 'other';
    }",
        true,
    );
}

#[test]
fn Immutability_test_2() {
    test_check(
        "
    proc foo(param: { foo: string }) {
      param = { foo: 'stuff' }
    }",
        true,
    );
}

#[test]
fn Immutability_test_3() {
    test_check(
        "
    proc foo(param: { foo: string }) {
      param.foo = 'stuff';
    }",
        false,
    );
}

#[test]
fn Immutability_test_4() {
    test_check(
        "
    proc foo(param: readonly { foo: string }) {
      param.foo = 'stuff';
    }",
        true,
    );
}

#[test]
fn Immutability_test_5() {
    test_check(
        "
    proc foo(param: readonly { foo: { bar: string } }) {
      param.foo.bar = 'stuff';
    }",
        true,
    );
}

#[test]
fn Immutability_test_6() {
    test_check(
        "
    const obj = { foo: 'bar' }

    proc foo(param: { foo: string }) {
      let alias = obj;
      alias.foo = 'other';
    }",
        true,
    );
}

#[test]
fn Immutability_test_7() {
    test_check(
        "
    const obj = { foo: 'bar' }

    proc foo(param: { foo: string }) {
      let alias = obj as readonly { foo: string }
      alias = { foo: 'other' }
    }",
        false,
    );
}

#[test]
fn Immutability_test_8() {
    test_check(
        "
    proc foo(param: { foo: string }) {
      const obj = param;
    }",
        false,
    );
}

#[test]
fn Immutability_test_9() {
    test_check(
        "
    proc foo(param: { foo: string }) {
      const obj = param;
      obj.foo = 'other';
    }",
        true,
    );
}

#[test]
fn Immutability_test_10() {
    test_check(
        "
    proc foo(param: readonly string) {
      const x: string = param;
    }",
        false,
    );
}

#[test]
fn Immutability_test_11() {
    test_check(
        "
    proc foo(param: readonly number[]) {
      const x: readonly unknown = param;
    }",
        false,
    );
}

#[test]
fn Immutability_test_12() {
    test_check(
        "
    proc foo(param: readonly number[]) {
      const x: unknown = param;
    }",
        true,
    );
}

#[test]
fn Immutability_test_13() {
    test_check(
        "
    func foo(x: unknown): readonly unknown => x
    const a: (x: unknown) => unknown = foo",
        true,
    );
}

#[test]
fn Immutability_test_14() {
    test_check(
        "
    type Objs = { foo: string } | { bar: number }
    type Fn = (x: Objs) => Objs
    type FnReadonlyReturn = (x: Objs) => readonly Objs

    func foo(fn: FnReadonlyReturn): Fn => fn",
        true,
    );
}

#[test]
fn Mutability_test_1() {
    test_check(
        "
    type Obj = { foo?: string }
    proc foo(obj: Obj) {
      obj.foo = 'foo';
      obj.foo = nil;
    }",
        false,
    );
}

#[test]
fn Parentehsized_type_pass() {
    test_check(
        "
    const foo: (string|number)[] = ['foo', 12, 14, 'bar']",
        false,
    );
}

#[test]
fn Parentehsized_type_fail() {
    test_check(
        "
    const foo: (string|number)[] = ['foo', 12, true, 'bar']",
        true,
    );
}

#[test]
fn Nullish_coalescing() {
    test_check(
        "
    func foo(a: number?, b: string?, c: boolean): number|string|boolean => a ?? b ?? c",
        false,
    );
}

#[test]
fn Nullish_coalescing_fail() {
    test_check(
        "
    func foo(a: string?, b: string?): string => a ?? b ?? 12",
        true,
    );
}

#[test]
fn Function_method_call() {
    test_check(
        "
    func foo(s: string) => s.length
    const a: number = 'foo'.foo()",
        false,
    );
}

#[test]
fn Function_method_call_fail() {
    test_check(
        "
    func foo(s: string) => s.length
    const a: string = 'foo'.foo()",
        true,
    );
}

#[test]
fn Function_method_call_with_property_1() {
    test_check(
        "
    type T = { foo: pure () => string }
    const t: T = { foo: () => 'stuff' }

    func foo(s: T) => 12
    const a: string = t.foo()",
        false,
    );
}

#[test]
fn Function_method_call_with_property_2() {
    test_check(
        "
    type T = { foo: pure () => string }
    const t: T = { foo: () => 'stuff' }

    func bar(s: T) => 12
    const a: string = t.foo()",
        false,
    );
}

#[test]
fn Function_method_call_with_property_3() {
    test_check(
        "
    type T = { foo: () => string }
    const t: T = { foo: () => 'stuff' }

    func bar(s: T) => 12
    const a: number = t.bar()",
        false,
    );
}

#[test]
fn Proc_declaration_with_statements_pass() {
    test_check(
        "
    proc log(val: unknown) { }

    proc doStuff(items: Iterator<{ foo: boolean }>) {
      let count = 0;

      for item of items {
          if item.foo {
              count = count + 1;
          }

          if count > 12 {
              log(items);
          } else if count != 10 {
              log('not 10!');
          } else {
              log(nil);
          }
      }

      log(count);
    }",
        false,
    );
}

#[test]
fn Proc_declaration_with_statements_fail_1() {
    test_check(
        "
    proc log(val: unknown) { }

    proc doStuff(items: Iterator<{ foo: boolean }>) {
      let count: string = 0;
    }",
        true,
    );
}

#[test]
fn Proc_declaration_with_statements_fail_2() {
    test_check(
        "
    proc log(val: unknown) { }

    proc doStuff(items: Iterator<{ foo: boolean }>) {
      const count = 0;

      for item of items {
        count = count + 1;
      }
    }",
        true,
    );
}

#[test]
fn Proc_declaration_with_statements_fail_3() {
    test_check(
        "
    proc doStuff(items: Iterator<{ foo: boolean }>) {
      const count = count;
    }",
        true,
    );
}

#[test]
fn Destructuring_statement_pass() {
    test_check(
        "
    proc doStuff(stuff: { foo: boolean }) {
      const { foo } = stuff;
    }",
        false,
    );
}

#[test]
fn Destructuring_statement_fail_1() {
    test_check(
        "
    proc doStuff(stuff: { foo: boolean }) {
      const { foo } = foo;
    }",
        true,
    );
}

#[test]
fn Destructuring_statement_fail_2() {
    test_check(
        "
    proc doStuff(stuff: { foo: boolean }) {
      const { bar } = stuff;
    }",
        true,
    );
}

#[test]
fn Destructuring_statement_fail_3() {
    test_check(
        "
    proc doStuff(stuff: { foo: boolean }) {
      const { foo } = stuff;
      foo = true;
    }",
        true,
    );
}

#[test]
fn Destructuring_array_statement_pass() {
    test_check(
        "
    proc doStuff(tuple: [number, number], array: number[]) {
      const [a] = tuple;
      const [b, c] = tuple;
      const ap: number = a;
      const bp: number = b;
      const cp: number = c;

      const [a2, b2] = array;
      const a2p: number|nil = a2;
      const b2p: number|nil = b2;
    }",
        false,
    );
}

#[test]
fn Destructuring_array_statement_fail_1() {
    test_check(
        "
    proc doStuff(tuple: [number, number]) {
      const [a, b, c] = tuple;
    }",
        true,
    );
}

#[test]
fn Destructuring_array_statement_fail_2() {
    test_check(
        "
    proc doStuff(array: number[]) {
      const [a, b] = array;
      const ap: number = a;
    }",
        true,
    );
}

#[test]
fn Pure_function() {
    test_check(
        "
    const a = 12
    func foo(b: number) => a * b",
        false,
    );
}

// Deno.test({
//   name: "Impure function",
//   fn() {
//     testTypecheck(`
//     let a = 12
//     func foo(b: number) => a * b`,
//     true)
//   }
// })

// Deno.test({
//   name: "Import all pass",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `export func foo(a: number) => a + 'stuff'`,
//       'b.bgl': `
//       import 'a.bgl' as moduleA
//       const x = moduleA.foo(12)`
//     },
//     false)
//   }
// })

// Deno.test({
//   name: "Import all fail 1",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `export func foo2(a: number) => a + 'stuff'`,
//       'b.bgl': `
//       import 'a.bgl' as moduleA
//       const x = moduleA.foo(12)`
//     },
//     true)
//   }
// })

// Deno.test({
//   name: "Import all fail 2",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `export func foo(a: number) => a + 'stuff'`,
//       'b.bgl': `
//       import 'a.bgl' as moduleA
//       const x = foo(12)`
//     },
//     true)
//   }
// })

// Deno.test({
//   name: "Import all fail 3",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `export func foo(a: number) => a + 'stuff'`,
//       'b.bgl': `
//       import 'a.bgl' as moduleA
//       const x = moduleA.foo('stuff')`
//     },
//     true)
//   }
// })

// Deno.test({
//   name: "Import all fail 4",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `export func foo(a: number) => a + 'stuff'`,
//       'b.bgl': `
//       import 'c.bgl' as moduleA`
//     },
//     true)
//   }
// })

#[test]
fn Record_type_pass() {
    test_check(
        "
    type Foo = readonly {[string]: boolean}
    const a: Foo = { foo: true, bar: false }
    const b: Foo = {}
    
    func f(val: Foo): readonly {[string]: boolean|string} => val",
        false,
    );
}

#[test]
fn Record_type_fail_1() {
    test_check(
        "
    type Foo = {[string]: boolean}
    const a: Foo = { foo: true, bar: 12 }",
        true,
    );
}

#[test]
fn Record_type_fail_2() {
    test_check(
        "
    type Foo = {[string]: boolean}
    func f(val: Foo): {[string]: boolean|string} => val",
        true,
    );
}

#[test]
fn Array_type_pass() {
    test_check(
        "
    type Foo = readonly (number|string)[]
    const a: Foo = [1, 'two', 3]
    const b: Foo = [1, 2, 3]
    const c: Foo = []
    
    func f(val: Foo): readonly (number|string|boolean)[] => val
    ",
        false,
    );
}

#[test]
fn Array_type_fail_1() {
    test_check(
        "
    type Foo = (number|string)[]
    func f(val: Foo): (number|string|boolean)[] => val
    ",
        true,
    );
}

#[test]
fn Array_type_fail_2() {
    test_check(
        "
    type Foo = (number|string)[]
    func f(val: Foo): number[] => val
    ",
        true,
    );
}

#[test]
fn Tuple_type_pass() {
    test_check(
        "
    type Foo = readonly [string, number, boolean]
    const a: Foo = ['stuff', 12, true]
    
    func f(val: Foo): readonly [string, number, boolean|number] => val",
        false,
    );
}

#[test]
fn Tuple_type_fail_1() {
    test_check(
        "
    type Foo = [string, number, boolean]
    const a: Foo = [4, 12, true]",
        true,
    );
}

#[test]
fn Tuple_type_fail_2() {
    test_check(
        "
    type Foo = [string, number, boolean]
    const a: Foo = ['stuff', 12, true, 'other']",
        true,
    );
}

#[test]
fn Tuple_type_fail_3() {
    test_check(
        "
    type Foo = [string, number, boolean]
    const a: Foo = ['stuff', 12]",
        true,
    );
}

#[test]
fn Tuple_type_fail_4() {
    test_check(
        "
    type Foo = [string, number, boolean]

    func f(val: Foo): [string, number, boolean|number] => val",
        true,
    );
}

#[test]
fn Tuple_type_fail_5() {
    test_check(
        "
    type Foo = [string, number, boolean]
    const a: Foo = []",
        true,
    );
}

#[test]
fn Tuple_type_fail_6() {
    test_check(
        "
    type Foo = [string, number, boolean]
    func foo(arr: (string|number|boolean)[]): Foo => arr",
        true,
    );
}

#[test]
fn Func_type_pass() {
    test_check(
        "
    type Foo = (a: number, b: string) => number
    const a: Foo = (a: number, b: string) => a
    const b: Foo = (a: number) => a",
        false,
    );
}

#[test]
fn Func_type_fail_1() {
    test_check(
        "
    type Foo = (a: number, b: string) => number
    const a: Foo = (a: number, b: string) => b",
        true,
    );
}

#[test]
fn Func_type_fail_2() {
    test_check(
        "
    type Foo = (a: number, b: string) => number
    const a: Foo = (a: number, b: string, c: boolean) => a",
        true,
    );
}

#[test]
fn Proc_type_pass() {
    test_check(
        "
    type Foo = (a: number, b: string) { }
    const a: Foo = (a: number, b: string) { }
    const b: Foo = (a: number) { }",
        false,
    );
}

#[test]
fn Proc_type_fail_1() {
    test_check(
        "
    type Foo = (a: number, b: string) { }
    const a: Foo = (a: number, b: string, c: boolean) { }",
        true,
    );
}

#[test]
fn Tuple_length_pass() {
    test_check(
        "
    const a = ['a', 'b', 'c']
    const len: 3 = a.length
    const el: 'b' = a[1]",
        false,
    );
}

#[test]
fn Tuple_length_fail() {
    test_check(
        "
    const a = ['a', 'b', 'c']
    const len: 2 = a.length",
        true,
    );
}

#[test]
fn Switch_expression_pass() {
    test_check(
        "
    func bar(n: number): 'zero' | 'one' | 'two' | 'I dunno!' =>
      switch n {
        case 0: 'zero',
        case 1: 'one',
        case 2: 'two',
        default: 'I dunno!'
      }

    func foo(s: 'a' | 'b' | 'c'): string =>
      switch s {
        case 'a': 'zero',
        case 'b': 'one',
        case 'c': 'two'
      }
      
    func stuff(x: { a: number } | string): string =>
      switch x {
        case { a: number }: x.a + '',
        case string: x
      }",
        false,
    );
}

#[test]
fn Switch_expression_fail_1() {
    test_check(
        "
    func bar(n: number): string =>
      switch n {
        case 0: 'zero',
        case 1: 'one',
        case 2: 'two'
      }",
        true,
    );
}

#[test]
fn Switch_expression_fail_2() {
    test_check(
        "
    func foo(n: number): string =>
      switch n {
        case 0: 'zero',
        case 'a': 'one',
        case 2: 'two',
        default: 'default'
      }",
        true,
    );
}

#[test]
fn Switch_expression_fail_3() {
    test_check(
        "
    func foo(s: 'a' | 'b' | 'c'): string =>
      switch s {
        case 'a': 'zero',
        case 'b': 'one',
        case 'c': 'two',
        default: 'other'
      }",
        true,
    );
}

#[test]
fn Range_expression_pass() {
    test_check(
        "
    const a: Iterator<number> = 0..10
    
    proc foo() {
      for n of 5..15 {

      }
    }",
        false,
    );
}

#[test]
fn Range_expression_fail() {
    test_check(
        "
    const a: Iterator<string> = 0..10",
        true,
    );
}

#[test]
fn String_interpolation_pass() {
    test_check(
        "
    const a = 12
    const b = 'dlfkhg'
    const c = true
    const s: string = '${a} - ${b} - ${c}'",
        false,
    );
}

#[test]
fn String_interpolation_fail() {
    test_check(
        "
    const a = { foo: 'bar' }
    const s: string = '${a}'",
        true,
    );
}

#[test]
fn Typeof_type_pass() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: typeof a) => nil
    
    const b = foo(a)",
        false,
    );
}

#[test]
fn Typeof_type_fail() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: typeof a) => nil
    
    const b = foo(12)",
        true,
    );
}

#[test]
fn Keyof_type_pass() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: keyof typeof a) => nil
    
    const b = foo('a')",
        false,
    );
}

#[test]
fn Keyof_type_fail() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: keyof typeof a) => nil
    
    const b = foo('other')",
        true,
    );
}

#[test]
fn Valueof_type_pass() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: valueof typeof a) => nil
    
    const b = foo(1)",
        false,
    );
}

#[test]
fn Valueof_type_fail() {
    test_check(
        "
    const a = { a: 1, b: 2, c: 3 }
    
    func foo(val: valueof typeof a) => nil
    
    const b = foo(12)",
        true,
    );
}

#[test]
fn Elementof_type_pass() {
    test_check(
        "
    const a = [1, 2, 3]
    
    func foo(val: elementof typeof a) => nil
    
    const b = foo(1)",
        false,
    );
}

#[test]
fn Elementof_type_fail() {
    test_check(
        "
    const a = [1, 2, 3]
    
    func foo(val: elementof typeof a) => nil
    
    const b = foo(12)",
        true,
    );
}

#[test]
fn Indexer_pass() {
    test_check("
    func foo(record: {['foo'|'bar']: number}, obj: { prop1: string, prop2: number }, arr: number[], tuple: [number, number]) =>
      const a: number? = record['foo'],
      
      const b: string = obj['prop1'],

      const c: number? = arr[12],

      const d: number = tuple[1],

      nil", false);
}

#[test]
fn Indexer_fail_1() {
    test_check(
        "
    func foo(record: {['foo'|'bar']: number}) =>
      const a: number = record['foo'],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_2() {
    test_check(
        "
    func foo(record: {['foo'|'bar']: number}) =>
      const a = record['stuff'],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_3() {
    test_check(
        "
    func foo(obj: { prop1: string, prop2: number }) =>
      const a = obj['stuff'],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_4() {
    test_check(
        "
    func foo(arr: number[]) =>
      const a: number = arr[12],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_5() {
    test_check(
        "
    func foo(arr: number[]) =>
      const a = arr['stuff'],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_6() {
    test_check(
        "
    func foo(tuple: [number, number]) =>
      const a = tuple[2],
      nil",
        true,
    );
}

#[test]
fn Indexer_fail_7() {
    test_check(
        "
    func foo(tuple: [number, number]) =>
      const a = tuple[-1],
      nil",
        true,
    );
}

#[test]
fn Tuple_indexer_pass() {
    test_check(
        "
    const tuple = [1, 'a', 3]
    func getEl(index: number): number|string|nil => tuple[index]
    ",
        false,
    );
}

#[test]
fn Tuple_indexer_fail() {
    test_check(
        "
    const tuple = [1, 'a', 3]
    func getEl(index: number): string => tuple[index]
    ",
        true,
    );
}

#[test]
fn String_indexer_pass() {
    test_check(
        "
    func getEl(str: string, index: number): string? =>
      str[index]
    ",
        false,
    );
}

#[test]
fn String_indexer_fail() {
    test_check(
        "
    func getEl(str: string, index: number): string =>
      str[index]
    ",
        true,
    );
}

#[test]
fn Exact_string_indexer_pass() {
    test_check(
        "
    const str = 'hello world'
    const letter: string = str[4]
    func foo(index: number): string? => str[index]
    ",
        false,
    );
}

#[test]
fn Exact_string_indexer_fail_1() {
    test_check(
        "
    const str = 'hello world'
    const letter: string = str[20]
    ",
        true,
    );
}

#[test]
fn Exact_string_indexer_fail_2() {
    test_check(
        "
    const str = 'hello world'
    func foo(index: number): string => str[index]
    ",
        true,
    );
}

#[test]
fn Mutability_broadening() {
    test_check(
        "
    proc push(arr: number[], el: number) {
    }

    proc foo() {
      let obj = { a: 0, b: '', c: false }
      obj.a = 12;
      obj.b = 'stuff';

      let arr = [1, 2];
      arr[1] = 3;
      arr.push(4);
    }",
        false,
    );
}

#[test]
fn Throw_statement_pass() {
    test_check(
        "
    proc foo() {
      throw Error('message');
    }",
        false,
    );
}

#[test]
fn Throw_statement_fail() {
    test_check(
        "
    proc foo() {
      throw 12;
    }",
        true,
    );
}

#[test]
fn Error_bubble_pass() {
    test_check(
        "
    proc foo() {
      throw Error('message');
    }
    
    proc main() {
      foo()?;
    }",
        false,
    );
}

#[test]
fn Error_bubble_fail() {
    test_check(
        "
    proc foo() {
      throw Error('message');
    }
    
    proc main() {
      foo();
    }",
        true,
    );
}

#[test]
fn Try_catch_pass() {
    test_check(
        "
    proc log(s: string) { }

    proc foo() {
      throw Error({ prop1: 'stuff' });
    }
    
    proc main() {
      try {
        foo();
      } catch e {
        log(e.value.prop1);
      }
    }",
        false,
    );
}

#[test]
fn Try_catch_fail_1() {
    test_check(
        "
    proc log(s: string) { }

    proc foo() {
      throw Error({ prop1: 'stuff' });
    }
    
    proc main() {
      try {
        foo();
      } catch e {
        log(e.value.prop2);
      }
    }",
        true,
    );
}

#[test]
fn Try_catch_fail_2() {
    test_check(
        "
    proc log(s: string) { }

    proc foo() {
      throw Error({ prop1: 'stuff' });
    }
    
    proc main() {
      try {
        foo()?;
      } catch e {
        log(e.value.prop1);
      }
    }",
        true,
    );
}

#[test]
fn Throws_declaration_pass() {
    test_check(
        "
    proc foo() throws Error<number> {
      throw Error(12);
    }

    type MyProc = (a: string) throws Error<number> { }

    const x: MyProc = foo",
        false,
    );
}

#[test]
fn Throws_declaration_fail_1() {
    test_check(
        "
    proc foo() throws Error<number> {
      throw Error('stuff');
    }",
        true,
    );
}

#[test]
fn Throws_declaration_fail_2() {
    test_check(
        "
    proc foo() throws Error<number> {
      throw Error(12);
    }

    type MyProc = (a: string) throws Error<string> { }

    const x: MyProc = foo",
        true,
    );
}

#[test]
fn Throws_declaration_fail_3() {
    test_check(
        "
    proc log(x: unknown) {}

    proc foo() throws Error<string> {
      throw Error('stuff');
    }

    proc bar() {
      try {
        foo();
      } catch e {
        log(e.value * 2);
      }
    }",
        true,
    );
}

#[test]
fn Circular_type_pass_1() {
    test_check(
        "
    type Foo =
      | { a: Foo }
      | nil
    
    const foo: Foo = { a: nil }",
        false,
    );
}

#[test]
fn Circular_type_pass_2() {
    test_check(
        "
    type JSON =
      | {[string]: JSON}
      | JSON[]
      | string
      | number
      | boolean
      | nil
    
    const foo: JSON = { bar: 'stuff' }",
        false,
    );
}

// Deno.test({
//   name: "Circular type pass 3",
//   fn() {
//     testTypecheck(`
//     export type JSON =
//       | {[string]: JSON}
//       | JSON[]
//       | string
//       | number
//       | boolean
//       | nil

//     export func clone<T extends JSON>(val: T): T =>
//         if val instanceof {[string]: unknown} {
//             val.entries()
//                 .map(entry => [entry[0], entry[1].clone()])
//                 .collectObject()
//         } else if val instanceof unknown[] {
//             val.map(clone).collectArray()
//         } else {
//             val
//         }`,
//     false)
//   }
// })

#[test]
fn Circular_type_fail_1() {
    test_check(
        "
    type JSON =
      | {[string]: JSON}
      | JSON[]
      | string
      | number
      | boolean
      | nil
    
    const foo: JSON = { bar: () => 12 }",
        true,
    );
}

// Deno.test({
//   name: "Nominal pass",
//   fn() {
//     testMultiModuleTypecheck({

//       'a.bgl': `
//       export nominal type A
//       export nominal type B
//       export nominal type C(number)
//       export type Thing = A | B`,

//       'b.bgl': `
//       from 'a.bgl' import { A, B, C, Thing }

//       func foo(thing: Thing) =>
//         if thing instanceof A {
//           12
//         } else {
//           13
//         }

//       const x: Thing = A
//       const y: C = C(12)`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Nominal fail 1",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `
//       export nominal type A`,
//       'b.bgl': `
//       from 'a.bgl' import { A as OtherA }

//       nominal type A

//       const x: OtherA = A`
//     }, true)
//   }
// })

// Deno.test({
//   name: "Nominal fail 2",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.bgl': `
//       nominal type C(number)

//       const y: C = C(12)`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Import JSON pass",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.json': `
//       {
//         "foo": 123,
//         "bar": {
//           "arr": [ "foo" ],
//           "thing": false,
//           "other": null
//         }
//       }`,
//       'b.bgl': `
//       import 'a.json' as a

//       const x: 123 = a.foo`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Import JSON fail",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.json': `
//       {
//         "foo": 123
//       }`,
//       'b.bgl': `
//       import 'a.json' as a

//       const x: string = a.foo`
//     }, true)
//   }
// })

// Deno.test({
//   name: "Import plaintext pass",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.txt': `Lorem ipsum`,
//       'b.bgl': `
//       import 'a.txt' as a

//       const x: 'Lorem ipsum' = a`
//     }, false)
//   }
// })

// Deno.test({
//   name: "Import plaintext fail",
//   fn() {
//     testMultiModuleTypecheck({
//       'a.txt': `Lorem ipsum`,
//       'b.bgl': `
//       import 'a.txt' as a

//       const x: number = a`
//     }, true)
//   }
// })

#[test]
fn Element_type_pass() {
    test_check(
        "
    const x: {
      tag: 'div',
      attributes: { foo: 12 },
      children: [
        'stuff'
      ]
    } = <div foo={12}>{'stuff'}</div>",
        false,
    );
}

#[test]
fn Element_type_fail_1() {
    test_check(
        "
    const x: {
      tag: 'div',
      attributes: { foo: 12 },
      children: [
        'stuff'
      ]
    } = <link foo={12}>{'stuff'}</link>",
        true,
    );
}

#[test]
fn Element_type_fail_2() {
    test_check(
        "
    const x: {
      tag: 'div',
      attributes: { foo: 12 },
      children: [
        'stuff'
      ]
    } = <div foo={13}>{'stuff'}</div>",
        true,
    );
}

#[test]
fn Element_type_fail_3() {
    test_check(
        "
    const x: {
      tag: 'div',
      attributes: { foo: 12 },
      children: [
        'stuff'
      ]
    } = <div foo={12}>{'other'}</div>",
        true,
    );
}

#[test]
fn Awaited_const_decl_pass() {
    test_check(
        "
    async func foo(plan: Plan<string>, plan2: Plan<{ a: string }>) =>
      const foo: string = await plan,
      const { a } = await plan2,
      foo",
        false,
    );
}

#[test]
fn Awaited_const_decl_fail_1() {
    test_check(
        "
    async func foo(plan: Plan<string>) =>
      const foo: number = await plan,
      foo",
        true,
    );
}

#[test]
fn Awaited_const_decl_fail_2() {
    test_check(
        "
    async func foo(plan: string) =>
      const foo = await plan,
      foo",
        true,
    );
}

#[test]
fn Awaited_const_decl_fail_3() {
    test_check(
        "
    async func foo(plan2: Plan<{ a: string }>) =>
      const { b } = await plan2,
      b",
        true,
    );
}

#[test]
fn Awaited_const_decl_fail_4() {
    test_check(
        "
    func foo(plan: Plan<string>, plan2: Plan<{ a: string }>) =>
      const foo: string = await plan,
      const { a } = await plan2,
      foo",
        true,
    );
}

#[test]
fn Object_literal_with_embedded_identifier_pass() {
    test_check(
        "
    const str = 'hello world'
    
    const obj = {
      str
    }
    
    const other: readonly {
      str: string
    } = obj",
        false,
    );
}

#[test]
fn Object_literal_with_embedded_identifier_fail() {
    test_check(
        "
    const str = 'hello world'
    
    const obj = {
      str
    }
    
    const other: readonly {
      str: number
    } = obj",
        true,
    );
}

#[test]
fn Object_literal_to_record_pass() {
    test_check(
        "
    func foo(key: string) => {
      foo: 'sdfasdf',
      ['sfdgsdgf']: 'poijnk',
      [key]: 12
    }
    
    const x: {[string]: 'sdfasdf' | 'poijnk' | 12} = foo('stuff')",
        false,
    );
}

#[test]
fn Object_literal_to_record_fail() {
    test_check(
        "
    func foo(key: string) => {
      foo: 'sdfasdf',
      ['sfdgsdgf']: 'poijnk',
      [key]: 12
    }
    
    const x: {foo: string, sfdgsdgf: string} = foo('stuff')",
        true,
    );
}

#[test]
fn Array_spread_pass() {
    test_check(
        "
    const base = [1, 2, 3]
    const other: (number|string)[] = [...base, 'foobar']",
        false,
    );
}

#[test]
fn Array_spread_fail() {
    test_check(
        "
    const base = [1, 2, 3]
    const other: number[] = [...base, 'foobar']",
        true,
    );
}

// TODO: We could probably figure out the tuple type of `other` here ^

#[test]
fn Await_statement_pass() {
    test_check(
        "
    async proc other() {
    }

    async proc foo(plan: Plan<string>) {
      const s1: string = await plan;
      const x: string = s1;

      const s2 = await plan;
      const y: string = s2;

      await other();
    }",
        false,
    );
}

#[test]
fn Await_statement_fail_1() {
    test_check(
        "
    async proc foo(plan: string) {
      const s = await plan;
    }",
        true,
    );
}

#[test]
fn Await_statement_fail_2() {
    test_check(
        "
    async proc foo(plan: Plan<string>) {
      const s = await plan;
      const x: number = s;
    }",
        true,
    );
}

#[test]
fn Await_statement_fail_3() {
    test_check(
        "
    proc foo(plan: Plan<string>) {
      const s1: string = await plan;
    }",
        true,
    );
}

#[test]
fn Async_proc_type_fail() {
    test_check(
        "
    async proc foo() {
    }

    const x: () {} = foo
    ",
        true,
    );
}

#[test]
fn Property_of_union_type_pass() {
    test_check(
        "
    type Objs =
      | { a: string, b: boolean }
      | { a: string, b: number, c: number}

    func foo(o: Objs): string => o.a
    
    func bar(o: Objs): number|boolean => o.b
    ",
        false,
    );
}

#[test]
fn Property_of_union_type_fail() {
    test_check(
        "
    type Objs =
      | { a: string, b: boolean }
      | { a: string, b: number, c: number}

    func foo(o: Objs): string => o.c
    ",
        true,
    );
}

#[test]
fn Spread_arguments_pass() {
    test_check(
        "
    func foo(...args: number[]): number? => args[0]

    const a = foo(1)
    const b = foo(1, 2)
    const c = foo(1, 2, 3)

    func hof(fn: (num: number) => number?) => nil

    const x = hof(foo)
    ",
        false,
    );
}

#[test]
fn Spread_arguments_fail_1() {
    test_check(
        "
    func foo(...args: number[]): number? => args[0]

    const a = foo('stuff')
    ",
        true,
    );
}

#[test]
fn Spread_arguments_fail_2() {
    test_check(
        "
    func foo(...args: string[]): number? => args[0]

    func hof(fn: (num: number) => nil) => nil

    const x = hof(foo)
    ",
        true,
    );
}

#[test]
fn Decorators_pass() {
    test_check(
        "
    func myDecorator1(fn: (n: number) => number): (n: number) => number => fn
    func myDecorator2(fn: (n: number) => number): (n: number) => number => fn
    func myGenericDecorator<
        TArgs extends readonly unknown[],
        TReturn
    >(fn: (...args: TArgs) => TReturn): (...args: TArgs) => TReturn => fn
    
    @myDecorator1
    @myDecorator2
    func foo(n: number): number => n

    @myGenericDecorator
    func foo2(n: number) => n

    func myProcDecorator(p: () {}): () {} => p

    @myProcDecorator
    proc bar() {
      
    }
    ",
        false,
    );
}

#[test]
fn Decorators_fail_1() {
    test_check(
        "
    const myDecorator = nil
    
    @myDecorator
    func foo(n: number): number => n
    ",
        true,
    );
}

#[test]
fn Decorators_fail_2() {
    test_check(
        "
    func myDecorator1(fn: (n: number) => number): (n: number) => number => fn
    func myDecorator2(fn: (n: string) => number): (n: number) => number => fn
    
    @myDecorator1
    @myDecorator2
    func foo(n: number): number => n",
        true,
    );
}

#[test]
fn Decorators_fail_3() {
    test_check(
        "
    func myProcDecorator(p: () {}): () {} => p

    @myProcDecorator
    func foo(n: number): number => n",
        true,
    );
}

#[test]
fn Decorators_fail_4() {
    test_check(
        "
    func myDecorator(fn: (n: number) => number): (n: string) => number =>
      (n: string) => n.length
    
    @myDecorator
    func foo(n: number): number => n",
        true,
    );
}

#[test]
fn Memo_decorator_pass() {
    test_check(
        "
    export func memo() => <
        TArgs extends readonly unknown[],
        TReturn
    >(fn: (...args: TArgs) => readonly TReturn): (...args: TArgs) => readonly TReturn => js# #js

    @memo()
    func foo(a: number, b: string): readonly (number|string)[] => [a, b]

    @memo()
    func bar(a: number): number => a * 2
    ",
        false,
    );
}

#[test]
fn Memo_decorator_fail() {
    test_check(
        "
    export func memo() => <
        TArgs extends readonly unknown[],
        TReturn
    >(fn: (...args: TArgs) => readonly TReturn): (...args: TArgs) => readonly TReturn => js# #js

    @memo()
    func foo(a: number, b: string): (number|string)[] => [a, b]
    ",
        true,
    );
}

#[test]
fn Regular_expression_pass() {
    test_check(
        "
    const expr: RegExp = /([a-z]+)/gi
    ",
        false,
    );
}

#[test]
fn Regular_expression_fail_1() {
    test_check(
        "
    const expr: RegExp = 12
    ",
        true,
    );
}

#[test]
fn Regular_expression_fail_2() {
    test_check(
        "
    const expr: number = /([a-z]+)/gi
    ",
        true,
    );
}

#[test]
fn Regular_expression_fail_3() {
    test_check(
        "
    const expr = /([a-z]+)/z
    ",
        true,
    );
}

#[test]
fn Tests_pass() {
    test_check(
        "
    func assert(condition: boolean, message?: string): Error<string?>? =>
      if condition {
        nil
      } else {
        Error(message)
      }

    test expr 'Two plus two equals four' => assert(2 + 2 == 3 as number)

    test block 'Do thing!' => {
        throw Error('Foo');
    }

    const x = { foo: 'lkdfghsdfg' }
    test type 'Assignable!' => readonly { foo: string }: typeof x",
        false,
    );
}

#[test]
fn Tests_fail_1() {
    test_check(
        "
    func assert(condition: boolean, message?: string): Error<string?>? =>
      if condition {
        nil
      } else {
        Error(message)
      }

    test expr 'Two plus two equals four' => 2 + 2 == 3 as number",
        true,
    );
}

#[test]
fn Tests_fail_2() {
    test_check(
        "
    test block 'Do thing!' => {
    }",
        true,
    );
}

#[test]
fn Tests_fail_3() {
    test_check(
        "
    const x = { foo: 'lkdfghsdfg' }
    test type 'Assignable!' => readonly { foo: number }: typeof x
    ",
        true,
    );
}

#[test]
fn Number_type_addition_pass() {
    test_check(
        "
    const a = 1
    const b = 2
    const c: 3 = a + b",
        false,
    );
}

#[test]
fn Number_type_addition_fail() {
    test_check(
        "
    const a = 1
    const b = 1
    const c: 3 = a + b",
        true,
    );
}

#[test]
fn String_type_addition_pass() {
    test_check(
        "
    const a = 'a'
    const b = 'b'
    const c: 'ab' = a + b",
        false,
    );
}

#[test]
fn String_type_addition_fail() {
    test_check(
        "
    const a = 'a'
    const b = 'a'
    const c: 'ab' = a + b",
        true,
    );
}

// Deno.test({
//   name: "Complex function type inference pass",
//   fn() {
//     testTypecheck(`
//     type Adder = {
//       inc: (n: number) => number
//     }

//     const a: Adder = {
//       inc: n => n + 1
//     }`,
//     false)
//   }
// })

#[test]
fn Pure_functions_pass() {
    test_check(
        "
    let val = 12

    pure func foo(n: number) => n * 2

    func doubleVal() => foo(val)

    func labeled() => doubleVal() + ' is val'

    const x = foo(2)

    type Foo = pure (n: number) => number
    type Bar = (n: number) => number

    func dfkj(fn: Foo): Bar => fn
    ",
        false,
    );
}

#[test]
fn Pure_functions_fail_1() {
    test_check(
        "
    let val = 12

    pure func foo() => val * 2
    ",
        true,
    );
}

#[test]
fn Pure_functions_fail_2() {
    test_check(
        "
    let val = 12

    pure func foo(n: number) => n * 2

    func doubleVal() => foo(val)

    pure func labeled() => doubleVal() + ' is val'
    ",
        true,
    );
}

#[test]
fn Pure_functions_fail_3() {
    test_check(
        "
    type Foo = pure (n: number) => number
    type Bar = (n: number) => number

    func dfkj(fn: Bar): Foo => fn
    ",
        true,
    );
}

#[test]
fn Pure_procs_pass() {
    test_check(
        "
    let val = 12

    pure proc foo(n: number) {
      let x = n;
    }

    proc bar() {
      foo(val);
    }

    proc main() {
      bar();
    }

    type Foo = pure (n: number) { }
    type Bar = (n: number) { }

    func dfkj(fn: Foo): Bar => fn
    ",
        false,
    );
}

#[test]
fn Pure_procs_fail_1() {
    test_check(
        "
    let val = 12

    pure proc foo(n: number) {
      let x = val;
    }
    ",
        true,
    );
}

#[test]
fn Pure_procs_fail_2() {
    test_check(
        "
    let val = 12

    proc foo() {
      let x = val;
    }

    pure proc bar() {
      foo();
    }
    ",
        true,
    );
}

#[test]
fn Pure_procs_fail_3() {
    test_check(
        "
    type Foo = pure (n: number) { }
    type Bar = (n: number) { }

    func dfkj(fn: Bar): Foo => fn
    ",
        true,
    );
}

fn test_check(bgl: &str, should_fail: bool) {
    let module_id = ModuleID::from("/foo/bar.bgl".to_owned());

    let parsed = parse(module_id.clone(), Rc::new(bgl.to_owned() + " ")).unwrap();

    let mut modules_store = HashMap::new();
    modules_store.insert(module_id, Ok(parsed));
    let modules_store = modules_store.into();

    let errors = gather_errors(&modules_store);

    let total_errors = errors
        .iter()
        .map(|(_, value)| value.len())
        .fold(0, Add::add);
    let had_errors = total_errors > 0;

    if !should_fail && had_errors {
        print_errors(&errors);
        println!("Type check should have passed but failed with errors");
    } else if should_fail && !had_errors {
        panic!("Type check should have failed but passed with no errors");
    }
}
