use std::collections::HashMap;

use regex::Regex;

use crate::{
    model::{BagelError, ModuleID, ParsedModule},
    passes::{parse, Compilable, CompileContext},
    utils::Rcable,
};

#[test]
#[allow(non_snake_case)]
fn Simple_func_declaration() {
    test_compile(
        "func uid() => '12345'",
        "const uid = function ___fn_uid() { return `12345` };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Abbreviated_func() {
    test_compile("const fn = a => a", "const fn = function (a) { return a };");
}

#[test]
#[allow(non_snake_case)]
fn Func_declaration_with_memo() {
    test_compile(
        "
        @memo({ maxItems: 12 })
        func uid() => '12345'
        ",
        "const uid = memo({maxItems: 12})(function ___fn_uid() { return `12345` });",
    );
}

#[test]
#[allow(non_snake_case)]
fn Binary_operator() {
    test_compile("const x = a < b", "const x = (a <  b);");
}

#[test]
#[allow(non_snake_case)]
fn Func_with_constants() {
    test_compile(
        "func uid(n: number) => 
          const double = 2 * n,
          const ten = 5 * double,
          ten",
        "const uid = function ___fn_uid(n: number) { return (() => {
          const double = (2 * n);
          const ten = (5 * double);
          return ten;
        })() };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Property_access() {
    test_compile(
        "const x = a.b.c",
        "const x = ___observe(___observe(a, 'b'), 'c');",
    );
}

#[test]
#[allow(non_snake_case)]
fn Method_call() {
    test_compile("const x = a.b()", "const x = b(a);");
}

#[test]
#[allow(non_snake_case)]
fn Double_method_call() {
    test_compile("const x = a.b()()", "const x = b(a)();");
}

#[test]
#[allow(non_snake_case)]
fn Deep_method_call() {
    test_compile("const x = a.b.c()", "const x = c(___observe(a, 'b'));");
}

#[test]
#[allow(non_snake_case)]
fn Method_chain() {
    test_compile("const x = a.b().c()", "const x = c(b(a));");
}

#[test]
#[allow(non_snake_case)]
fn Property_access_with_space() {
    test_compile(
        "const x = a
          .b
          .c",
        "const x = ___observe(___observe(a, 'b'), 'c');",
    );
}

#[test]
#[allow(non_snake_case)]
fn Method_chain_with_space() {
    test_compile(
        "const x = a
          .b()
          .c()",
        "const x = c(b(a));",
    );
}

#[test]
#[allow(non_snake_case)]
fn If_expression() {
    test_compile(
        "func merge() =>
              if arr1.length <= 0 {
                  2
              } else {
                  3
              }", 
        "const merge = function ___fn_merge() { return ((___observe(arr1, 'length') <= 0) ? 2 : 3) };");
}

#[test]
#[allow(non_snake_case)]
fn Chained_if_expression() {
    test_compile(
        "func merge() =>
              if arr1.length <= 0 {
                  2
              } else if arr1.length <= 1 {
                  3
              } else {
                4
              }",
        "const merge = function ___fn_merge() {
          return
            ((___observe(arr1, 'length') <= 0) ? 2 : 
            (___observe(arr1, 'length') <= 1) ? 3 :
            4) };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Empty_proc() {
    test_compile(
        "proc foo() |> { }",
        "const foo = function ___fn_foo(): void { };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Chained_if_statements() {
    test_compile(
        "proc foo() |> {
                if true {
                  log('true');
                } else if false {
                  log('false');
                } else {
                  log('other');
                }
              }",
        "const foo = function ___fn_foo(): void { 
          if (true) { 
            log(`true`);
          } else if (false) { 
            log(`false`);
          } else { 
            log(`other`);
          };
        };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Object_literal_with_spread() {
    test_compile(
        "const a = { foo: 'stuff' }
        const b = { ...a, bar: 'other' }",
        "const a = {foo: `stuff`};
        const b = {...a, bar: `other`};",
    );
}

#[test]
#[allow(non_snake_case)]
fn Array_literal_with_spread() {
    test_compile(
        "
        const a = [1, 2, 3]
        const b = [...a, 4]",
        "
        const a = [1, 2, 3];
        const b = [...a, 4];",
    );
}

#[test]
#[allow(non_snake_case)]
fn Indexer_expression() {
    test_compile(
        "func uid(arr, i) => arr[i]",
        "const uid = function ___fn_uid(arr, i) { return ___observe(arr, i) };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Indexing_an_array() {
    test_compile("const x = [ arr1[0] ]", "const x = [___observe(arr1, 0)];");
}

#[test]
#[allow(non_snake_case)]
fn Simple_proc_declaration() {
    test_compile(
        "proc doStuff(a) |> { }",
        "const doStuff = function ___fn_doStuff(a): void { };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Basic_proc_declaration() {
    test_compile(
        "
        proc doStuff(items: Iterable<number>) |> {
          let count = 0;
          
          for item of items {
          }
  
          log(count);
        }",
        "
        const doStuff = function ___fn_doStuff(items: Iterable<number>): void { 
          const count = { value: 0 };
  
          for (const item of items.inner) {
          };
  
          log(___observe(count, 'value'));
        };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Proc_declaration_with_statements() {
    test_compile("
        proc doStuff(items: Iterable<number>) |> {
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
        }", "
        const doStuff = function ___fn_doStuff(items: Iterable<number>): void { 
          const count = { value: 0 };
  
          for (const item of items.inner) { 
            if ((___observe(item, 'foo') != null && (___observe(item, 'foo') as unknown) !== false && (___observe(item, 'foo') as any).kind !== ___ERROR_SYM)) { 
              ___invalidate(count, 'value', (___observe(count, 'value') + 1));
            };
  
            if ((___observe(count, 'value') > 12)) { 
              log(items);
            } else if ((___observe(count, 'value') !== 10)) { 
              log(`not 10!`);
            } else { 
              log(undefined);
            };
          };
  
          log(___observe(count, 'value'));
        };");
}

#[test]
#[allow(non_snake_case)]
fn Const_declaration_with_type() {
    test_compile(
        "const foo: FooType = 'stuff'",
        "const foo: FooType = `stuff`;",
    );
}

#[test]
#[allow(non_snake_case)]
fn Basic_property_access() {
    test_compile(
        "const foo = bar.prop1.prop2",
        "const foo = ___observe(___observe(bar, 'prop1'), 'prop2');",
    );
}

#[test]
#[allow(non_snake_case)]
fn Typed_func_declaration() {
    test_compile(
        "func foo(a: string, b: number): number => 0",
        "const foo = function ___fn_foo(a: string, b: number): number { return 0 };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Typed_proc_declaration() {
    test_compile(
        "proc bar(a: string[], b: { foo: number }) |> { }",
        "const bar = function ___fn_bar(a: string[], b: {foo: number}): void { };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Func_type() {
    test_compile(
        "export type MyFn = (a: number, b: string) => string[]",
        "export type MyFn = (a: number, b: string) => (string)[];",
    );
}

#[test]
#[allow(non_snake_case)]
fn Tricky_type_parse_1() {
    test_compile(
        "type Foo = string | number[]",
        "type Foo = string | (number)[];",
    );
}

#[test]
#[allow(non_snake_case)]
fn Tricky_type_parse_2() {
    test_compile(
        "type Foo = (a: string) => (b: number|boolean) |> { foo: nil[] }",
        "type Foo = (a: string) => (b: number | boolean) |> { foo: (null | undefined)[], };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Comment_test_line() {
    test_compile(
        "func foo() => 13 // foo bar comment
         const a = 12",
        "const foo = function ___fn_foo() { return 13 };
         const a = 12;",
    );
}

#[test]
#[allow(non_snake_case)]
fn Comment_test_block() {
    test_compile(
        "func foo() => 13 /* foo bar comment
              moar comment*/
         const a = 12",
        "const foo = function ___fn_foo() { return 13 };
         const a = 12;",
    );
}

#[test]
#[allow(non_snake_case)]
fn Negation_precedence() {
    test_compile(
        "
      const a: boolean = true
      const b: boolean = true

      const foo = !a && b
      ",
        "
      const a: boolean = true;

      const b: boolean = true;

      const foo = (!(a) && b);",
    );
}

#[test]
#[allow(non_snake_case)]
fn Indexer_assignment() {
    test_compile(
        "
      export proc setItem(key: string, value: string) |> {
        _localStorage[key] = value;
        setLocalStorage(key, value);
      }",
        "
      export const setItem = function ___fn_setItem(key: string, value: string): void {
        ___invalidate(_localStorage, key, value);
        setLocalStorage(key, value);
      };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Assignment_ops() {
    test_compile(
        "
      proc foo() |> {
        let n = 0;
        n *= 2;

        let s = 'foo';
        s += ' other';
      }",
        "
      const foo = function ___fn_foo(): void {
        const n = { value: 0 };
        ___invalidate(n, 'value', n.value * 2);

        const s = { value: `foo` };
        ___invalidate(s, 'value', s.value + ` other`);
      };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Method_proc_call() {
    test_compile(
        "
      proc push<T>(arr: T[], el: T) |> {
        // stub
      }

      export proc bar() |> {
        let foo = [1, 2, 3];
        foo.push(4);
      }",
        "
      const push = function ___fn_push<T>(arr: T[], el: T): void {
      };

      export const bar = function ___fn_bar(): void {
        const foo = { value: [1, 2, 3] };
        push(___observe(foo, 'value'), 4);
      };",
    );
}

#[test]
#[allow(non_snake_case)]
fn JS_func() {
    test_compile(
        "
      js func foo(a: number, b: string): string |> {#
        return a + b;
      #}
      ",
        "
      const foo = function ___fn_foo(a: number, b: string): string {
        return a + b;
      };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn JS_proc() {
    test_compile(
        "
      js proc foo(a: { foo: number }, b: number) |> {#
        a.foo = b;
      #}
      ",
        "
      const foo = function ___fn_foo(a: {foo: number}, b: number): void {
        a.foo = b;
      };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn While_loop() {
    test_compile(
        "
      proc foo() |> {
        while true {
          log('stuff');
        }
      }",
        "
      const foo = function ___fn_foo(): void {
        while (true) {
            log(`stuff`);
        };
      };",
    );
}

#[test]
#[allow(non_snake_case)]
fn Runtime_types() {
    test_compile(
        "
      const x = 12

      nominal type FooNominal(string)

      const a = x instanceof string
      const b = x instanceof number
      const c = x instanceof unknown
      const d = x instanceof boolean
      const e = x instanceof nil
      const f = x instanceof 'stuff'
      const g = x instanceof Iterable<number>
      const h = x instanceof Plan<number>
      const j = x instanceof Error<number>
      const k = x instanceof FooNominal
      const l = x instanceof number[]
      const m = x instanceof {[string]: number}
      const n = x instanceof { a: string, b: number }
      const o = x instanceof { a: string, b?: number }",
        "
      const x = 12;

      const ___FooNominal = Symbol('FooNominal');
      const FooNominal = ((value: string): FooNominal => ({ kind: ___FooNominal, value })) as (((value: string) => FooNominal) & { sym: typeof ___FooNominal });
      FooNominal.sym = ___FooNominal;
      type FooNominal = { kind: typeof ___FooNominal, value: string };

      const a = ___instanceOf(x, ___RT_STRING);
      const b = ___instanceOf(x, ___RT_NUMBER);
      const c = ___instanceOf(x, ___RT_UNKNOWN);
      const d = ___instanceOf(x, ___RT_BOOLEAN);
      const e = ___instanceOf(x, ___RT_NIL);
      const f = ___instanceOf(x, { kind: ___RT_LITERAL, value: `stuff` });
      const g = ___instanceOf(x, { kind: ___RT_ITERATOR, inner: ___RT_NUMBER });
      const h = ___instanceOf(x, { kind: ___RT_PLAN, inner: ___RT_NUMBER });
      const j = ___instanceOf(x, { kind: ___RT_ERROR, inner: ___RT_NUMBER });
      const k = ___instanceOf(x, { kind: ___RT_NOMINAL, nominal: FooNominal.sym });
      const l = ___instanceOf(x, { kind: ___RT_ARRAY, inner: ___RT_NUMBER });
      const m = ___instanceOf(x, { kind: ___RT_RECORD, key: ___RT_STRING, value: ___RT_NUMBER });
      const n = ___instanceOf(x, { kind: ___RT_OBJECT, entries: [{ key: 'a', value: ___RT_STRING, optional: false },{ key: 'b', value: ___RT_NUMBER, optional: false }] });
      const o = ___instanceOf(x, { kind: ___RT_OBJECT, entries: [{ key: 'a', value: ___RT_STRING, optional: false },{ key: 'b', value: ___RT_NUMBER, optional: true }] });
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Non_JS_truthiness() {
    test_compile(
        "
      func foo(n: number?) =>
        n && n + 1
      ",
        "
      const foo = function ___fn_foo(n: number|null|undefined) { return
        ((n != null && (n as unknown) !== false && (n as any).kind !== ___ERROR_SYM)
          ? (n + 1)
          : n) };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Range_operator() {
    test_compile(
        "
      const arr = (0..9).collectArray()
      ",
        "
      const arr = collectArray((___range(0, 9)));
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Async_proc() {
    test_compile(
        "
      async proc doStuff(plan: Plan<string>) |> {
        const str = await plan;
        log(str);
      }
      ",
        "
      const doStuff = async function ___fn_doStuff(plan: ___Plan<string>): Promise<void>{
        const str = await (plan)();
        log(str);
      };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Async_func() {
    test_compile(
        "
      func getStuff(plan: Plan<string>) => (
        const str = await plan,

        str
      )
      ",
        "
      const getStuff = function ___fn_getStuff(plan: ___Plan<string>) { return (() => (async () => {
        const str = await (plan)();
        return str;
      })()) };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Type_declarations() {
    test_compile(
        "
      export type Foo = {
        a: string[],
        b: 'stuff',
        c: Iterable<Other>
      }

      export nominal type Bar(number)
      ",
        "
      export type Foo = {a: string[], b: `stuff`, c: Iterable<Other>};

      const ___Bar = Symbol('Bar');
      export const Bar = ((value: number): Bar => ({ kind: ___Bar, value })) as (((value: number) => Bar) & { sym: typeof ___Bar });
      Bar.sym = ___Bar;
      export type Bar = { kind: typeof ___Bar, value: number };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Imports() {
    test_compile(
        "
      from './foo.bgl' import { a, b as otherb }
      import './bar.bgl' as bar
      ",
        "
      import { a, b as otherb } from `./foo.bgl.ts`;
      import * as bar from `./bar.bgl.ts`;
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Inline_destructuring_declaration() {
    test_compile(
        "
      func foo(obj: { a: string, b: number }) =>
        const { a } = obj,
        a
      ",
        "
      const foo = function ___fn_foo(obj: {a: string, b: number}) { return (() => {
        const { a } = obj;
        return a;
      })() };
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Inline_const() {
    test_compile(
        "
      const a: number = (
        const b: number = 12,

        b * 2
      )",
        "
      const a: number = ((() => {
        const b: number = 12;
        return (b * 2);
      })());",
    );
}

#[test]
#[allow(non_snake_case)]
fn Types() {
    test_compile(
        "
      type A = <T>({})
      type B = (a: number) |> {}
      type C = (a: string) => number
      type E = { ...Other }
      type F = {[string]: number}
      type G = Error<string>
      type H = unknown
      type I = (string | number)

      const x = 'foo' as string
      ",
        "
      type A = unknown;
      type B = (a: number) => void;
      type C = (a: string) => number;
      type E = Other & {};
      type F = Record<string, number>;
      type G = ___Error<string>;
      type H = unknown;
      type I = (string | number);

      const x = `foo` as string;
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Decorators() {
    test_compile(
        "
      func myDecorator1(fn: (n: number) => number): (n: number) => number => fn
      func myDecorator2(fn: (n: number) => number): (n: number) => number => fn

      @myDecorator1
      @myDecorator2
      func foo(n: number): number => n

      func myProcDecorator(p: () |> {}): () |> {} => p

      @myProcDecorator
      proc bar() |> {
      }
      ",
        "
      const myDecorator1 = function ___fn_myDecorator1(fn: (n: number) => number): (n: number) => number { return fn };
      const myDecorator2 = function ___fn_myDecorator2(fn: (n: number) => number): (n: number) => number { return fn };

      const foo = myDecorator1(myDecorator2(function ___fn_foo(n: number): number { return n }));

      const myProcDecorator = function ___fn_myProcDecorator(p: () => void): () => void { return p };

      const bar = myProcDecorator(function ___fn_bar(): void {
      });
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Regular_expressions() {
    test_compile(
        "
      const expr: RegExp = /([a-z]+)/gi
      ",
        "
      const expr: RegExp = /([a-z]+)/gi;
      ",
    );
}

#[test]
#[allow(non_snake_case)]
fn Tests() {
    test_compile(
        "
      test expr 'Two plus two equals four' => assert(2 + 2 == 3)

      test block 'Do thing!' |> {
          throw Error('Foo');
      }",
        "
      export const ___tests = {
        testExprs: [{ name: 'Two plus two equals four', expr: assert(((2 + 2) === 3)) }],
        testBlocks: [{ name: 'Do thing!', block: () => {
          return { kind: ___ERROR_SYM, value: `Foo` };;
        } }]
      }",
    );
}

#[test]
#[allow(non_snake_case)]
fn Strings() {
    test_compile(
        "
      const a = 'Hello world ${12}'
      const b = 'it\\'s me!'
      const c = '\\\\foobar'
      const d = 'num: \\${12}'
      const e = ''",
        "
      const a = `Hello world ${12}`;
      const b = `it's me!`;
      const c = `\\\\foobar`;
      const d = `num: \\${12}`;
      const e = ``;",
    );
}

#[test]
#[allow(non_snake_case)]
fn Element_tag() {
    test_compile(
        "
    const thing = (
      <div>
        {'The thing:'}
        <a href={'google.com'}>{'My link'}</a>
      </div>
    )
    ",
        "
      const thing = ({
        tag:'div',
        attributes:{ },
        children:[
          `The thing:`,
          {
            tag:'a',
            attributes:{ href:`google.com`, },
            children:[ `My link`, ]
          },
        ]
      });
    ",
    )
}

fn test_compile(bgl: &str, js: &str) {
    let module_id = ModuleID::Artificial("foo".to_owned().rc());
    let parsed = parse(module_id.clone(), (bgl.to_owned() + " ").rc());

    match parsed {
        Ok(parsed) => {
            let mut modules_store = HashMap::new();
            modules_store.insert(
                module_id.clone(),
                Ok(ParsedModule::Bagel {
                    module_id: module_id.clone(),
                    ast: parsed.clone(),
                }),
            );

            let mut compiled = String::new();
            parsed
                .compile(
                    CompileContext {
                        modules: &modules_store,
                        current_module: modules_store.get(&module_id).unwrap().as_ref().unwrap(),
                        include_types: true,
                        qualify_identifiers_with: None,
                        qualify_all_identifiers: false,
                    },
                    &mut compiled,
                )
                .unwrap();
            assert_eq!(normalize(&compiled), normalize(js));
        }
        Err(err) => {
            println!(
                "{}\n",
                BagelError::from(err).pretty_print_string(true).unwrap(),
            );
            panic!("Parsing failed");
        }
    }
}

fn normalize(js: &str) -> String {
    let re = Regex::new(r"[\s]+").unwrap();
    re.replace_all(js.trim(), " ").to_string()
}
