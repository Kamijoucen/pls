module TinyLang1 = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  type env = list<(string, int)>

  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) * eval(b, env)
    // 绑定变量，e2标识绑定后需要执行的表达式
    //  list{(x, eval(e1, env) 这行代码就是将e1 产生的值绑定到x上
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    // 取变量，这里的env其实是个map，去变量时根据x作为key去查表
    | Var(x) => List.assoc(x, env)
    }
  }
}

module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type env = list<int>

  // ast eval
  let rec eval = (expr: expr, env: env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) * eval(b, env)
    | Var(n) => List.nth(env, n)
    // e1的值先计算，并放入新环境的头部，
    // 那么对于e2来说，e2所在环境的第一个值就一定是e1的值
    // 也就是这里将e1的值绑定到了新环境的头部，并用新环境去执行e2
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    }
  }
}

type cenv = list<string>
let index = (cenv: cenv, x: string) => {
  let rec find = (cenv: cenv, curIdx: int) => {
    switch cenv {
    | list{} => assert false
    | list{r, ...rest} =>
      if r == x {
        curIdx
      } else {
        find(rest, curIdx + 1)
      }
    }
  }
  find(cenv, 0)
}

// 将带名字的TinyLang1翻译到静态索引的nameless
let rec comp = (expr: TinyLang1.expr, cenv: cenv): Nameless.expr => {
  switch expr {
  | Cst(i) => Cst(i)
  | Add(a, b) => Add(comp(a, cenv), comp(b, cenv))
  | Mul(a, b) => Mul(comp(a, cenv), comp(b, cenv))
  // 这里就是将变量名绑定到静态索引的位置，需要将字符串的变量名映射到数组下标
  // 这里的cenv是compile env，也就是编译时的上下文
  // cenv内容是程序中出现的变量名，索引就是该变量名静态编译后的位置
  | Var(x) => Var(index(cenv, x))
  | Let(x, e1, e2) => Let(comp(e1, cenv), comp(e2, list{x, ...cenv}))
  }
}

module StackMachine1 = {
  type inser = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type inserts = list<inser>
  type operand = int
  type stack = list<operand>

  let rec eval = (inserts: inserts, stack: stack) => {
    switch (inserts, stack) {
    | (list{}, list{rest}) => rest
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | (list{Var(i), ...rest}, _) => eval(rest, list{List.nth(stack, i), ...stack})
    | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | _ => assert false
    }
  }
}

// nameless -> stackmachine
module Compiler1 = {
  let concatMany = Belt.List.concatMany
  // 本地变量与临时变量
  // 本地变量指的是有名字的变量，临时变量指运算的中间结果 （比如 1 + 2 的结果并没有绑定名字）
  type sv = Slocal | Stmp
  type senv = list<sv>

  // sevn编译时环境
  // i nameless中变量的位置
  let sindex = (senv, i) => {
    // acc: vm中变量i对应的位置 acc
    let rec go = (senv, i, acc) => {
      switch senv {
      | list{} => raise(Not_found)
      | list{Slocal, ...rest} =>
        if i == 0 {
          acc
        } else {
          go(rest, i - 1, acc + 1)
        }
      | list{Stmp, ...rest} => go(rest, i, acc + 1)
      }
    }
    go(senv, i, 0)
  }

  let nless2smCompile = (expr: Nameless.expr) => {
    let rec go = (expr: Nameless.expr, senv: senv) => {
      switch expr {
      | Cst(i) => list{StackMachine1.Cst(i)}
      | Add(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Add}])
      | Mul(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Mul}])
      | Let(e1, e2) => concatMany([go(e1, senv), go(e2, list{Slocal, ...senv}), list{Swap, Pop}])
      | Var(x) => list{Var(sindex(senv, x))}
      }
    }
    go(expr, list{})
  }
}

// let(要绑定的名称，需要绑定的值，绑定后执行的代码)

// 绑定后执行的代码
let letAfterExpr = TinyLang1.Add(Cst(1), Var("test"))
// 绑定操作
let varExpr = TinyLang1.Let(
  // 绑定的变量名称
  "test",
  // 需要绑定的表达式
  Mul(Cst(2), Cst(3)),
  // 绑定后需要执行的表达式
  letAfterExpr,
)
let result1 = TinyLang1.eval(varExpr, list{})
Js.log(result1)
assert (result1 == 7)

// homework
// 将 Let(x, Cst(17), Add(Var(x), Var(x))) 编译到StackMachine中

// ast 版本
let tiny1exp = TinyLang1.Let("test", Cst(11), Add(Var("test"), Var("test")))
let namelessexp = comp(tiny1exp, list{})

Js.log(namelessexp)
// 此时neameless打印的结构
// {
//   TAG: 4,
//   _0: { TAG: 0, _0: 11 },
//   _1: { TAG: 1, _0: { TAG: 3, _0: 0 }, _1: { TAG: 3, _0: 0 } }
// }

let result2 = Nameless.eval(namelessexp, list{})
Js.log(result2)

Js.log("compile to vm")
// nameless to vm
let insertResult = Compiler1.nless2smCompile(namelessexp)
Js.log(insertResult)

Js.log("vm result")

let vmResult = StackMachine1.eval(insertResult, list{})
Js.log(vmResult)