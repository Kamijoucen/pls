// eval ast
module TinyLang0 = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)

  let rec eval = expr => {
    switch expr {
    | Cst(a) => a
    | Add(a, b) => eval(a) + eval(b)
    | Mul(a, b) => eval(a) * eval(b)
    }
  }
}

module StackMachine = {
  type inser = Cst(int) | Add | Mul
  type inserts = list<inser>
  type operand = int
  type stack = list<operand>

  let rec eval = (inserts: inserts, stk: stack) => {
    switch (inserts, stk) {
    // 如果指令列表为空，直接返回栈顶值
    | (list{}, list{result}) => result
    // 如果首部指令是Cst，将Cst取出，并将剩余部分重新eval，并将Cst的值加入栈顶
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
    // 如果首部指令是Add，从栈中取出两个值，求和并重新压栈
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    // 如果首部指令是Mul，从栈中取出两个值，相乘并重新压栈
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | _ => assert false
    }
  }
}

module Compiler = {
  let rec compile = (expr: TinyLang0.expr) => {
    switch expr {
    | Cst(i) => list{StackMachine.Cst(i)}
    | Add(a, b) => Belt.List.concatMany([compile(a), compile(b), list{StackMachine.Add}])
    | Mul(a, b) => Belt.List.concatMany([compile(a), compile(b), list{StackMachine.Mul}])
    }
  }
}

let expr = TinyLang0.Add(Cst(1), Mul(Cst(2), Cst(3)))
// 1 + 2 * 3
let result1 = TinyLang0.eval(expr)
Js.log(result1)

let code = Compiler.compile(expr)

let result2 = StackMachine.eval(code, list{})
Js.log(result2)
