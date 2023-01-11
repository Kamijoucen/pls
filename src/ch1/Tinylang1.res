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
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    // 取变量
    | Var(x) => List.assoc(x, env)
    }
  }
}

let letAfterExpr = TinyLang1.Add(Cst(1), Var("test"))

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
