let concatMany = Belt.List.concatMany
let map = Belt.List.map
let zip = Belt.List.zip

// Tiny language 3: equipped with functions
module Fun = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)
    | Fn(list<string>, expr)
    | App(expr, list<expr>)

  type rec value =
    | Vint(int)
    | Vclosure(env, list<string>, expr)
  and env = list<(string, value)>

  let vadd = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(i1), Vint(i2)) => Vint(i1 + i2)
    | _ => assert false // type error
    }
  }

  let vmul = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(i1), Vint(i2)) => Vint(i1 * i2)
    | _ => assert false // type error
    }
  }

  let rec eval = (expr, env): value => {
    switch expr {
    | Cst(i) => Vint(i)
    | Add(a, b) => vadd(eval(a, env), eval(b, env))
    | Mul(a, b) => vmul(eval(a, env), eval(b, env))
    | Var(x) => List.assoc(x, env)
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    | Fn(xs, e) => Vclosure(env, xs, e) // computation suspended for application
    | App(e, es) => {
        let Vclosure(env_closure, xs, body) = eval(e, env)
        let vs = map(es, e => eval(e, env))
        let fun_env = concatMany([zip(xs, vs), env_closure])
        eval(body, fun_env)
      }
    }
  }
}

// Tiny language 4: nameless style
module NamelessFun = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)
    // no need to store the arity, precompute the index of parameters
    | Fn(expr)
    // // we need semantics checking!
    | App(expr, list<expr>)

  type rec value =
    | Vint(int)
    | Vclosure(env, expr)
  and env = list<value>

  let vadd = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(i1), Vint(i2)) => Vint(i1 + i2)
    | _ => assert false
    }
  }

  let vmul = (v1, v2): value => {
    switch (v1, v2) {
    | (Vint(i1), Vint(i2)) => Vint(i1 * i2)
    | _ => assert false
    }
  }

  let rec eval = (expr, env): value => {
    switch expr {
    | Cst(i) => Vint(i)
    | Add(a, b) => vadd(eval(a, env), eval(b, env))
    | Mul(a, b) => vmul(eval(a, env), eval(b, env))
    | Var(i) => List.nth(env, i)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    | Fn(e) => Vclosure(env, e)
    | App(e, es) => {
        let Vclosure(env_closure, body) = eval(e, env)
        let vs = map(es, e => eval(e, env))
        let fun_env = concatMany([vs, env_closure]) // piece together
        eval(body, fun_env)
      }
    }
  }
}
