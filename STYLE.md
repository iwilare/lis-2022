# Style conventions

Whenever a contextual configuration `c` is present, use the following helper functions:

```ocaml
  let mset = memory_set c.m in
  let mget = memory_get c.m in
  let rget = register_get c.r in
  let rset = register_set c.layout c.r in
  let rget_bit r mask = get_bit mask (rget r) in
  let rset_bit r mask v = rset r @@ set_bit mask v (rget r) in
```

Restrict yourself to only defining the helpers required in the current block.