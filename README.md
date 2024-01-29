fib
=====
Best fibonacci server ever :D

# Start the application

In console
```bash
rebar3 shell
```
or create a release and start it

```bash
rebar3 release
```

```bash
 ./_build/default/rel/fib/bin/fib start
```

By default, fib app will start on port `8080` (can be changed in sys.config).
For access swagger open: 
http://localhost:8080/api-docs/index.html


## Test

```bash
rebar3 test
```

