# Bitcoin Miner - DOSP 2022

To run this erlang application

For the server, run the following:
```bash
erl -make
erl -name uzair@10.20.23.40 -pa ebin
> application:start(bitcoin).
> app:start(5).
```

For the worker, run the following:
```bash
erl -make
erl -name uzair2@10.20.23.40 -pa ebin
> app:start('uzair@10.20.23.40').
```