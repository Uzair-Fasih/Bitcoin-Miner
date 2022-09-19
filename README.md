# Bitcoin Miner - DOSP 2022

To run this erlang application

For the server, run the following:
```bash
erl -make
erl -name uzair@10.20.23.40 -pa ebin
> application:start(bitcoin).
> server:start(5, 4).
```

For the worker, run the following:
```bash
erl -make
erl -name uzair2@10.20.23.40 -pa ebin
> worker:start('uzair@10.20.23.40').
```