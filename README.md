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

Highest number of zeroes found: 7
input:mfasih;e08d5eb05a867923d8767888ffdd80a5a6fbf84749162c10b4f2980e3d2d7c4b6  bitcoin:0000000ae87297ee05e43ce135fd9e5b46fb822811868665e565fe2b999f0e50