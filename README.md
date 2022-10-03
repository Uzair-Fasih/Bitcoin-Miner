# Bitcoin Miner - DOSP Fall 2022

## Authors
| Name | UFID |
| ----------- | ----------- |
| Mohammed Uzair Fasih | [REDACTED] |
| Sohaib Uddin Syed | [REDACTED] |

## Overview
The goal of the project is to use the actor model in erlang and design a distributed solution to mine bitcoins. We have designed our system to work as follows:

- The server (boss) spawns two processes: 
  - **A Supervisor** that spawns actors (allowing the server machine to mine its own bitcoins)
  - **A Performance monitor** that clalculates the CPU utilization for a configurable interval of time (default is 10s).

The main server process then listens for incoming messages from its own processes and also other processes over the OTP.

- Each actors takes as input a `GatorId` and a `Nonce` value. The `Nonce` value is unique for every actor spawned so that repeated work between actors can be avoided.
- In order to generate new coins, each actor uses the previous hash value it generated along with the `GatorId` and `Nonce` (in the following format: `<GatorId><Previous Hash><Nonce>`) as input to generate the next hash value.
- The mining actors pass the mined bitcoins to the server immediately as they are found.
- This process repeats for work unit times after which the actor requests the supervisor for a new `Nonce`.
- The environment variables `GatorId`, `ActorCount`, `WorkUnit` and `PerformanceInterval` are configurable from the `bitcoin.app` file.

## Rubric and Output

### 1) Work Unit

We used a work unit of `1,000,000`. There is very little information needed by the actors that the server can provide. In our implementation the `Nonce` and `GatorId` prefix needs to be provided only once for the actor to work independently and perform work that doesn't result in collisions. Setting the work unit to a higher value allows the actors to work independently to a higher degree, while also allowing them to get another set of problems from the server after exhausting their work unit. This has the benefit of allowing actors to explore another Nonce for generating bitcoins. Our chosen number has a good balance of both and resulted in more mined bitcoins.  

### 2) Sample Result
The following is a runtime sample with an input of 4.
| Input | Output |
| ----------- | ----------- |
| mfasih;986f1cab0a0bc0e9bc2ed60308ceecbccfa6f9d17c64ecad1c6a120e5e324ed9 | 000081f3cdb7cd8d12de69e13a02cabcbf6f474f62868eae79f4da3ab5674bf7 |
| mfasih;33ea694dc18a829d94dd1e4cd8a772ac4f144233c3373458db3efda6e8c1d732 | 0000d108c4b920721d5d9a81d76871bcd8e8b3ce69e19e4265868e14f79454ff |
| mfasih;d87d2c64860b79cb39316c0f45ee09c1ec2b7e1b5413d05e6bfc154baf1d28dc | 00006ddae8ce7535c52ed24d15d1bf22373e32bf5f6f93f0b78873923aed2df2 |
| mfasih;7d1ec45bf4a148bfdb8aaa4580882044ef5c1739f708bb333bcbb9a2f9db5fc4 | 0000d3d79703ad330420e217b9c87b3fd8d64fe9cfcfecd8fdf3466b5659ecbb |

### 3) Running Time
We used an Intel Core i5 CPU with 4 cores and 8 processors running 16 actors.

CPU Time - 1188.29 sec<br/>
Real Time: 259.45 sec<br/>
Core utilisation: 4

### 4) Most Zeros
Highest number of zeroes found in a coin: 7
| Input | Coin |
| ----------- | ----------- |
|mfasih;e08d5eb05a867923d8767888ffdd80a5a6fbf84749162c10b4f2980e3d2d7c4b6  |0000000ae87297ee05e43ce135fd9e5b46fb822811868665e565fe2b999f0e50 |

### 5) Largest number of machines
We were able to connect 4 worker machines to a supervisor machine.

## Running the application
1) Install erlang from https://www.erlang.org/ and clone this repository.

2) Install erlang tools.


3) To start the server(boss), run the following:

```bash
erl -make
erl -name [boss_identifier]@[boss_ip] -pa ebin
> application:start(bitcoin).
> app:start(5).
```

- ```boss_identifier``` is a unique identifier for the boss and ```boss_ip``` is the ip address of the machine the boss is running on.
- ```start``` takes the number of leading zeros desired in a coin.

4) To start the worker, run the following:

```bash
erl -make
erl -name [worker_identifier]@[worker_ip] -pa ebin
> app:start('[boss_identifier]@[boss_ip]').
```
- ```worker_identifier``` is a unique identifier for the worker and ```worker_ip``` is the ip address of the machine the boss is running on.
- ```start``` takes the boss identifier and ip from step 3.
