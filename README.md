# Bitcoin Miner - DOSP Fall 2022

##Authors
| Name | UFID |
| ----------- | ----------- |
| Mohammed Uzair Fasih | 1234 5678 |
| Sohaib Uddin Syed | 5740 5488 |

## Overview
The goal of the project is use the actor model in erlang and design a distributed solution to mine bitcoins. We have designed our system to work as follows:

- The server(boss) has a supervisor that spawns actors both for itself and any incoming workers and a performance monitor actor that clalculates the CPU utilization in a configurable interval of time.
- Each actors takes as input a GatorId and a nonce value. The nonce value is unique for every actor spawned so that repeated work between actors can be avoided.
- In order to generate new coins, each actor uses the previous hash value with the GatorId and nonce as input to generate the next hash value in a recursive fashion. 
- This process repeats for work unit times after which the actor requests the supervisor for a new nonce.
## Rubric and Output

### 1) Work Unit

A work unit of 1,000,000. 

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

CPU Time - 1188.29 sec
Real Time: 259.45 sec 
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