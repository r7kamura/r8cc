# r8cc

A toy C compiler written in Rust.

## Usage

`cargo run` takes a program and outputs assembly code that returns the calculated result.

```console
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.04s
     Running `target/debug/r8cc`
Usage: r8cc <PROGRAM>
```

Compile `1 + 2 - 3` to assembly code:

```console
$ cargo run -- "1 + 2 - 3"
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/r8cc '1 + 2 - 3'`
.intel_syntax noprefix
.global main
main:
  push 1
  push 2
  pop rdi
  pop rax
  add rax, rdi
  push rax
  push 3
  pop rdi
  pop rax
  sub rax, rdi
  push rax
  pop rax
  ret
```

Assemble output by `cc`:

```console
$ cargo run -- "1 + 2 - 3" > tmp.s
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/r8cc '1 + 2 - 3'`
$ docker-compose run --rm base cc -o tmp tmp.s
$ docker-compose run --rm base ./tmp
$ echo $?
0
```

## Naming

*r8cc* is a successor of my [r7cc](https://github.com/r7kamura/r7cc).
