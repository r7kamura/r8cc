# r8cc

A toy C compiler written in Rust.

## Usage

`cargo run` takes a number and outputs assembly code that returns the number.

```console
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.04s
     Running `target/debug/r8cc`
Usage: r8cc <NUMBER>
$ cargo run -- 123
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/r8cc 123`
.intel_syntax noprefix
.global main
main:
  mov rax, 123
  ret
```

Assemble output by `cc`:

```console
$ cargo run -- 123 > tmp.s
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/r8cc 123`
$ docker-compose run --rm base cc -o tmp tmp.s
$ docker-compose run --rm base ./tmp
$ echo $?
123
```

## Naming

*r8cc* is a successor of my [r7cc](https://github.com/r7kamura/r7cc).
