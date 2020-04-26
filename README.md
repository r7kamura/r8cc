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

Compile given C-like code to assembly code:

```console
$ cargo run -- "int main() { return 1; }"
   Compiling r8cc v0.1.0 (/Users/r7kamura/src/github.com/r7kamura/r8cc)
    Finished dev [unoptimized + debuginfo] target(s) in 0.47s
     Running `target/debug/r8cc 'int main() { return 1; }'`
.intel_syntax noprefix
.data
.global main
main:
  push rbp
  mov rbp, rsp
  push 1
  pop rax
  mov rsp, rbp
  pop rbp
  ret
```

Assemble output by `cc`:

```console
$ cargo run -- "int main() { return 1; }" > tmp.s
    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
     Running `target/debug/r8cc 'int main() { return 1; }'`
$ docker-compose run --rm base cc -o tmp tmp.s
$ docker-compose run --rm base ./tmp
$ echo $?
1
```

## Naming

*r8cc* is a successor of my [r7cc](https://github.com/r7kamura/r7cc).
