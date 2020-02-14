# Latte compiler
₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇..ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑*̑˟̑

## Installation

Use the package manager [cabal](https://hackage.haskell.org/package/Cabal) to install hex and discrimination-0.4.

```bash
cabal install hex
cabal install discrimination-0.4
make
```

## Usage

latc_x86 foo/bar/baz.lat for a correct program baz.lat should create files baz.s (assembly) and executtable a.out in the directory foo/bar
```bash
latc_x86 foo/bar/baz.lat
```
For an accepted program, the compiler must output OK ("OK\n") and exit with code 0.
For a rejected program, the first line of stderr must be ERROR ("ERROR\n"). Further lines should contain appropriate error messages. The compiler must then exit with a non-zero exit code.

## Latte lenguage
Latte is an imperative language, almost a subset of Java and can be easily translated to it.
Full grammar can be found [latte](https://github.com/gzybola/latte-compiler/blob/master/src/Latte.cf)

### Example: Hello world
```java
// Hello world 

int main () {
  printString("hello world") ;
  return 0 ;
}
```
### Example: factorial in two ways
```int main () {
  printInt(fact(7)) ;
  printInt(factr(7)) ;
  return 0 ;
}

// iterative
int fact (int n) {
  int i,r ;
  i = 1 ;
  r = 1 ;
  while (i < n+1) {
    r = r * i ;
    i++ ;
  }
  return r ;
}

// recursive
int factr (int n) {
  if (n < 2) 
    return 1 ;
  else 
    return (n * factr(n-1)) ; 
}
```
Full description of requirements: [latte-com](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/Latte/description.html)

₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇..ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑̑˟̑̑˟̑ෆ.₊̣̇.ෆ˟̑̑˚̑*̑˟̑
