# Kompilator języka Instant

Program należy skompilować poleceniem `make`.
Zostaną utworzone dwa programy:

- `insc_llvm`
- `insc_jvm`

Programy te kompilują program napisany w języku Instant do kolejno LLVM i JVM.
Domyślnie tworzony jest także bitkod llvm i skompilowana klasa Javy.
Można to pominąć korzystając z flagi `-c`.

## Budowa kompilatora

W katalogu `lib/` znajduje się plik `jasmin.jar`, który umożliwia skompilowanie kodu maszynowego javy.

Kompilator został napisany w języku Haskell
Implementacja znajduje się w katalogu `src/`. W nim są dwa pliki, kolejno dla każdej wersji
języka docelowego.
W podkatalogu `parser/` znajduje się frontend, odpowiedzialny za parsowanie.
Został on wygenerowany za pomocą `bnfc`.
W podkatalogu `compiler/` znajduje się backend, odpowiedzialny za generowanie kodu źrógłowego, a także funkcje pomocnicze.

Dołączono pliki `.cabal` są alternatywną opcją zbudowania kompilatora.
