``` common-lisp
(asdf:load-system "paren")
(asdf:test-system "paren")
```

### TODOs

+ Basic Functionalities
  + [X] New Type System
    + [X] Type Spec
    + [X] Type Formatter
    + [X] Hook into main code / main test
  + [ ] Preprocessor Directives
        
    A list of preproc directives:
    + `#define`: Defines macros.
    + `#undef`: Undefines macros.
    + `#include`: Includes external files.
    + `#if, #ifdef, #ifndef`: Conditional compilation.
    + `#else, #elif, #endif`: Provides alternatives in conditional compilation.
    + `#pragma`: Compiler-specific instructions.
    + `#error`: Causes a compilation error with a message.
    + `#line`: Sets line numbers and filenames for error messages.
    Note that newlines need to be separated by `\`.
        
  + [X] Implement control flow: block, label, goto.
  + [ ] Lisp Interopts

+ Workflow

  + [X] compile-lsp-file 
  
    Add a function that directly executes a `*.lsp` file by compiling into
    `C`. executing, and printing its output to repl.
    
  + [ ] Run automatic tests in `./examples/`.
  
    Under the `./examples/` directory, process all `*.lsp` files, compile them
    with `paren`, and compare the results with the corresponding `*.c` files.
    
    + [ ] Fix (nested) indentation.
    + [ ] Fix semicolon and newline issue.
    + [ ] Implement lisp interopt.
    + [ ] Implement the c-operator `let` for C-macros.
    + [ ] Build comparison test workflow against example files.
    + [ ] Build integration test for the behavior of generated c code.
    
  + [ ] Break the code and tests into `paren.type(.test)`, `paren.cop(.test)`.

+ Provide Examples (as Programmatic Tests)

  + [ ] Transpile examples from Learn C the Hard Way.
  + [ ] Transpile examples from part of Emacs's source code.
  + [ ] Give persuading example showing superiority of lisp macro.
  + [ ] After `Paren` is stable, use it to implement `BOCL`.
