# SCOOP v2.1.1

A compiler and static analysis of task parallel C programs.

## Citing

If SCOOP contributes to a project that leads to a scientific
publication, please acknowledge this work by citing the corresponding
paper
[Inference and Declaration of Independence in Task-Parallel Programs](http://link.springer.com/chapter/10.1007/978-3-642-45293-2_1)
(also available
[here](http://users.ics.forth.gr/~polyvios/appt2013a.pdf)).

## Annotation Syntax

    #pragma scoop start(list of variables)
    #pragma scoop finish
    #pragma scoop malloc
    #pragma scoop free
    #pragma scoop sync
    #pragma scoop barrier
    #pragma scoop wait all
    #pragma scoop wait on(list of variables)
    #pragma scoop task [in(<input parameters>)]
                       [inout(<input parameters>)]
                       [out(<input parameters>)]
    
    Parameter notation:
    
      Non stride: <parameter>[\[number of elements (for arrays)\]]
    
      Stride: <parameter>\[Block Rows|Block Columns\]\[[Array Rows|]Array Columns\]
    
      Array Rows is optional and is totally ignored
    
      The parameter size/stride/els/elsz must be an expression, thus we
      don't allow function calls. Also there is no support for the
      conditional operator (? :)
    
    Example:
      #pragma scoop task in(a, b[4]) out(c[16])

## Setup

This section describes how to install SCOOP on your system.  We
suppose that you have checked out/cloned SCOOP under `/opt/scoop`
directory.  However the same instructions should apply for any
alternative path, simply replacing `/opt/scoop` with the desired
alternative path for the rest of this section.

###Dependencies

In order to build SCOOP you will need to install the following
packages:

* ocaml &ge; 3.11.2
* camlp4/ocaml-camlp4/ocaml-camlp4-devel
* Flex
* bison
* indent
* ncurses-devel
* emacs
* gperf


### Compile

To compile SCOOP you have to run `configure` and then `make`.

    ./configure && make

### Install

You can install SCOOP running

    sudo make install

this will create a copy of the scoop executable in
`/usr/local/bin`.

**NOTE**: You still have to keep the current directory to your system.

Alternatively you can append `/opt/scoop` to the `PATH`
variable, i.e,

    echo "export PATH=$PATH:/opt/scoop" >> $HOME/.bashrc

### Uninstall

You can uninstall SCOOP running

    sudo make uninstall

this will erase the copy of the scoop executable from
`/usr/local/bin`.

If you chose the alternative method of adding `/opt/scoop` to your
`PATH` variable, simply remove the added line from your `.bashrc`.

## Usage

    scoop --runtime=<myrmics/dummy> [options] <file> [file2 ...]

### Options

    --runtime                         SCOOP: Define the target runtime (myrmics | dummy)
    --cflags                          SCOOP: Define the flags you want to pass to gcc.
    --include-path                    SCOOP: Define the path containing the runtime header files.
    --debug-SCOOP                     SCOOP: Print debugging information.
    --trace                           SCOOP: Trace SCOOP.
    --out-name                        SCOOP: Specify the output files' prefix. e.g. (default: scoop_trans) will produce scoop_trans.c (and scoop_trans_func.c for cell)
    --pragma                          SCOOP: Specify the string constant following the pragma e.g. (default: runtime name) myrmics will recognise #pragma myrmics ... 
    --disable-sdam                    SCOOP: Disable the static dependence analysis module (SDAM)
    --myrmics-main-function           SCOOP: Specify the name of the main function for the myrmics runtime (default: main)

## Extending SCOOP

In order to add support for your runtime on the SCOOP compiler you
have to take the following steps.

1. copy `src/scoop_dummy.ml` and `src/scoop_dummy.mli` to
   `src/scoop_myruntime.ml` and `src/scoop_myruntime.mli` respectively.
1. append `scoop_myruntime` to the `SCOOP_MODULES` variable in
   `Makefile`.
1. Perform any required changes to `src/scoop_myruntime.ml`
1. Append `Scoop_myruntime.options` to `fd_extraopt` in `src/scoop.ml`
1. Add the following lines to `match !arch with` in `src/scoop.ml` `|
   "myruntime" ->` ` new Scoop_myruntime.codegen callgraph !gen_file
   !pragma_str !includePath`

## Common Errors/Limitations/Known Bugs

* Adding a semicolon at the end of `#pragma`s will make SCOOP fail,
  i.e., `#pragma scoop sync;`
* Putting `#pragma scoop barrier` at the end of a block will make
  SCOOP fail (add a semicolon right below the `#pragma` to fix it).
* **Fatal error: exception Invalid_argument("Unknown")** you probably
  have wrong argument at a call tagged with `#pragma scoop task`
* Using DEFINES or MACROS in pragmas (preprocessor doesn't process
  them)
* Putting `#pragma` directly above a declaration of a variable
  (pragmas are only supported above statements)
* Using directly the runtime API instead of the corresponding
  `#pragma` may result in SDAM not working properly.
* **Error: "segment___0" not found in the #pragma scoop task** usually
  means that the tool is renaming a variable due to previous
  declaration try renaming it manually (e.g. segment2) (This should be
  fixed by now)
