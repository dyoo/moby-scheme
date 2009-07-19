function compile(s) {
    var exprs = readSchemeExpressions(s);
    var compiledProgram =
        program_dash__greaterthan_compiled_dash_program(exprs);
    return compiled_dash_program_dash_main(compiledProgram);   
}
