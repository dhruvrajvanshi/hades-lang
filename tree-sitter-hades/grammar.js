module.exports = grammar({
  name: 'hades',
  rules: {
    source_file: $ => repeat($.definition),
    definition: $ => choice(
      $.def_function,
    ),
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]/,
    def_function: $ => seq(
      'def',
      $.identifier,
      '(',
      ')',
      ':',
      $.type,
    ),
    type: $ => choice(
      $.type_var
    ),
    type_var: $ => $.identifier
  }
})