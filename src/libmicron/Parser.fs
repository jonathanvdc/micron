namespace libmicron

open Flame
open Flame.Compiler
open libcontextfree

module Parser =
    // A list of nonterminal names.

    /// A nonterminal name for function application.
    let applyIdentifier = "apply"
    let parenIdentifier = "paren"
    let programIdentifier = "program"
    let moduleIdentifier = "module"
    /// A nonterminal name for identifiers. This is
    /// the Moon-Moon of the micron parser.
    let identifierIdentifier = "identifier"
    /// A nonterminal name for integer literals.
    let literalIntIdentifier = "literal-int"
    /// A nonterminal name for double literals.
    let literalDoubleIdentifier = "literal-double"

    // All expressions starting with a dollar sign exist
    // solely to group other nonterminals, or for precedence reasons.
    // They are listed below.

    let expressionGroupIdentifier = "$expr"
    let applyGroupIdentifier = "$apply"
    let parenGroupIdentifier = "$paren"

    /// Tests if the given nonterminal name is really a group
    /// name. An internal node in a parse tree
    /// with a group name nonterminal as its label derives its 
    /// meaning entirely from its only child, which must itself
    /// be a production node.
    let isGroupIdentifier (ident : string) =
        ident.StartsWith("$")

    /// A set of production rules for expressions.
    let expressionRules : Set<ProductionRule<string, TokenType>> =
        Set.ofList
            [
                // $expr -> $apply
                ProductionRule(expressionGroupIdentifier, [Nonterminal applyGroupIdentifier])

                // $apply -> apply | $paren
                ProductionRule(applyGroupIdentifier, [Nonterminal applyIdentifier])
                ProductionRule(applyGroupIdentifier, [Nonterminal parenGroupIdentifier])
                // apply -> $apply $expr
                ProductionRule(applyIdentifier, [Nonterminal applyGroupIdentifier; Nonterminal expressionGroupIdentifier])

                // $paren -> paren | identifier | literal-int | literal-double
                ProductionRule(parenGroupIdentifier, [Nonterminal parenIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal identifierIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalIntIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalDoubleIdentifier])

                // paren -> <(> $expr <)>
                ProductionRule(parenIdentifier, [Terminal TokenType.LParen; Nonterminal expressionGroupIdentifier; Terminal TokenType.RParen])

                // identifier -> <identifier>
                ProductionRule(identifierIdentifier, [Terminal TokenType.Identifier])
                // literal-int -> <integer>
                ProductionRule(literalIntIdentifier, [Terminal TokenType.Integer])
                // literal-double -> <double>
                ProductionRule(literalDoubleIdentifier, [Terminal TokenType.Double])
            ]

    let micronGrammar : ContextFreeGrammar<string, TokenType> =
        // TODO: create the top-level grammar for micron here.
        let rules = 
            [
                // program -> epsilon
                ProductionRule(programIdentifier, [])
                // program -> module
                ProductionRule(moduleIdentifier, [Nonterminal moduleIdentifier])
                // module -> <module> <identifier>
                // ProductionRule("module", [Terminal TokenType.ModuleKeyword; Terminal TokenType.Identifier])

                // etc
            ]

        let allRules = Set.ofList rules |> Set.union expressionRules
        ContextFreeGrammar(allRules, programIdentifier)

