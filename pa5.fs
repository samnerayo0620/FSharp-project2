module MyModule

(* Author: Samuel Nerayo 
    
    The F# program below intended to write an expression evaluation function 
    that take as arguments a list of variable bindings
    and a string that is composed of letters and operators from the set [+ - * /$@].
    It will scan the string from left to right, interpreting the string as an expression
    in postfix notation (except for the $ and @ special characters). 

 *)

   (*
    Author Note: I am getting warning after I run the program yet I am getting the correct output, 
    I asked the teacher about it and he said it is fine based on the given instruction
    
    To Run the program I did [ dotnet fsi pa5.fs ]on the terminal of visual studio
   *)

(* Helper function to find the value associated with a variable *)
let rec findValue varBindings var =
    match varBindings with
    | [] -> failwith "Variable not found"
    | (v, value)::rest -> if v = var then value else findValue rest var

(* Function to evaluate expressions *)
let eval vars expr =
    let rec innerEval vars stack expr =
        match expr with
        | "" -> List.head stack  // Return the top value on the stack when the string is exhausted
        | rest when rest.[0] = ' ' -> innerEval vars stack (rest.[1..])  // Skip spaces
        | rest ->
            match rest.[0] with
            | '+' ->  // Addition
                let (top :: second :: restStack) = stack
                innerEval vars ((second + top) :: restStack) (rest.[1..])
            | '-' ->  // Subtraction
                let (top :: second :: restStack) = stack
                innerEval vars ((second - top) :: restStack) (rest.[1..])
            | '*' ->  // Multiplication
                let (top :: second :: restStack) = stack
                innerEval vars ((second * top) :: restStack) (rest.[1..])
            | '/' ->  // Division
                let (top :: second :: restStack) = stack
                innerEval vars ((second / top) :: restStack) (rest.[1..])
            | '$' ->  // Swap
                let (top :: second :: restStack) = stack
                innerEval vars (second :: top :: restStack) (rest.[1..])
            | '@' ->  // Variable binding
                let newVar = rest.[1]
                let newVal = List.head stack
                let newVars = (string newVar, newVal)::vars
                innerEval newVars stack (rest.[2..])  // Move to the next character after '@' for the variable
            | _ ->  // Operand
                let value = findValue vars (string rest.[0])
                innerEval vars (value::stack) (rest.[1..])
            | _ -> failwith "Invalid input"  // Handle all other cases
    innerEval vars [] expr

(* Test code *)
let initialBindings = [("a",5);("b",2);("c",9)]
let testEval = eval initialBindings
let exprList = ["ab-ab*ab+--"; "bbb @q bqbq*****"; "ca- @b bc$-"]
let resultList = List.map testEval exprList
printfn "%A" resultList
