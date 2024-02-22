

///Samuel Nerayo 


 // pa4.fs

//// To run this program on a mac VS-code, follow this
///  dotnet fsi pa4.fs -> Enter 
/// open MyModule -> Enter 
/// Then run all the testing Examples one by one  


module MyModule 

// --------------------------------------------------------------------------------
/// part 1: takes a list of floating-point tuples that represent dimensions of a cube
/// (length, width, and height) and returns the volume of the cube that has the largest volume
let rec maxCubeVolume arr =
    match arr with
    | (a,b,c)::t ->
        let v = a*b*c
        if v > (maxCubeVolume t) then v else (maxCubeVolume t)
    | [] -> 0.0

// --------------------------------------------------------------------------------
/// part 2: takes a string and a list of tuples as arguments

/// Each element of the list is tuple consisting of a string and an int.
/// Result is collected integers from list per provided string sorted in ascending order.
let rec findMatches (str:string) (arr: (string * int) list) =
    match arr with
    | [] -> []
    | (s,n)::t ->
        (if s = str then n::(findMatches str t) else (findMatches str t)) |> List.sort

// --------------------------------------------------------------------------------
// part 3: binary search tree functions for a binary search tree of integers

/// Binary tree definition
type BST =
    | Empty
    | TreeNode of int * BST * BST

/// Inserts the value into the tree and returns the resulting tree.
/// If the value already exists in the tree, returns the tree without inserting the value.
let rec insert n tree =
    match tree with
    | Empty -> TreeNode (n, Empty, Empty)
    | TreeNode (v, left, right) when n < v -> TreeNode (v, insert n left, right)
    | TreeNode (v, left, right) when n > v -> TreeNode (v, left, insert n right)
    | _ -> tree

/// Returns true if the value is in the tree or false if it is not.
let rec contains n tree =
    match tree with
    | Empty -> false
    | TreeNode (v, left, right) ->
        if n = v then true
        elif n < v then contains n left
        else contains n right

/// The parameter func is a Boolean function that takes a single parameter and returns true or false.
/// The function tests the value of each node with func and returns the number of nodes that evaluate to true.
let rec count f tree =
    match tree with
    | Empty -> 0
    | TreeNode (v, left, right) ->
        let leftCount = count f left
        let rightCount = count f right
        let nodeCount = if f v then 1 else 0
        leftCount + rightCount + nodeCount

/// Returns the number of nodes that contain even integers.
let evenCount tree =
    let evenF x = (x % 2) = 0
    count evenF tree

// --------------------------------------------------------------------------------
