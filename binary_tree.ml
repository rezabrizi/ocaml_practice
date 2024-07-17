type treeNode = 
| Leaf 
| Internal of {
  value: int;
  left: treeNode;
  right: treeNode;
}

let rec size_aux acc = function 
| [] -> acc
| Leaf::rest -> size_aux acc rest
| Internal {value = _; left; right }::rest 
  -> size_aux(acc+1) (left :: right :: rest)


let size tree = size_aux 0 [tree]