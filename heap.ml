type 'a heap = 
{ 
  compare: 'a -> 'a -> bool
  ; mutable size: int
  ; data: 'a array
}

let swap arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j); 
  arr.(j) <- temp

let bubble_up heap index =
  let rec aux i =
    if i > 0 then
      let parent = (i - 1) / 2 in
      if heap.compare heap.data.(i) heap.data.(parent) then (
        swap heap.data i parent;
        aux parent
      )
  in
  aux index

let bubble_down heap index = 
  let rec aux i = 
    let left = 2 * i + 1 in 
    let right = 2 * i + 2 in 
    let smallest = 
      if left < heap.size && heap.compare heap.data.(left)  heap.data.(i) then left else i 
    in
    let smallest = if right < heap.size && heap.compare heap.data.(right) heap.data.(smallest) then right else smallest 
    in
    if smallest <> i then (
      swap heap.data i smallest; 
      aux smallest 
    ) 
  in 
  aux index 
       

let insert heap x = 
  if heap.size = Array.length heap.data then 
    failwith "Heap is full";
  heap.data.(heap.size) <- x; 
  bubble_up heap heap.size; 
  heap.size <- heap.size + 1 


let extract heap = 
  if heap.size = 0 then 
    failwith "Heap is empty";
  let min = heap.data.(0) in 
  heap.size <- heap.size - 1; 
  heap.data.(0) <- heap.data.(heap.size); 
  bubble_down heap 0;
  min

let top heap = 
  if heap.size = 0 then failwith "Heap empty"
  heap.data.(0)

  

let create_heap capacity default_value compare = 
  { size = 0; data = Array.make capacity default_value; compare = compare}