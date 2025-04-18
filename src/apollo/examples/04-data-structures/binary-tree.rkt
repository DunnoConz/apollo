#lang racket

;; Binary Tree Example
;; Demonstrates functional binary tree implementation with pattern matching

;; Define a binary tree node
;; - Empty tree is represented by #f
;; - Nodes contain value, left subtree, and right subtree
(struct tree-node (value left right) #:transparent)

;; Create an empty tree
(define empty-tree #f)

;; Check if a tree is empty
(define (tree-empty? tree)
  (eq? tree #f))

;; Create a leaf node (no children)
(define (make-leaf value)
  (tree-node value #f #f))

;; Insert a value into a binary search tree
(define (tree-insert tree value)
  (cond
    ;; If tree is empty, create a new leaf node
    [(tree-empty? tree) (make-leaf value)]
    
    ;; If value is less than current node, insert into left subtree
    [(< value (tree-node-value tree))
     (tree-node 
      (tree-node-value tree)
      (tree-insert (tree-node-left tree) value)
      (tree-node-right tree))]
    
    ;; If value is greater than current node, insert into right subtree
    [(> value (tree-node-value tree))
     (tree-node 
      (tree-node-value tree)
      (tree-node-left tree)
      (tree-insert (tree-node-right tree) value))]
    
    ;; If value is equal to current node, just return the tree unchanged
    [else tree]))

;; Find a value in a binary search tree
(define (tree-find tree value)
  (cond
    ;; If tree is empty, value is not found
    [(tree-empty? tree) #f]
    
    ;; If value is less than current node, search left subtree
    [(< value (tree-node-value tree))
     (tree-find (tree-node-left tree) value)]
    
    ;; If value is greater than current node, search right subtree
    [(> value (tree-node-value tree))
     (tree-find (tree-node-right tree) value)]
    
    ;; If value matches current node, return true
    [else #t]))

;; Convert a tree to a list using in-order traversal
(define (tree->list tree)
  (if (tree-empty? tree)
      '()
      (append
       (tree->list (tree-node-left tree))
       (list (tree-node-value tree))
       (tree->list (tree-node-right tree)))))

;; Calculate the height of a tree
(define (tree-height tree)
  (if (tree-empty? tree)
      0
      (+ 1 (max (tree-height (tree-node-left tree))
                (tree-height (tree-node-right tree))))))

;; Count the number of nodes in a tree
(define (tree-count tree)
  (if (tree-empty? tree)
      0
      (+ 1 (tree-count (tree-node-left tree))
           (tree-count (tree-node-right tree)))))

;; Print a tree in a readable format (using pattern matching)
(define (tree->string tree)
  (match tree
    [#f "()"]
    [(tree-node value #f #f)
     (format "(~a)" value)]
    [(tree-node value left #f)
     (format "(~a ~a)" value (tree->string left))]
    [(tree-node value #f right)
     (format "(~a _ ~a)" value (tree->string right))]
    [(tree-node value left right)
     (format "(~a ~a ~a)" 
             value 
             (tree->string left) 
             (tree->string right))]))

;; Build a sample tree
(define (build-sample-tree)
  (let* ([t empty-tree]
         [t (tree-insert t 50)]
         [t (tree-insert t 30)]
         [t (tree-insert t 70)]
         [t (tree-insert t 20)]
         [t (tree-insert t 40)]
         [t (tree-insert t 60)]
         [t (tree-insert t 80)])
    t))

;; Test the binary tree implementation
(define sample-tree (build-sample-tree))

;; Display tree information
(printf "Tree representation: ~a\n" (tree->string sample-tree))
(printf "Tree as sorted list: ~a\n" (tree->list sample-tree))
(printf "Tree height: ~a\n" (tree-height sample-tree))
(printf "Node count: ~a\n" (tree-count sample-tree))

;; Test search
(printf "Contains 40? ~a\n" (tree-find sample-tree 40))
(printf "Contains 55? ~a\n" (tree-find sample-tree 55))

;; Expected Luau output:
#|
-- Binary tree implementation
local function tree_node(value, left, right)
  return {
    value = value,
    left = left,
    right = right
  }
end

local empty_tree = nil

local function tree_empty(tree)
  return tree == nil
end

local function make_leaf(value)
  return tree_node(value, nil, nil)
end

local function tree_insert(tree, value)
  if tree_empty(tree) then
    return make_leaf(value)
  elseif value < tree.value then
    return tree_node(tree.value, tree_insert(tree.left, value), tree.right)
  elseif value > tree.value then
    return tree_node(tree.value, tree.left, tree_insert(tree.right, value))
  else
    return tree
  end
end

local function tree_find(tree, value)
  if tree_empty(tree) then
    return false
  elseif value < tree.value then
    return tree_find(tree.left, value)
  elseif value > tree.value then
    return tree_find(tree.right, value)
  else
    return true
  end
end

local function tree_to_list(tree)
  if tree_empty(tree) then
    return {}
  else
    local result = {}
    
    -- Helper function to perform in-order traversal
    local function traverse(node)
      if node.left then traverse(node.left) end
      table.insert(result, node.value)
      if node.right then traverse(node.right) end
    end
    
    traverse(tree)
    return result
  end
end

local function tree_height(tree)
  if tree_empty(tree) then
    return 0
  else
    local left_height = tree_height(tree.left)
    local right_height = tree_height(tree.right)
    return 1 + math.max(left_height, right_height)
  end
end

local function tree_count(tree)
  if tree_empty(tree) then
    return 0
  else
    return 1 + tree_count(tree.left) + tree_count(tree.right)
  end
end

local function tree_to_string(tree)
  if tree_empty(tree) then
    return "()"
  elseif tree.left == nil and tree.right == nil then
    return string.format("(%s)", tostring(tree.value))
  elseif tree.right == nil then
    return string.format("(%s %s)", tostring(tree.value), tree_to_string(tree.left))
  elseif tree.left == nil then
    return string.format("(%s _ %s)", tostring(tree.value), tree_to_string(tree.right))
  else
    return string.format("(%s %s %s)", 
                        tostring(tree.value), 
                        tree_to_string(tree.left), 
                        tree_to_string(tree.right))
  end
end

-- Build a sample tree
local function build_sample_tree()
  local t = empty_tree
  t = tree_insert(t, 50)
  t = tree_insert(t, 30)
  t = tree_insert(t, 70)
  t = tree_insert(t, 20)
  t = tree_insert(t, 40)
  t = tree_insert(t, 60)
  t = tree_insert(t, 80)
  return t
end

-- Test the implementation
local sample_tree = build_sample_tree()

-- Display tree information
print(string.format("Tree representation: %s", tree_to_string(sample_tree)))
print(string.format("Tree as sorted list: %s", table.concat(tree_to_list(sample_tree), ", ")))
print(string.format("Tree height: %d", tree_height(sample_tree)))
print(string.format("Node count: %d", tree_count(sample_tree)))

-- Test search
print(string.format("Contains 40? %s", tostring(tree_find(sample_tree, 40))))
print(string.format("Contains 55? %s", tostring(tree_find(sample_tree, 55))))
|# 