-- Module: example
-- This is the compiled output from binary-tree.rkt

-- Define tree node structure
local function tree_node(value, left, right)
  return {
    value = value,
    left = left,
    right = right
  }
end

local function tree_node_value(node)
  return node.value
end

local function tree_node_left(node)
  return node.left
end

local function tree_node_right(node)
  return node.right
end

-- Define empty tree
local empty_tree = false

-- Check if tree is empty
local function tree_empty(tree)
  return tree == false
end

-- Create a leaf node (a node with no children)
local function make_leaf(value)
  return tree_node(value, false, false)
end

-- Insert a value into a tree
local function tree_insert(tree, value)
  if tree_empty(tree) then
    return make_leaf(value)
  elseif value < tree_node_value(tree) then
    return tree_node(
      tree_node_value(tree), 
      tree_insert(tree_node_left(tree), value), 
      tree_node_right(tree)
    )
  elseif value > tree_node_value(tree) then
    return tree_node(
      tree_node_value(tree), 
      tree_node_left(tree), 
      tree_insert(tree_node_right(tree), value)
    )
  else
    -- Value already exists, return tree unchanged
    return tree
  end
end

-- Find a value in a tree
local function tree_find(tree, value)
  if tree_empty(tree) then
    return false
  elseif value == tree_node_value(tree) then
    return true
  elseif value < tree_node_value(tree) then
    return tree_find(tree_node_left(tree), value)
  else
    return tree_find(tree_node_right(tree), value)
  end
end

-- Convert tree to sorted list
local function tree_to_list(tree)
  if tree_empty(tree) then
    return {}
  else
    local left_list = tree_to_list(tree_node_left(tree))
    local right_list = tree_to_list(tree_node_right(tree))
    
    -- Combine lists: left + current value + right
    local result = left_list
    table.insert(result, tree_node_value(tree))
    for _, v in ipairs(right_list) do
      table.insert(result, v)
    end
    
    return result
  end
end

-- Calculate tree height
local function tree_height(tree)
  if tree_empty(tree) then
    return 0
  else
    local left_height = tree_height(tree_node_left(tree))
    local right_height = tree_height(tree_node_right(tree))
    return 1 + math.max(left_height, right_height)
  end
end

-- Count nodes in a tree
local function tree_count(tree)
  if tree_empty(tree) then
    return 0
  else
    return 1 + tree_count(tree_node_left(tree)) + tree_count(tree_node_right(tree))
  end
end

-- Convert tree to string representation
local function tree_to_string(tree)
  if tree == false then
    return "()"
  elseif tree_node_left(tree) == false and tree_node_right(tree) == false then
    return string.format("(%s)", tree_node_value(tree))
  elseif tree_node_right(tree) == false then
    return string.format("(%s %s)", tree_node_value(tree), tree_to_string(tree_node_left(tree)))
  elseif tree_node_left(tree) == false then
    return string.format("(%s _ %s)", tree_node_value(tree), tree_to_string(tree_node_right(tree)))
  else
    return string.format("(%s %s %s)", tree_node_value(tree), tree_to_string(tree_node_left(tree)), tree_to_string(tree_node_right(tree)))
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

-- Create and test the sample tree
local sample_tree = build_sample_tree()

print(string.format("Tree representation: %s", tree_to_string(sample_tree)))
print(string.format("Tree as sorted list: %s", table.concat(tree_to_list(sample_tree), ", ")))
print(string.format("Tree height: %d", tree_height(sample_tree)))
print(string.format("Node count: %d", tree_count(sample_tree)))
print(string.format("Contains 40? %s", tostring(tree_find(sample_tree, 40))))
print(string.format("Contains 55? %s", tostring(tree_find(sample_tree, 55))))