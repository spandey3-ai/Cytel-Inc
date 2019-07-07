
# Define Item as a namedtuple
from collections import namedtuple
Item = namedtuple("Item", ['index', 'value', 'weight', 'density'])

# A knapsack problem solver using Branch and Bound:
# To find an upper bound and prune sub-trees that will never give an optimal solution
# one can relax the requirement of x_i being 0 or 1 and let it take any fractional value.
# The maximum value that can now be packed in the knapsack can be computed by:
def get_best_estimate(items_sorted, capacity, selections):
# to start the calculations , set initial values of value(profit) and weight to zero    
    value = 0
    weight = 0
# moving through the sorted list in terms of its density.
   
    for idx in xrange(len(items_sorted)):
        if idx < len(selections) and selections[idx] == 0: continue
# the idx index of the item_sorted list is considered          
        i = items_sorted[idx]
#it is checked if the weight of that particulat item fits ito the capacity of knapsack.
# The highest density item is selected and checked if there is further room...
# ...for the next best density item. if yes then fraction of the weight will be 
# considered for knapsack depending on the remaining capacity
# and accordingly fraction of value is taken
    
        if weight + i.weight > capacity:
            value += i.value * (((capacity - weight) * 1.0) / i.weight) 
            break
# value and weight are updated as per the item that was considered 
        value += i.value
        weight += i.weight
# cumulative value which the knapsack can generate is returned. 
    return value

# Every item is available for selection unless there is a 0 value entry
# in the selections list passed to the above method. 

# So now that we have a method to estimate the best value that
# a sub-tree can achieve, we can prune sub-trees as follows:
#    1. If the available capacity of this node is negative then
#       we need not look at any nodes in the sub-tree. Also,
#       this node cannot be a solution because we have already
#       exceeded the knapsack’s capacity.
#    2. If the best estimate for this node is less than the best
#       solution found so far we can ignore rest of the sub-tree.

# A node can be defined as a class:
class Node():
# Class is created above with an object Node, Node has following characteristics that define it-    
    best_value = 0
    best_selections = []
    capacity = 0
    items_sorted = []
# By __init__  Python creates an object for you.
# When you call the class the value is  passes as the first parameter to the __init__  
    def __init__(self, value, room, selections, previous_estimate):
# In case of the tree branching when the class will be called, the arguments will be passed to th _init_
# function. So a certain node's value will be passes into self which is basically a starting point and has
# two succeding values in terms of left and rght child.The child will later  on become the self and have its
# own left and right child.        
        self.value = value
        self.room = room
        self.estimate = previous_estimate if previous_estimate != -1 else get_best_estimate(Node.items_sorted, Node.capacity, selections)
        self.selections = selections
        self.index = len(self.selections)
# the left child will get the values of self as its argument.        
    def get_left_child(self):
        item = Node.items_sorted[self.index]
# it will consider the index of the self and returns the following.
# self.value + item.value will give updated value for value, remining room, selection which is 1 and estimate
# these values of room and estimate are used for later comparison of feasibility of the node and also for 
#further branchin.
        return Node(self.value + item.value, self.room - item.weight, self.selections + [1], self.estimate)
# the right child will get the values of self as its argument.       
    def get_right_child(self):
        item = Node.items_sorted[self.index]
# it will consider the index of the self and returns the following.
# as this node is for not selecting that particular item the parameters dont get updated with new values
# and selection has [0] corresponding to that item.        
        return Node(self.value, self.room, self.selections + [0], -1)
# This is for the last node of the tree in which the argument self is passed
# the index of the self gives the last item from the item_sorted list
    def is_leaf(self):
        return self.index == len(Node.items_sorted)

# Also a recursion is avoided by using a stack to store all nodes
# that are to be processed. In every iteration it is checked if
# one can prune the node or if one has already found a solution.
# Otherwise create a left and a right child, add the children to
# the stack and remove the node from the stack. 
def branch_and_bound(item_count, capacity, items):
#    item_count, capacity, items are passed as argument to the function branch_and_bound
# initializing the values to zero or empty list or according to the argument passed to function.
    Node.best_value = 0
    Node.best_selections = []
    Node.capacity = capacity
# the list is sorted according to density and order is reversed to get best density first    
    Node.items_sorted = sorted(items, key = lambda i: i.density, reverse = True)
# Class is used here.
# the object in class is give the following values and root gets the value of self.
# now self has become root from which left and right child will be derived    
    root = Node(0, capacity, [], -1)
#  STore left and rright child of the root in the stack    
    stack = [root.get_right_child(), root.get_left_child()]
# till stack become empty run the following loop:    
    while stack:
# from stack, select the best value and make it node(self or root for further derivations)       
        node = max(stack, key = lambda i: i.value)
        # remove that value put in node form the stack list
        stack.remove(node)
 # checking if that selected item as node exceeds capacity of knapsack 
 # or whether its estimate is lower than the best estimate previously calculated       
        if node.room < 0 or node.estimate < Node.best_value: continue
# check if the node we reached is the leaf or end node of the tree        
        if node.is_leaf() and node.value > Node.best_value:
# if we are at leaf, and the value of that node is better than previois best value,
# then update the previous best value with the one from this node and change selection             
            Node.best_value = node.value
            Node.best_selections = node.selections
            continue
#if the node is not a leaf node thn append it's right and left child into stack        
        if not node.is_leaf():
            stack.append(node.get_left_child())
            stack.append(node.get_right_child())
# selections is a list of zeroes with length= length of best_selections    
    selections = [0] * len(items)
# for loop upto the legth of best_selections
    for idx in xrange(len(Node.best_selections)):
# if in the best selction,the idx'th index is selected ie it is 1,
    # then item in selection will change to 1 for index for that idx'th item in item_sorted  
    #-1 is because the .index has started from 1 and indexing in list will start from 0
        if Node.best_selections[idx] == 1: selections[Node.items_sorted[idx].index-1] = 1
# return the final optimal selection of items
    return selections

# there are 8 items available
item_count = 8
# capasity of knapsack is 110 which cannot be exceeded
capacity = 110
# list of item number along with its value and weight
problem_items = [ [1,11,1], [2,21,11], [3,31,21], [4,33,23], [5,43,33], [6,53,43], [7,55,45], [8,65,55] ]
# list of namedtuple is formed
items = []
# representing every value in the problem_items list in terms of-  index,value,weight, density
for i in problem_items:
    items.append( Item( i[0], i[1], i[2], float(i[1]) / float(i[2]) ) )
print items
print branch_and_bound(item_count, capacity, items)   























