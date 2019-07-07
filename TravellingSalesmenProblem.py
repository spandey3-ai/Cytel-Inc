import math
import random
# These are the planar coordinates for 14 villages in Burma
#coord = [ [16.47,96.10], [16.47,94.44], [20.09,92.54],
 #         [ 22.39,93.37], [25.23,97.24], [22.00,96.05],
  ##       [14.05,98.12], [16.53,97.38], [21.52,95.59],
    #      [19.41,97.13], [20.09,94.55] ]

def randRestart2opt(C):
    
# One approach is to build a matrix as given below and then fill it up
#my_matrix = [ [ None for x in range( len( coord ) ) ] for x in range( len( coord ) ) ]

# The other approach is to build the matrix with correct numbers, adding them one by one
    def distances (C):
        my_matrix=[]
        for city_from in range( len( C ) ):
            my_matrix.append( [] )
            for city_to in range( len( C ) ):
                my_matrix[ city_from ].append( math.sqrt( (C[city_to][0] - C[city_from][0]) ** 2 + (C[city_to][1] - C[city_from][1]) ** 2) )
        return my_matrix
# code that returns the objective function of TSP - a tour's length
    def obj( tour,C ):
        my_matrix=distances(C)       
        tour_length = 0
        for i in range( len( tour ) - 1 ):
            tour_length = tour_length + my_matrix[ tour[ i ] ][ tour[ i+1 ] ]
        return tour_length + my_matrix[ tour[ len(tour) - 1 ] ][ tour[ 0 ] ]

# example of using the objective function
#print obj( [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ] )

# This function gets all the permutations of a given list
# In order to convert its output into a list, write
# permuted_list = list( all_perms( list_to_be_permuted ) )
    def all_perms( elements ):
        if len( elements ) <= 1:
            yield elements
        else:
            for perm in all_perms( elements[ 1: ] ):
                for i in range( len( elements ) ):
                    yield perm[ :i ] + elements[ 0:1 ] + perm[ i: ]

# Example of use of the all_perms() function:
#my_list = [ 0, 1, 2, 3, 4,5,6,7]
#permuted_list = list( all_perms( my_list ) )
#print "permuted list is: " , permuted_list

#print obj(permuted_list[1])


# This function returns an improving 2- opt neighbor of the current solution
# if neither neighbor is improving, it returns an empty list


    def improve_tour(current,C):
        b1=0
        b2=0
        for b1 in range (0,len(current)-2):
            for b2 in range(b1+2, len(current)):
                candidate=list(current)            
                candidate[b1:b2]=list(reversed(candidate[b1:b2]))
                delta=obj(candidate,C) -obj(current,C)
                if delta<=0:
                    return(candidate)
            
    
    a=[]
    for i in range(0,len(C)):
            a.append(i)
     
    #print obj(a)

    #b=improve_tour(a)
    #print obj(b)


    best_solution=a
    best_objective=obj(a,C)
# A local search algorithm using 2-opt begins here
# with R random restart
#initial=[0,1,2,3,4,5,6,7,8,9,10,11,12,13]
    R=30
    for iteration in range(1,R) :
        random.shuffle(a)
        current=a
        continue_the_search=1
        while continue_the_search==1:
            result=improve_tour(current,C)
            if True:   # or we can write - if not result:
                continue_the_search=0
            else:
                current=result
                ##print 'current best solution=' , current,'objective=' , obj(current)
            
        if obj(current,C) < obj(best_solution,C):
            best_solution=list(current)
            best_objective=obj(current,C)
    
    #print 'best solution=', best_solution,'objective=',   best_objective
    return best_solution, best_objective
    
    
    
coord = [ [16.47,96.10], [16.47,94.44], [20.09,92.54],
         [ 22.39,93.37], [25.23,97.24], [22.00,96.05] ]   
print  randRestart2opt(coord)        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    