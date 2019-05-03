datatype 'a bintree =
        Empty | Node  of 'a * 'a bintree * 'a bintree 

exception Empty_tree

fun isEmpty Empty = true
    | isEmpty _=false

fun subtree Empty =raise Empty_tree
    | subtree (Node(N,Lst,Rst)) = (Lst,Rst)

fun root Empty = raise Empty_tree
    | root (Node(N,_,_) )= N


fun leftsubtree Empty = raise Empty_tree
    | leftsubtree (Node(_,Lst,_) )= Lst


fun rightsubtree Empty = raise Empty_tree
    | rightsubtree (Node(_,_,Rst) )= Rst

fun height Empty = 0
    |height(Node(N,Lst,Rst)) = 1+ Int.max(height(Lst),height(Rst))

fun isBalanced Empty = true
    | isBalanced (Node(N,Lst,Rst)) = 
      (abs(height(Lst)-height(Rst))<=1) andalso isBalanced(Lst) andalso isBalanced(Rst)

fun size Empty = 0
    | size (Node(N,Lst,Rst)) = 1+ size(Lst) + size(Rst)

fun preorder Empty = nil
    |preorder(Node(N,Lst,Rst)) = [N]@preorder(Lst)@preorder(Rst)

fun inorder Empty = nil
    |inorder(Node(N,Lst,Rst)) = inorder(Lst)@[N]@inorder(Rst)

fun postorder Empty = nil
    |postorder(Node(N,Lst,Rst)) = postorder(Lst)@postorder(Rst)@[N]

fun BTmap f = 
    let
       fun BTM Empty = Empty 
       | BTM (Node(N,Lst,Rst)) = 
         Node((f N),BTM(Lst),BTM(Rst))
    in
      BTM
    end

val t7 = Node(7,Empty,Empty);
val t6 = Node(6,Empty,Empty);
val t4 = Node(4,Empty,Empty);
val t2 = Node(2,Empty,t4);
val t5 = Node(5,t6,Empty);
val t3 = Node(3,t5,t7);
val t1 = Node(1,t2,t3);