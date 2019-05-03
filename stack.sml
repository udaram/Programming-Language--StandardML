

datatype 'a stack = Empty 
        |Node of 'a * 'a stack 

fun isEmpty Empty=true
    |isEmpty _ =false

fun push(top,ele)=(Node(ele,top))

fun value(Node(a,_))=a

fun pop(Node(a,b))= b

fun into_post(a)=
    let fun create([],p,top)=if isEmpty(top) then p 
            else 
            create([],value(top)::p,pop(top))
        |create(h::t,p,top)=
        if Char.isDigit(h) then create(t,h::p,top)
        else 
            if h = #")" then 
               let fun pop_from_stack(top,p)=if value(top)=#"(" then (p,top)
                                            else pop_from_stack(pop(top),value(top)::p)
                   val v=pop_from_stack(top,p)
                in 
                  create(t,v,top)    
                end
            else push(top,h)
    val x=explode(a)
    val top=Node(Empty)
    in
        create(x,[],Empty)
    end


(*
fun Print(top)=
    if isEmpty(top)=true then print("Empty")
    else
       print(value(top));
       Print((pop(top))    
 *)
(*
val t=Empty;
val t=push(t,"1");
val t=push(t,"2");
val t=push(t,"3");
val t=push(t,"4");
val t=push(t,"2");



(*a is 'a list*)
fun postfix(a)=
    let fun conv_post([],l,s)=push_into_list(l,s)
        |conv_post(h::t,l,s)=
         if Char.isDigit(h) then conv_post(t,l@h,s)
         else if h = #")" then conv_post(t,l@pop_list(rev(s)))

    in
    conv_post(a,[],[])
    end;
*)
   
