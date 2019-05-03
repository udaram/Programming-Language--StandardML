exception Error


(*  Function toString    *)
datatype 'a RE =Nil|Eps|Atom of 'a |Dot of 'a RE * 'a RE |
                Or of 'a RE * 'a RE |Star of 'a RE  

fun toString Nil = ""
    |toString Eps = "Eps"
    |toString (Star(a)) = toString(a)^"*" 
    |toString (Atom(a)) = a
    |toString (Dot(a,b)) = "("^toString(a)^"."^toString(b)^")"
    |toString (Or(a,b))= "("^toString(a)^"|"^toString(b)^")"



fun drop_hd(h::t)=t
fun drop(h::t)=t

(*Function from String*)
fun fromString a=
   let fun convert(h::[])=Atom(implode([h]))
        |convert(h::t) = if h= #")" then 
                            let fun default(h::t)=
                              if Char.isDigit(h) then  
                                 if hd(t)= #"|" then Or(convert([h]),convert(drop_hd(t))) 
                                 else if hd(t)= #"." then Dot(convert([h]),convert(drop_hd(t)))
                                 else convert(drop_hd(t))
                              else raise Error
                            in 
                            default(t)
                            end
                        else if h = #"(" then convert(t)
        (*orelse h= #")" then convert(t)*)
                         else if h= #"*" then Star(convert(t))
                         else (*Or(convert([h]),convert([h]))*)
                         if Char.isDigit(h) then 
                              if hd(t)= #"|" then Or(convert([h]),convert(drop_hd(t))) 
                              else if hd(t)= #"." then Dot(convert([h]),convert(drop_hd(t)))
                              else if hd(t)= #"*" then Star(convert(drop_hd(t)))
                              else convert(t)
                         else convert(t)
    
    val v = rev(drop(explode(a)))
    val z = drop(v)
   in
       convert(z) 
   end


(*Third and second part*)
fun delete_digit []=[]
    |delete_digit(h::t)=
        if Char.isDigit(h)=true then delete_digit(t)
        else h::t    

fun head_rep []=false
    |head_rep(h::t)=if h = #"+" orelse h = #"-" then true 
                    else false 

fun dfa a = 
    let fun states(ps,[],es)=ps
       |states(ps,(h::t),es)=
            if ps=0 then 
                 if h = #"+" orelse h = #"-" then 
                      if head_rep(t) then es
                      else states(ps,t,es)
                 else if Char.isDigit(h) then states(ps+1,delete_digit(t),es)
                 else es
            else if ps=1 then 
                 if h = #"." then states(ps+1,t,es)
                 else es 
            else if ps=2 then 
                if Char.isDigit(h) then states(ps+1,delete_digit(t),es)
                else es
            else if ps =3 then 
                 if h = #"E" then states(ps+1,t,es)
                 else es
            else if ps=4 then 
                 if h = #"+" orelse h = #"-" then 
                      if head_rep(t) then es
                      else states(ps,t,es)
                 else if Char.isDigit(h) then states(ps+1,delete_digit(t),es)
                 else es 
            else es
    in 
    states(0,explode(a),6)
    end;

datatype class = INT | FIXED | FLOAT
datatype 'class option =NONE | SOME of 'class 

fun accept(a)=
    let 
    val x=dfa(a)
    in
    if x=1 then SOME INT
    else if x=3 then SOME FIXED
    else if x=5 then SOME FLOAT
    else NONE
    end




