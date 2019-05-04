(*Implementing FL0 Interpreter*)

datatype 'a const=
            l
            |m
            |Z 
            | T 
            | F 
            | P of 'a const 
            | S of 'a const 
            | ITE of 'a const * 'a const * 'a const 
            | IZ of 'a const 
            | GTZ of 'a const 
            |lembda of 'a const* 'a const 
            | appl of 'a const * 'a const 



exception error 
fun fl t=
let
    
    fun pre (P(Z))=(P(Z))
        |pre (P(S(t)))= if fl(t)=T orelse fl(t)=F then raise error else fl(t)
        |pre (P(T)) = raise error
        |pre (P(F)) = raise error
        |pre (P(t))= if fl(t)=S(Z) then Z else (P(fl(t)))
       
    (*succesor of expression *)
    fun suc (S(Z))=(S(Z))
        |suc (S(P(t))) = if fl(t)=T orelse fl(t)=F then raise error else fl(t)
        |suc (S(T)) = raise error
        |suc (S(F)) = raise error
        |suc (S(t))= if fl(t)=P(Z) then Z else (S(fl(t))) 
    (*check ITE*)
    fun ite (ITE(f,x,y)) =  if fl(f) = T then fl(x) 
                            else fl(y) 
    (*check IZ*)
    fun iz (IZ(t)) = if fl(t) = Z then T 
            else F 
    
    fun check (S(t))=T
        |check(t)=F 
    (*check GTZ*)    
    fun gtz (GTZ(t)) = if check(fl(t))=T then T
             else F  
    
    
    fun application(lembda(x,t),M)=
            let fun reduction (lembda(k,L),M,y) = lembda(k,reduction(L,M,y))
                    |reduction(appl(K,L),M,y) = (appl(reduction(K,M,y),reduction(L,M,y)))
                    |reduction (P(t),M,y) = if t = y then P(fl(M))
                                else P(reduction(t,M,x))
                    |reduction (S(t),M,y)= if t = y then S(fl(M)) 
                                else S(reduction(t,M,x))
                    |reduction (IZ(t),M,y)=if t = y then IZ(fl(M)) 
                                else IZ(reduction(t,M,x))
                    |reduction (GTZ(t),M,y)=if t = y then GTZ(fl(M)) 
                                else GTZ(reduction(t,M,x)) 
                    |reduction (ITE(f,a,b),M,y)= ITE(reduction(f,M,y),reduction(a,M,y),reduction(b,M,y)) 
                    |reduction (t,M,x) = if t=x then fl(M) 
                                else t 
                val n=x
            in
                reduction(t,M,n)
            end 
        |application (appl(L,M),K)=fl(appl(application(L,K),application(M,K)))
        |application(L,M)=fl(L) 
    
    fun  nf Z = Z
        |nf T = T
        |nf F = F
        |nf (P(t)) = (pre(P(t)))
        |nf (S(t)) = (suc(S(t)))
        |nf (ITE(f,x,y)) = ite(ITE(f,x,y)) 
        |nf (IZ(t))=iz(IZ(t))
        |nf (GTZ(t))=gtz(GTZ(t)) 
        |nf (lembda(x,t))=lembda(x,t) 
        |nf (appl(L,M)) =fl(application(L,M))   
in 
    nf(t) 
end 


(*
val a=P(S(S(S(P(P(P(P(S(S(S(S(Z))))))))))));
*)






















































































