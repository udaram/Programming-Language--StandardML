

exception NotUnsignedIntegers

(*to make strings of equal length*)
fun make_equalstring(x,y) =
    if length(x) = length(y) then (x,y)
    else if length(x)<length(y) then make_equalstring([#"0"]@x,y)
        
    else make_equalstring(x,[#"0"]@y)  
    
(*For Addition*)
fun sum(r,x::[],y::[],carry)=(chr((((ord(x)-48)+(ord(y)-ord(#"0"))+carry) mod 10)+48)::r,((ord(x)-ord(#"0"))+(ord(y)-ord(#"0"))+carry) div 10)
    |sum(r,x::xx,y::yy,carry)=
    sum(chr((((ord(x)-48)+(ord(y)-ord(#"0"))+carry) mod 10)+48)::r,xx,yy,((ord(x)-ord(#"0"))+(ord(y)-ord(#"0"))+carry) div 10)
    
    
 
(* subtraction *)        
fun subtract(r,x::[],y::[],carry)= 
        if ord(x)+carry<ord(y) then (chr((((ord(x)-48)+10-(ord(y)-ord(#"0"))+carry))+48)::r,~1)
        else (chr((((ord(x)-48)-(ord(y)-ord(#"0"))+carry))+48)::r,0)
    |subtract(r,x::xx,y::yy,carry)=
       if ord(x)+carry<ord(y) then subtract(chr((((ord(x)-48)+10-(ord(y)-ord(#"0"))+carry))+48)::r,xx,yy,~1) 
       else subtract(chr((((ord(x)-48)-(ord(y)-ord(#"0"))+carry))+48)::r,xx,yy,0)
       

(*to check the sign *)      
fun negsign(x,y)=
    if implode(x)<implode(y) then true
    else false
    
(*Add zeros to string *)
fun add_zeros(x,m)=
    if m=0 then x
    else add_zeros(x@[#"0"],m-1)
    


(*Check Validity of x,y    not working*)
fun validity(h::[])=Char.isDigit(h)
    | validity(h::x)=
       if Char.isDigit(h) then validity(x)
       else false


fun calculate(x,y)=
    if (ord(hd(explode(x)))-48)*(ord(hd(explode(y)))-48) >9 then [chr(((ord(hd(explode(x)))-48)*(ord(hd(explode(y)))-48))div 10 +48),chr(((ord(hd(explode(x)))-48)*(ord(hd(explode(y)))-48))mod 10 +48)]
    else 
    [chr((ord(hd(explode(x)))-48)*(ord(hd(explode(y)))-48)+48)]
 (* Main function Karatsuba *)
 fun karatsuba(x,y)= 
      let 
          fun calculate_z1(a1,a0,b1,b0,z0,z2)=
              let 
                 fun subtract_cal(v0,v1)=
                    let
                       val (xx0,xx1)=make_equalstring(explode(v0),explode(v1))
                    in 
                       if negsign(xx0,xx1) then (subtract([],rev(xx1),rev(xx0),0),true)
                       else (subtract([],xx0,xx1,0),false)
                    end 
              val ((x0_x1,_),signx) = subtract_cal(a0,a1)
              val ((y0_y1,_),signy) = subtract_cal(b1,b0)
              val zz = karatsuba(implode(x0_x1),implode(y0_y1))
              val (zz0,zz2) = make_equalstring(z0,z2)
              val (z02,carry) = sum([],rev(zz0),rev(zz2),0)
              (*updated sum of z0 + z2 *)
              val zz02 = chr(carry+48)::z02 
              
              val (z_02,zz1) = make_equalstring(zz02,zz)
              
              in  
                 if (signx=true andalso signy=true ) orelse (signx=false andalso signy=false)  then sum([],rev(z_02),rev(zz1),0)
                 else if negsign(z_02,zz1) then subtract([],rev(z_02),rev(zz1),0)
                 else subtract([],rev(zz1),rev(z_02),0)      
              end
   
    
         fun karatsuba_cal(x,y) = 
            let         
                   val (a,b)=make_equalstring(explode(x),explode(y))
                   val m=length(a) 
                   val a1=substring(implode(a),0,m div 2)
                   val a0=substring(implode(a),m div 2,m-(m div 2))
                   val b1=substring(implode(b),0,m div 2)
                   val b0=substring(implode(b),m div 2,m-(m div 2))
                        
                   val z2 = karatsuba(a1,b1)
                   val z0 = karatsuba(a0,b0)
                   val (z_1,carry) = calculate_z1(a1,a0,b1,b0,z0,z2) 
                   val z1 = chr(carry+48)::z_1
                        
                     (*z2B2m +z1Bm + z0 *)
                   val zz2 = add_zeros(z2,2*m)
                   val zz1 = add_zeros(z1,m)
                        
                        
                   val (z_2,z11) = make_equalstring(zz2,zz1)
                        
                   val (res,carry1) = sum([],rev(z_2),rev(z11),0)
                   val result = chr(carry1+48)::res
                        
                   val (res1,z00) = make_equalstring(result,z0)
                        
                   val (ans,carry2) = sum([],rev(z_2),rev(z11),0)
                     (* Final Answer *)
                   val xy = chr(carry2+48)::ans   
             in 
                   xy
             end   
      in 
         if validity(explode(x)) = false orelse validity(explode(y)) = false then raise NotUnsignedIntegers 
         else if size(x)=1 then calculate(x,y)
         else (karatsuba_cal(x,y))    
    end
   
   
   
    
    
   
    
    
    
    
    
    
    
    
   
