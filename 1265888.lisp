;Name:Kieran Boyle
;Student Number: 1265888
;Course: CMPUT 325
;Lecture Section: B1
;Lab Section: HO1
;Assignment Number: 2

; fl-interp is the main function for my assignment an fl function that is translated
; and evaluated in lisp. The values that it takes in are are a expression and a program
; an expression consists both a function and its arguments. The program consists of 
; nil, a program or several programs. 
;
;First I split the expression into the program name and the  arguments. I then check
;the program name against a number of primitive functions (who are much the same as 
;in lisp). I check if the function is user defined by dearching through the program 
;list to see if it is defined if so I evaluatee the arguments and send send the function to
;my  user-defined funtion.


;examples
;    (fl-interp '(f (f 2)) '( (f X =  (* X X)) ))
;           =>  16
;
;   (fl-interp '(+ 1 2) nil)
;           => 3
;
;   not(fl-interp '(atom (3))
;           => NIL
;
;   (fl-interp '(pop (1 2 3)) '((pop x = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x)(pop (rest x)))))))
;           =>(1 2)





(defun fl-interp (E P)
  (cond 
	((atom E) E)   ;%this includes the case where expr is nil
        (t
           (let* ( (f (car E))  (arg (cdr E)) (iffprog (findProgram f (argCount arg) p)))
	      (cond 
                ; handle built-in functions
               
                ((eq f 'atom) (atom (fl-interp (car arg) P)))
                ((eq f 'number) (numberp (fl-interp (car arg) P)))
                ((eq f 'first)  (car (fl-interp (car arg) P)))
                ((eq f 'rest) (cdr (fl-interp (car arg) P)))
                ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
                ((eq f 'not) (not (fl-interp (car arg) P)))
                ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
                ((eq f 'null) (null (fl-interp (car arg) P)))

	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 
                ;((not (null P)) (user-defined  f (evalArgs f arg P) (findProgram f (argcount arg) P )))
                ((not (null iffprog)) (user-defined  f (evalArgs f arg P) P))
                ; otherwise f is undefined; in this case,
                ; E is returned as if it is quoted in lisp

                (t E))))))









;This function evaluates all of the arguments given within the program. It is a recursive
;definition that checks if there are arguments. If there is it runs fl-interp on the nested
;arguments. 
;
;examples:
;
;   (evalArgs 'a ((+ 1 3))'( (a X = (+ X 1)) )) 
;       => (4)
 
(defun evalArgs (f args p)
    (cond 
        ((null args) nil)
        (t (cons (fl-interp (car args) p) (evalArgs f (cdr args) p))))
    )



    


;Tis function simply calculates the number of values within a list. THis is 
;particularly useful whenever you are trying to identify which progrma is which.
;
;examples:

;        (argcount (1 2 3 4 5 6))
;               => 6
;
;          (argcount (a b c))
;               => 3
;
(defun argCount (args)
    (cond 
        ((null args)0)
        (t (+ 1 (argCount (cdr args))))
        )
    )






;This function gets the implimentation from the
;program. It does this by selecting everything after the occurence of the
; "=" sign.
;example:

;   (prog x y = (+ x y))
;          => (+x y)
(defun getProgram (p)
    (if (eq(car p) '=)
        (cdr p)
        (getProgram (cdr p))
        )
    )


;This function gets the paramaters of the program.
;to do this it just takes every value before a the "="
;sign and puts them in a list. 
;examples:
;      (x y = (+ x y))
;          => (x y)

(defun getParams (p)
    (if (eq (car p) '=)
        nil
        (cons (car p) (getParams(cdr p)))
        )

    )

;This functions substitutes a given paramater for an argument given.
;It does this by parsing through the function until it hits a match,
;once a match is made the argument is cons'd back in insteade of the 
;paramater. once the function is completely empty then you return 
;it with that paramater subbed back in. 
;
;example:
;      (sub1 'A '1 '(+ A (- A B)))
;           => (+ 1 (- 1 B))

(defun sub1 (param arg function)

    (cond 
        ((null function) 
        function)
        ((eq (car function) param)
            (cons arg (sub1 param arg (cdr function))))

        ((not (atom (car function)))
           (cons (sub1 param arg (car function)) (sub1 param arg (cdr function)) ))
        
        (t (cons (car function) (sub1 param arg (cdr function))))
        )
    )



;This function substitutes all of the paramters with the arguments for the function.
;It takes in a list of paramaters, a list of arguments and the program that you wish
;to sub them into. The program just goes through these lists calling sub1 on 
;every paramater argument match. A completely replaced funtion is returned. 
;example:
;   (subAll '(X Y) '(1 2) '(+ X Y))
;        => (+ 1 2)

(defun subAll(params arguments function)
    (cond 
        ((null arguments) function)
        ((null params) function)

        (t (subAll(cdr params) (cdr arguments) (sub1 (car params) (car arguments) function)))

    )
)


;finds the correct program based on the function name and the arity
;It does thios by going through every program in the list of programs
;checking if there is a name match and if the arity matches. If the arity
;matches and the function name matches than that function in the program list
;is returned. 
;
;example:

;   findProgram 'prog 2 '((neat x =  (+XX)) (prog x y = (+ x y)) (prog x y z= (-z(+ x y))))
;           => (prog x y = (+xy))

(defun findProgram (f numArgs p)
    (cond 
        ((null p) nil)
        ((eq f (car (car p)))
            (if (eq numArgs (argCount (getParams (cdr (car p)))))
                (car p)
                (findProgram f numArgs (cdr p))
                )
            )
        (t (findProgram f numArgs (cdr p)))
        )
    )
;This function takes in the program name, arguments and program part of the input.
;I simply run fl-interp on the found program with the correct arguments substituted, the current progam 
;The function is then recombined to make it readable by fl-interp. Essentially all this function does
; is run fl-interp on the new function that I make from al;l the other subfunctions ccombined
;
;Refer to the examples for fl-interp in order to see what the return of user-defined is.
;

(defun user-defined (f args p)

    (fl-interp (car (subAll (getParams (cdr (findProgram f (argCount args) p)))  args  (getProgram (findProgram f (argcount args)P)))) p)


    )

