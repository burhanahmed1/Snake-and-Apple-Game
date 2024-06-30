[org 0x0100] 
jmp start 

;for snake speed
delay1:
    push cx
	push dx
    mov cx,[delay1var1] ;
    loop1:
        ; Inner loop for 10000 cycles
        mov dx,[delay1var2]
        nestedloop:
            dec dx
            jnz nestedloop
        dec cx
        jnz loop1
   pop dx
   pop cx
 ret


;for word snake game and instructions
 delay2:
   push cx
   push dx
   mov cx,100 ;
   loop2:
        ; Inner loop for 10000 cycles
        mov dx,200
        nestedloop2:
            dec dx
            jnz nestedloop2
        dec cx
        jnz loop2
   pop dx
   pop cx
 ret

 delay3:
   push cx
   push dx
   mov cx,500 ;
   loop3:
        ; Inner loop for 10000 cycles
        mov dx,500
        nestedloop3:
            dec dx
            jnz nestedloop3
        dec cx
        jnz loop3
   pop dx
   pop cx
 ret



play_sound:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	mov al, 182
	out 43h, al
	mov ax,3560                  ;[bp+4]	move the passed frequency

	out 42h, al
	mov al, ah
	out 42h, al
	in al, 61h

	or al, 00000011b
	out 61h, al
	mov bx, 25
	pause1:
    mov cx,2000                 ; [bp+6]
    pause2:
    dec cx
    jne pause2
    dec bx
    jne pause1
    in al, 61h
    and al, 11111100b
    out 61h, al

	pop cx
	pop bx
	pop ax
	pop bp
	ret 


	play_sound1:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	mov al, 182
	out 43h, al
	mov ax,3560                  ;[bp+4]	move the passed frequency

	out 42h, al
	mov al, ah
	out 42h, al
	in al, 61h

	or al, 00000011b
	out 61h, al
	mov bx, 25
	pause3:
    mov cx,100                 ; [bp+6]
    pause4:
    dec cx
    jne pause4
    dec bx
    jne pause3
    in al, 61h
    and al, 11111100b
    out 61h, al

	pop cx
	pop bx
	pop ax
	pop bp
	ret 

; subroutine to clear the screen for frontpage and backpage
clrscr1: 
 push es 
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 xor di, di ; point di to top left column 
 mov ax,0x0020 ; space char in normal attribute 
 mov cx, 2000 ; number of screen locations 
 cld ; auto increment mode 
 rep stosw ; clear the whole screen 
 pop di
 pop cx 
 pop ax 
 pop es 
 ret 

; subroutine to clear the screen for game
clrscr2: 
 push es 
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 xor di, di ; point di to top left column 
 mov ax,0x7020 ; space char in normal attribute 
 mov cx, 2000 ; number of screen locations 
 cld ; auto increment mode 
 rep stosw ; clear the whole screen 
 pop di
 pop cx 
 pop ax 
 pop es 
 ret 

printhor:                       ;Subroutine for printing horizontal borders
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax               ;  point es to video base 
 mov al, 80               ; load al with columns per row 
 mul byte [bp+10]    ; multiply with y position 
 add ax, [bp+12]      ; add x position 
 shl ax, 1                  ; turn into byte offset 
 mov di, ax                ; point di to required location 
 mov al, [bp+6]       ; mov space in al 
 mov cx, [bp+4]       ; load length of string in cx 
 mov ah, [bp+8]       ; load attribute in ah 
next:
 mov [es:di], ax         ; show this char on screen 
 add di, 2                  ; move to next screen location      
 loop next           ; repeat the operation cx times 
 pop di 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 10

printver:                   ;subroutine for Printing Vertical Borders
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax               ;  point es to video base 
 mov al, 80               ; load al with columns per row 
 mul byte [bp+10]    ; multiply with y position 
 add ax, [bp+12]      ; add x position 
 shl ax, 1                  ; turn into byte offset 
 mov di, ax                ; point di to required location 
 mov al, [bp+6]        ; mov space in al 
 mov cx, [bp+4]       ; load length of string in cx 
 mov ah, [bp+8]       ; load attribute in ah 
next1:
 mov [es:di], ax         ; show this char on screen 
 add di, 160                 ; move to next screen location      
 loop next1                   ; repeat the operation cx times 
 pop di 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 10

printstr:
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 mov ax, 0xb800 
 mov es, ax            ; point es to video base 
 mov al, 80            ; load al with columns per row 
 mul byte [bp+10]      ; multiply with y position 
 add ax, [bp+12]       ; add x position 
 shl ax, 1             ; turn into byte offset 
 mov di,ax             ; point di to required location 
 mov si, [bp+6]        ; point si to string 
 mov cx, [bp+4]        ; load length of string in cx 
 mov ah, [bp+8]        ; load attribute in ah 
nextstrchar: 
 mov al, [si]          ; load next char of string 
 mov [es:di], ax       ; show this char on screen 
 add di, 2
 add si, 1             ; move to next char in string 
 loop nextstrchar         ; repeat the operation cx times 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 10 

 printstr1:
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 mov ax, 0xb800 
 mov es, ax            ; point es to video base 
 mov al, 80            ; load al with columns per row 
 mul byte [bp+10]      ; multiply with y position 
 add ax, [bp+12]       ; add x position 
 shl ax, 1             ; turn into byte offset 
 mov di,ax             ; point di to required location 
 mov si, [bp+6]        ; point si to string 
 mov cx, [bp+4]        ; load length of string in cx 
 mov ah, [bp+8]        ; load attribute in ah 
nextmsgch: 
 mov al, [si]          ; load next char of string 
 mov [es:di], ax       ; show this char on screen 
 add di, 2
 add si, 1             ; move to next char in string
 call delay2
 loop nextmsgch         ; repeat the operation cx times 
 pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 10 

print_score: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax             ;   point es to video base 
 mov ax, [Score]         ; load number in ax 
 mov bx, 10             ; use base 10 for division 
 mov cx, 0              ; initialize count of digits 
nextdigit: 
 mov dx, 0              ; zero upper half of dividend 
 div bx                 ; divide by 10 
 add dl, 0x30           ; convert digit into ascii value 
 push dx                ; save ascii value on stack 
 inc cx                 ; increment count of values 
 cmp ax, 0              ; is the quotient zero 
 jnz nextdigit          
 mov di,248
 nextpos: pop dx        ; remove a digit from the stack 
 mov dh, 0x09           ; use normal attribute 
 mov [es:di], dx        ; print char on screen 
 add di, 2              ; move to next screen location 
 loop nextpos           ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 

 print_score1: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax             ;   point es to video base 
 mov ax, [Score]         ; load number in ax 
 mov bx, 10             ; use base 10 for division 
 mov cx, 0              ; initialize count of digits 
nextdigit1: 
 mov dx, 0              ; zero upper half of dividend 
 div bx                 ; divide by 10 
 add dl, 0x30           ; convert digit into ascii value 
 push dx                ; save ascii value on stack 
 inc cx                 ; increment count of values 
 cmp ax, 0              ; is the quotient zero 
 jnz nextdigit1          
 mov di,1848
 nextpos1: 
 pop dx               ; remove a digit from the stack 
 mov dh, 0x09           ; use normal attribute 
 mov [es:di], dx        ; print char on screen 
 add di, 2              ; move to next screen location 
 loop nextpos1          ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 



snake_grow:
 push ax
 push bx
 push si
 mov si,[snakelength]
 shl si,1             ;multiply by 2 as array of snake is of 2 bytes
 sub si,2
 mov ax,[snakepos+si]
 sub si,2
 mov bx,[snakepos+si]
 add si,6
 sub bx,ax
 add ax,bx
 mov [snakepos+si],ax
 mov [tailpos],ax
 mov bx,[snakelength]
 add bx,1
 mov [snakelength],bx
 pop si
 pop bx
 pop ax
 ret



print_snake:
 mov ax,0xb800
 mov es,ax
 mov ax,0x7020
 mov di,[tailpos]
 mov [es:di],ax
 mov si,0
 mov ax,[head]              ;storing attribute and character of head
 mov cx,[snakelength]
 mov di,[snakepos+si]
 mov [es:di],ax             ;prints head
 sub cx,1
 add si,2
 mov ax,[body]              ;prints body
 body_loop:
    mov di,[snakepos+si]
	mov [es:di],ax
    add si,2
	loop body_loop
    mov [tailpos],di
 ret


 change_pos_left:             ;swap indexes and changes head
  mov si,[snakelength]
  add si,[snakelength]
  sub si,2
  mov cx,[snakelength]
  sub cx,1
  loop_pos:
  mov ax,[snakepos+si-2]
  mov [snakepos+si],ax
  sub si,2
  loop loop_pos
  mov ax,[snakepos+0]
  sub ax,2
  mov [snakepos+0],ax
  ret

change_pos_right:                ;swap indexes and changes head
  mov si,[snakelength]
  add si,[snakelength]
  sub si,2
  mov cx,[snakelength]
  sub cx,1
  loop_pos1:
  mov ax,[snakepos+si-2]
  mov [snakepos+si],ax
  sub si,2
  loop loop_pos1
  mov ax,[snakepos+0]
  add ax,2
  mov [snakepos+0],ax
  ret

change_pos_up:                ;swap indexes and changes head
  mov si,[snakelength]
  add si,[snakelength]
  sub si,2
  mov cx,[snakelength]
  sub cx,1
  loop_pos2:
  mov ax,[snakepos+si-2]
  mov [snakepos+si],ax
  sub si,2
  loop loop_pos2
  mov ax,[snakepos+0]
  sub ax,160
  mov [snakepos+0],ax
  ret

change_pos_down:                ;swap indexes and changes head
  mov si,[snakelength]
  add si,[snakelength]
  sub si,2
  mov cx,[snakelength]
  sub cx,1
  loop_pos3:
  mov ax,[snakepos+si-2]
  mov [snakepos+si],ax
  sub si,2
  loop loop_pos3
  mov ax,[snakepos+0]
  add ax,160
  mov [snakepos+0],ax
  ret

move_up:
 push ax
 push cx
 push di
 push si                      
 call change_pos_up               ;changing positions 1 step per call
 call delay1
 call print_snake                 
 call collision                   ;checks for collision with borders
 pop si
 pop di
 pop cx
 pop ax
 ret

move_down:
 push ax
 push cx
 push di
 push si    
 call change_pos_down           ;changing positions 1 step per call
 call delay1
 call print_snake
 call collision                 ;checks for collision with borders
 pop si
 pop di
 pop cx
 pop ax
 ret
 
move_left:
 push ax
 push cx
 push di
 push si
 call change_pos_left             ;changing positions 1 step per call
 call delay1
 call print_snake
 call collision                  ;checks for collision with borders
 pop si
 pop di
 pop cx
 pop ax
 ret

move_right:
 push ax
 push cx
 push di
 push si
 call change_pos_right            ;changing positions 1 step per call
 call delay1
 call print_snake 
 call collision                   ;checks for collision with borders and body
 pop si
 pop di
 pop cx
 pop ax
 ret


collision:
 push ax
 push bx
 push cx
 push si
 mov ax,[snakepos+0]              ;storing head position
 mov cx,[borderlength]            ;storing length of border for loop
 mov si,0

 loop_bordercollision:
  mov bx,[border+si]            
  add si,2
  cmp ax,bx                       ;cmp each border posotion with head
  je terminate_program
  loop loop_bordercollision

  mov cx,[snakelength]            ;storing length of snake for loop
  sub cx,1
  mov si,2                        
 loop_bodycollision:
  mov bx,[snakepos+si]            ;storing position of second part of snake
  add si,2
  cmp ax,bx
  je terminate_program
  loop loop_bodycollision

 pop si
 pop cx
 pop bx
 pop ax
 ret


terminate_program:
 pop si
 pop cx
 pop bx
 pop ax


 call clrscr1

 mov ax,15
 push ax               ; push x position 
 mov ax,5 
 push ax               ; push y position 
 mov ax, 0x0C          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,end1  
 push ax                     ; push address of message 
 mov ax,[endlen]
 push ax
 call printstr              ;  call the printstr subroutine 
 call delay3

 mov ax,15
 push ax               ; push x position 
 mov ax,6 
 push ax               ; push y position 
 mov ax, 0x0C          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,end2  
 push ax                     ; push address of message 
 mov ax,[endlen]
 push ax
 call printstr              ;  call the printstr subroutine 
 call delay3

 mov ax,15
 push ax               ; push x position 
 mov ax,7 
 push ax               ; push y position 
 mov ax, 0x0C          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,end3  
 push ax                     ; push address of message 
 mov ax,[endlen]
 push ax
 call printstr              ;  call the printstr subroutine 
 call delay3

 mov ax,15
 push ax               ; push x position 
 mov ax,8 
 push ax               ; push y position 
 mov ax, 0x0C          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,end4  
 push ax                     ; push address of message 
 mov ax,[endlen]
 push ax
 call printstr              ;  call the printstr subroutine 
 call delay3

 mov ax, 1
 push ax               ; push x position 
 mov ax, 11 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,scoreword  
 push ax                     ; push address of message 
 push word [scorewlen]     ; push message length 
 call printstr1               ;  call the printstr subroutine 
 call print_score1

 loop_sound:
   mov cx,500
   call play_sound1
 loop loop_sound


 mov ax, 0x4c00 ; terminate program 
 int 0x21



check_valid:
 push ax
 push bx
 push cx
 push si
 mov al,80                        ;storing apple position
 mul byte[appleY]
 add ax,[appleX]
 shl ax,1
 mov cx,[borderlength]            ;storing length of border for loop
 mov si,0

 loop_checkborder:
  mov bx,[border+si]            
  add si,2
  cmp ax,bx                       ;cmp each border posotion with head
  je GenNextApple
  loop loop_checkborder

  xor bx,bx
  mov cx,[snakelength]            ;storing length of snake for loop
  mov si,0                        
 loop_checksnake:
  mov bx,[snakepos+si]            ;storing position of second part of snake
  add si,2
  cmp ax,bx
  je GenNextApple
  loop loop_checksnake

  xor bx,bx
  mov bx,[appleY]
  cmp bx,0
  je GenNextApple
  cmp bx,1
  je GenNextApple
  cmp bx,2
  je GenNextApple

 pop si
 pop cx
 pop bx
 pop ax
 ret

;Generate next apple position
GenNextApple:
 push bp
 mov bp,sp
 push ax
 push cx
 push dx

 generate_loop:                       ; Generate random X and Y positions within the board dimensions
     ; Generate X position
	 xor ah,ah                      ; Set function code for random number generation (e.g., using BIOS)
     int 1Ah                         ; Call BIOS function to get current time
     mov ax,dx                        ; Store current time in BX
     xor dx,dx                        ; Clear AX
     mov cx,160                       ; Load board width
     div cx                           ; Divide by board width
     mov word[appleX], dx                 ; Store the X position in appleX

	 ; Generate Y position
     xor ah,ah                      ; Set function code for random number generation (e.g., using BIOS)
     int 0x1A                         ; Call BIOS function to get current time
     mov ax,dx                        ; Store current time in BX
     xor dx,dx                        ; Clear AX  
     mov cx,25              ; Load board height
     div cx                            ; Divide by board height
     mov [appleY], dx                  ; Store the Y position in appleY
	 call check_valid

 pop dx
 pop cx
 pop ax
 pop bp
 ret

;Print apple on screen
print_apple:
 push bp
 mov bp, sp
 push es
 push ax
 push di
 call GenNextApple
 mov ax,0xb800
 mov es,ax
 mov al,80                        ;storing apple position
 mul byte[appleY]
 add ax,[appleX]
 shl ax,1
 mov di,ax
 mov ax,0x4041
 mov [es:di],ax
 pop di
 pop ax
 pop es
 pop bp
 ret

eat_apple:
 push ax
 push bx
 push cx
 push dx
 
 mov al,80                        ;storing apple position
 mul byte[appleY]
 add ax,[appleX]
 shl ax,1
 mov bx,[snakepos+0]
 cmp ax,bx
 jne return_eat
 cmp word[delay1var1],0
 je continue
 sub word[delay1var1],50
 continue:
 call play_sound
 add word[Score],5
 call print_score
 call snake_grow
 call print_apple

 return_eat:
 pop dx
 pop cx
 pop bx
 pop ax
 ret


kbisr:                            ;Takes input(move snake),using interrupts
    push ax
	push es
	mov ax, 0xb800
	mov es, ax
	in al, 0x60 

	left_compare:
		mov ah,1
		cmp [right],ah
		je right_compare

		cmp al, 30
		jne right_compare
		mov al,1
		mov byte [left],al
		mov al,0
		mov byte[right],al
		mov byte[up],al
		mov byte[down],al
		jmp nomatch
	
	right_compare:
		cmp [left],ah
		je up_compare
		
		cmp al, 32 
		jne up_compare
		mov al,1
		mov byte [right],al
		mov al,0
		mov byte[left],al
		mov byte[up],al
		mov byte[down],al
		jmp nomatch
	
	up_compare:
		cmp [down],ah
		je down_compare
		
		cmp al, 17
		jne down_compare
		mov al,1
		mov byte [up],al
		mov al,0
		mov byte[right],al
		mov byte[left],al
		mov byte[down],al
		jmp nomatch
	
	down_compare:
		cmp [up],ah
		je exitgame
		
		cmp al, 31 
		jne exitgame 
		mov al,1
		mov byte [down],al
		mov al,0
		mov byte[right],al
		mov byte[up],al
		mov byte[left],al
		jmp nomatch
	
	exitgame:
		cmp al, 16
		jne nomatch 
		or byte [close],1
		
		
	nomatch: 
	mov al, 0x20
	out 0x20, al           ; send EOI to PIC
	pop es
	pop ax
	iret





;///////////////////////Main Driver Code
start: 

 call clrscr1

 mov ax, 2
 push ax               ; push x position 
 mov ax, 0 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes1  
 push ax                     ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr               ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 1 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes2  
 push ax                     ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr               ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 2 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes3  
 push ax                     ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr               ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 3 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes4  
 push ax                  ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr               ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 4 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes5  
 push ax                     ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax 
 call printstr               ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 5
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes6  
 push ax                  ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr            ;  call the printstr subroutine 
 call delay3

  mov ax, 2
 push ax               ; push x position 
 mov ax, 6 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes7  
 push ax                  ; push address of message 
 mov ax,[Snakeslength]    ; push message length 
 push ax
 call printstr          ;  call the printstr subroutine 
 call delay3

 mov ax, 2
 push ax               ; push x position 
 mov ax, 7 
 push ax               ; push y position 
 mov ax, 0x0C          ; red on black attribute 
 push ax               ; push attribute 
 mov ax,Snakes8  
 push ax                ; push address of message 
 mov ax,[Snakeslength]  ; push message length 
 push ax
 call printstr          ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,10 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg1  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,11
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg2
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,12 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg3  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,13
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg4  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,14 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg5  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,15 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg6  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,16 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg7  
 push ax                     ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1               ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,17 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg8  
 push ax                     ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1               ;  call the printstr subroutine 

 mov ax,0
 push ax               ; push x position 
 mov ax,18 
 push ax               ; push y position 
 mov ax, 0x09          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,msg9  
 push ax               ; push address of message 
 mov ax,[msglen]
 push ax
 call printstr1        ;  call the printstr subroutine 

 mov ax,27
 push ax               ; push x position 
 mov ax,21 
 push ax               ; push y position 
 mov ax, 0x8A          ; blue on black attribute 
 push ax               ; push attribute 
 mov ax,pres  
 push ax               ; push address of message 
 mov ax,[preslen]      ; push message length 
 push ax
 call printstr1        ;  call the printstr subroutine

 mov ah,0
 int 0x16



 call clrscr2               ; call clrscr subroutine 
 
 ;printing middle obstacle
 mov ax, 40
 push ax                    ;x position 
 mov ax, 10
 push ax                    ;y position 
 mov ax, 0x20               ; attribute 
 push ax               
 mov ax, 0x7C
 push ax               
 mov ax, 8
 push ax                    ; push length 
 call printver  

 ;printing middle border
 mov ax, 0
 push ax                    ;x position 
 mov ax, 2
 push ax                    ; y position 
 mov ax, 0x20               ; attribute 
 push ax                      
 mov ax, 0x5F               ;space
 push ax                 
 mov ax, 80
 push ax                    ;  length 
 call printhor   

 ;printing lower border
 mov ax, 0
 push ax                    ;x position 
 mov ax, 24 
 push ax                    ; y position 
 mov ax, 0x20               ; attribute 
 push ax                      
 mov ax, 0x5F               ;dash
 push ax                 
 mov ax, 80
 push ax                    ;  length 
 call printhor 

 ;printing right border1
 mov ax, 79
 push ax                    ;x position 
 mov ax, 0
 push ax                    ;y position 
 mov ax, 0x20               ; attribute 
 push ax               
 mov ax, 0x7C
 push ax               
 mov ax, 11
 push ax                    ; push length 
 call printver    

 ;printing right border2
 mov ax, 79
 push ax                    ;x position 
 mov ax, 17
 push ax                    ;y position 
 mov ax, 0x20               ; attribute 
 push ax               
 mov ax, 0x7C
 push ax               
 mov ax, 8
 push ax                    ; push length 
 call printver    
 
; printing left border1
 mov ax, 0
 push ax                      ;x position 
 mov ax, 0
 push ax                      ;y position 
 mov ax, 0x20                 ; attribute 
 push ax               
 mov ax, 0x7C
 push ax               
 mov ax, 11
 push ax                      ; push length 
 call printver

 ; printing left border2
 mov ax, 0
 push ax                      ;x position 
 mov ax, 17
 push ax                      ;y position 
 mov ax, 0x20                 ; attribute 
 push ax               
 mov ax, 0x7C
 push ax               
 mov ax, 8
 push ax                      ; push length 
 call printver
 
 ;printing upper border
 mov ax, 0
 push ax                      ;x position 
 mov ax, 0
 push ax                      ; y position 
 mov ax, 0x20                 ; brown attribute 
 push ax                      
 mov ax, 0x5F                 ;dash
 push ax                 
 mov ax, 80
 push ax                      ;length 
 call printhor 




;enabling WASD keys to move snake
xor ax, ax
mov es, ax                         ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr], ax                   ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr+2], ax                 ; save segment of old routine
cli                                ; disable interrupts
mov word [es:9*4], kbisr           ; store offset at n*4
mov [es:9*4+2], cs                 ; store segment at n*4+2
sti                                ; enable interrupts
call print_snake
call print_apple

 mov ax, 1
 push ax                           ; push x position 
 mov ax, 1 
 push ax                           ; push y position 
 mov ax, 0x09                      ; blue on black attribute 
 push ax                           ; push attribute 
 mov ax,scoreword  
 push ax                           ; push address of message 
 push word [scorewlen]             ; push message length 
 call printstr                     ;  call the printstr subroutine 

 call print_score

;main loop to move snake
move_main:

	cmp byte [up],1
	jne skip_up
	    call eat_apple
		call move_up
		
	skip_up:
	cmp byte [down],1
	jne skip_down
	    call eat_apple
	    call move_down
	
	skip_down:
	cmp byte [left],1
	jne skip_left
	    call eat_apple
		call move_left
		
	skip_left:
	cmp byte [right],1
	jne skip_right
	    call eat_apple
		call move_right
		
	skip_right:
	cmp byte [close],0
	je move_main


 xor ax,ax
 mov es,ax
	
 mov ax, [oldisr]                 ; read old offset in ax
 mov bx, [oldisr+2]               ; read old segment in bx
 cli                              ; disable interrupts 
 mov [es:9*4], ax                 ; restore old offset from ax
 mov [es:9*4+2], bx               ; restore old segment from bx
 sti                              ; enable interrupts









 ;/////////////////////Data for game
tailpos: dw 2314                    
head: dw 0xE44F
body: dw 0xD020
appleX: dw 21
appleY: dw 18
snakelength: dw 5
snakeposs: dw 2304,2306,2308,2310,2312
;This array is for snake growing
snakepos: dw 2304,2306,2308,2310,2312,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
border : dw 1680,1840,2000,2160,2320,2480,2640,2800,640,800,960,1120,1280,1440,1600,2720,2880,3040,3200,3360,3520,3680,3840,320,322,324,326,328,330,332,334,336,338,340,342,344,346,348,350,352,354,356,358,360,362,364,366,368,370,372,374,376,378,380,382,384,386,388,390,392,394,396,398,400,402,404,406,408,410,412,414,416,418,420,422,424,426,428,430,432,434,436,438,440,442,444,446,448,450,452,454,456,458,460,462,464,466,468,470,472,474,476,478,798,958,1118,1278,1438,1598,1758,2878,3038,3198,3358,3518,3678,3838,3998,3842,3844,3846,3848,3850,3852,3854,3856,3858,3860,3862,3864,3866,3868,3870,3872,3874,3876,3878,3880,3882,3884,3886,3888,3890,3892,3894,3896,3898,3900,3902,3904,3906,3908,3910,3912,3914,3916,3918,3920,3922,3924,3926,3928,3930,3932,3934,3936,3938,3940,3942,3944,3946,3948,3950,3952,3954,3956,3958,3960,3962,3964,3966,3968,3970,3972,3974,3976,3978,3980,3982,3984,3986,3988,3990,3992,3994,3996
borderlength : dw 196
close: db 0
left: db 0
right: db 0
up: db 0
down: db 0
oldisr: dd 0
scorewlen: dw 78
scoreword: db '                                   Score :                                    '
Score: dw 0
delay1var1: dw 450
delay1var2: dw 600


;\\\\\\\\\\\\\\\\Front page
Snakes1: db ' _______ _       _______ _       _______    _______ _______ _______ _______ ' 
Snakes2: db '(  ____ ( (    /(  ___  ) \    /(  ____ \  (  ____ (  ___  |       |  ____ \'
Snakes3: db '| (    \/  \  ( | (   ) |  \  / / (    \/  | (    \/ (   ) | () () | (    \/'
Snakes4: db '| (_____|   \ | | (___) |  (_/ /| (__      | |     | (___) | || || | (__    '
Snakes5: db '(_____  ) (\ \) |  ___  |   _ ( |  __)     | | ____|  ___  | |(_)| |  __)   '
Snakes6: db '      ) | | \   | (   ) |  ( \ \| (        | | \_  ) (   ) | |   | | (      '
Snakes7: db '/\____) | )  \  | )   ( |  /  \ \ (____/\  | (___) | )   ( | )   ( | (____/\'
Snakes8: db '\_______)/    )_)/     \|_/    \(_______/  (_______)/     \|/     \(_______/'
Snakeslength: dw 76

msg1: db '-> For moving upward    press  W'
msg2: db '-> For moving left      press  A'
msg3: db '-> For moving downward  press  S'
msg4: db '-> For moving right     press  D'
msg5: db '-> For exit game        press  Q'
msg6: db '-> After digesting an Apple:    '
msg7: db '   1_ Snake will grow (larger)  '
msg8: db '   2_ Its speed will increase   '
msg9: db '   3_ Score increases by five   '
msglen: dw 32
pres: db 'Press ENTER to continue'
preslen: dw 23


;\\\\\\\\\\\\\\\\\\\\\Back page
end1: db'  ___    __    __  __  ____    ____  _  _  ____  /\'  
end2: db' / __)  /__\  (  \/  )( ___)  ( ___)( \( )(  _ \ )('  
end3: db'( (_-. /(__)\  )    (  )__)    )__)  )  (  )(_) )\/'  
end4: db' \___/(__)(__)(_/\/\_)(____)  (____)(_)\_)(____/ ()'  
endlen: dw 51

